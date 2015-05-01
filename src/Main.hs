{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.ReverseProxy as RP
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, void)
import Data.Conduit
import Data.Conduit.List (mapAccum)
import Data.FileEmbed (embedFile)
import Data.String (fromString)
import Data.Unique (Unique, newUnique)
import Network.HTTP.Client (Manager, Response, withManager, defaultManagerSettings)
import Network.HTTP.Types (status200)
import Options.Applicative

import HttpUtils (isHtmlResponse)
import ByteStringUtils (byteStringToLower)

defaultHost :: String
defaultHost = "0.0.0.0"

defaultPort :: Int
defaultPort = 8808

defaultRemotePort :: Int
defaultRemotePort = 80

data Options = Options
    { host :: String
    , port :: Int
    , remoteHost :: String
    , remotePort :: Int
    }

parseOptions :: Parser Options
parseOptions = Options
    <$> strOption (long "host" <> metavar "HOST" <> value defaultHost <> help "host to listen on")
    <*> option auto (long "port" <> metavar "PORT" <> value defaultPort <> help "port to listen on")
    <*> strOption (long "remote-host" <> metavar "REMOTEHOST" <> help "host of destination server that should be proxied to")
    <*> option auto (long "remote-port" <> metavar "REMOTEPORT" <> value defaultRemotePort <> help "port of destination server that should be proxied to")

type Client = (Unique, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcastReload :: ServerState -> IO ()
broadcastReload clients = do
    forM_ clients $ \(_, conn) -> WS.sendTextData conn ("reload" :: T.Text)

main :: IO ()
main = do
    execParser opts >>= runWithOptions

    where
    opts = info (helper <*> parseOptions)
        (fullDesc <> header "serverer - Development web server with live reload")

runWithOptions :: Options -> IO ()
runWithOptions opts = do
    putStrLn $ "Listening on " ++ host opts ++ ":" ++ show (port opts) ++ "..."

    let settings = (Warp.setHost (fromString (host opts)))
            . (Warp.setPort (port opts)) $ Warp.defaultSettings

    state <- newMVar newServerState

    withManager defaultManagerSettings $ \manager ->
        Warp.runSettings settings $
            WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp state) (reverseProxy (remoteHost opts) (remotePort opts) state manager)

reverseProxy :: String -> Int -> MVar ServerState -> Manager -> Network.Wai.Application
reverseProxy proxyHost proxyPort state manager = RP.waiProxyToSettings getDest settings manager
    where
    getDest :: Network.Wai.Request -> IO RP.WaiProxyResponse
    getDest req
        | Network.Wai.pathInfo req == ["__reload"] = return $ RP.WPRApplication (reloadHandler state)
        | otherwise = return $ RP.WPRProxyDest (RP.ProxyDest (fromString proxyHost) proxyPort)
    settings :: RP.WaiProxySettings
    settings = RP.def { RP.wpsProcessBody = processBody }
    processBody :: Response () -> Maybe (Conduit BS.ByteString IO (Flush Builder))
    processBody response
        | isHtmlResponse response = Just injectScript
        | otherwise = Nothing

reloadHandler :: MVar ServerState -> Network.Wai.Application
reloadHandler state _ respond = do
    readMVar state >>= broadcastReload
    respond $ Network.Wai.responseLBS status200 [] "Triggered Reload"

script :: BS.ByteString
script = $(embedFile "src/script.js")

stripNewlines :: BS.ByteString -> BS.ByteString
stripNewlines source =
    BS.foldl go BS.empty source
    where
    go built char
        | char == fromIntegral (fromEnum '\n') = built
        | otherwise = built `BS.snoc` char

injection :: BS.ByteString
injection = BS.concat
    [ "<!-- BEGIN INJECTION by serverer --><script type=\"text/javascript\">"
    , stripNewlines script
    , "</script><!-- END INJECTION -->"
    ]

injectScript :: Conduit BS.ByteString IO (Flush Builder)
injectScript =
    void $ mapAccum doChunk (Just headTag)
    where
    headTag = "<head>"
    -- TODO `doChunk` has a bug where it won't handle the case where `headTag`
    -- happens to be split between two input chunks. (This should be pretty
    -- rare in practice since the tag usually appears very near the beginning
    -- of the document and chunks are supposed to be pretty large)
    doChunk :: BS.ByteString -> Maybe BS.ByteString -> (Maybe BS.ByteString, Flush Builder)
    doChunk chunk Nothing = (Nothing, Chunk (fromByteString chunk))
    doChunk chunk (Just _) =
        let chunkLower = byteStringToLower chunk
        in case BS.breakSubstring headTag chunkLower of
            (x, y)  | BS.null y -> (Just headTag, Chunk (fromByteString chunk))
                    | otherwise ->
                        let index = BS.length x + BS.length headTag
                            start = BS.take index chunk
                            end = BS.drop index chunk
                        in (Nothing, Chunk $ mconcat
                            [ fromByteString start
                            , fromByteString injection
                            , fromByteString end ])

wsApp :: MVar ServerState -> WS.ServerApp
wsApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    newId <- newUnique
    let client = (newId, conn)
        disconnect = do
            modifyMVar_ state $ \s -> do
                let s' = removeClient client s
                return s'
    flip finally disconnect $ do
        modifyMVar_ state $ \s -> do
            let s' = addClient client s
            return s'
        clientWait conn

clientWait :: WS.Connection -> IO ()
clientWait conn = do
    _ <- (WS.receiveData conn) :: IO BS.ByteString
    clientWait conn
