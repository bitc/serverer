{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.ReverseProxy as RP
import qualified Network.Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString
import Control.Monad (void)
import Data.Conduit
import Data.Conduit.List
import Data.FileEmbed (embedDir)
import Data.FileEmbed (embedFile)
import Network.HTTP.Client (Manager, Response, withManager, defaultManagerSettings)

import HttpUtils (isHtmlResponse)
import ByteStringUtils (byteStringToLower)

main :: IO ()
main = do
    putStrLn "Starting..."

    let settings = (Warp.setHost "0.0.0.0")
            . (Warp.setPort 8000) $ Warp.defaultSettings

    withManager defaultManagerSettings $ \manager ->
        Warp.runSettings settings $
            WaiWS.websocketsOr WS.defaultConnectionOptions wsApp (reverseProxy manager)

proxyDest :: RP.ProxyDest
proxyDest = RP.ProxyDest "localhost" 8800

reverseProxy :: Manager -> Network.Wai.Application
reverseProxy manager = RP.waiProxyToSettings getDest settings manager
    where
    getDest :: Network.Wai.Request -> IO RP.WaiProxyResponse
    getDest _ = return $ RP.WPRProxyDest proxyDest
    settings :: RP.WaiProxySettings
    settings = RP.def { RP.wpsProcessBody = processBody }
    processBody :: Response () -> Maybe (Conduit BS.ByteString IO (Flush Builder))
    processBody response
        | isHtmlResponse response = Just injectScript
        | otherwise = Nothing

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

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

wsApp :: WS.ServerApp
wsApp pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    WS.sendTextData conn ("wassup there!" :: T.Text)
