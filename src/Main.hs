{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import qualified Data.Text as T
import qualified Network.HTTP.ReverseProxy as RP
import qualified Network.Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import Blaze.ByteString.Builder (Builder)
import Data.ByteString (ByteString)
import Data.Conduit (Conduit, Flush)
import Data.FileEmbed (embedDir)
import Network.HTTP.Client (Manager, Response, withManager, defaultManagerSettings)

import HttpUtils (isHtmlResponse)

import Debug.Trace

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
    processBody :: Response () -> Maybe (Conduit ByteString IO (Flush Builder))
    processBody response
        | isHtmlResponse response = trace (show response) (error "TODO")
        | otherwise = Nothing

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

wsApp :: WS.ServerApp
wsApp pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    WS.sendTextData conn ("wassup there!" :: T.Text)
