{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import qualified Data.Text as T
import Data.FileEmbed (embedDir)

main :: IO ()
main = do
    putStrLn "Starting..."
    Warp.runSettings Warp.defaultSettings
        { Warp.settingsPort = 8000
        } $ WaiWS.websocketsOr WS.defaultConnectionOptions application staticApp

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    WS.sendTextData conn ("wassup there!" :: T.Text)
