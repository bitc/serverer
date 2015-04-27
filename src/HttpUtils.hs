{-# LANGUAGE OverloadedStrings #-}

module HttpUtils
    ( isHtmlResponse
    ) where

import qualified Data.ByteString as BS
import Network.HTTP.Client (Response, responseHeaders)
import Network.HTTP.Types (ResponseHeaders, HeaderName, hContentType)
import qualified Data.CaseInsensitive as CI

htmlMimeTypes :: [CI.CI BS.ByteString]
htmlMimeTypes = ["text/html", "application/xhtml+xml"]

isHtmlResponse :: Response () -> Bool
isHtmlResponse response =
    let contentType = getHeader hContentType (responseHeaders response)
    in case contentType of
        Nothing -> False
        Just val -> let mimeType = CI.mk (getMimeType val)
                    in elem mimeType htmlMimeTypes

getHeader :: HeaderName -> ResponseHeaders -> Maybe BS.ByteString
getHeader = lookup

-- Takes a value such as "text/html; charset=utf-8" and returns "text/html"
getMimeType :: BS.ByteString -> BS.ByteString
getMimeType val = fst (BS.break (== (fromIntegral (fromEnum ';'))) val)
