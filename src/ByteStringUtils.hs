module ByteStringUtils
    ( byteStringToLower
    ) where

import Data.Word (Word8)
import qualified Data.ByteString as BS

byteStringToLower :: BS.ByteString -> BS.ByteString
byteStringToLower = BS.map toLower

toLower :: Word8 -> Word8
toLower c
    | c >= fromIntegral (fromEnum 'A') && c <= fromIntegral (fromEnum 'Z') =
        c + (fromIntegral (fromEnum 'a')) - (fromIntegral (fromEnum 'A'))
    | otherwise = c
