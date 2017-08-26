{-# LANGUAGE OverloadedStrings #-}
module Utils.Scotty.CSRF(
    generateCSRF
) where

import  Codec.Crypto.SimpleAES

import Data.ByteString
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import  Data.ByteString.Base64.URL
import  Data.String.Conversions

generateCSRF :: String -> IO T.Text
generateCSRF key = do
  secret <- randomKey
  c <- encryptMsg ECB ( C8.pack key) (cs secret)
  return $ T.pack $ C8.unpack $ encode . cs $ c