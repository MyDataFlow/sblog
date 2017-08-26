{-# LANGUAGE OverloadedStrings #-}
module Utils.Scotty.Auth(
  JWTConf(..)
  ,headerSecure
  ,cookieSecure
  ,generateCookie
) where

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Maybe
import Data.Char
import Data.Default 

import Utils.Auth.JWT 


instance Default JWTConf where
  def = JWTConf 57600 "" ""

generateCookie :: JWTConf -> IO T.Text
generateCookie conf = do
  token <- issued conf
  return $ token  

cookieSecure :: String -> Maybe T.Text -> IO (Maybe T.Text)
cookieSecure secret auth = do
  case auth of
    Nothing -> return $ Nothing
    Just claims -> verify (def {jwtSecret = T.pack secret}) $  claims

headerSecure :: String -> Maybe LT.Text -> IO (Maybe T.Text)
headerSecure secret auth = do
  case auth of
    Nothing -> return $ Nothing
    Just token ->
      case C8.break isSpace $ C8.pack $ LT.unpack token of
        (strategy,claims)
          | C8.map toLower strategy == "bearer" -> verify (def {jwtSecret = T.pack secret}) $ T.pack $ C8.unpack claims
          | otherwise -> return $ Nothing

