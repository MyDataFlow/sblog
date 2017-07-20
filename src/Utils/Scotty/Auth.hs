{-# LANGUAGE OverloadedStrings #-}
module Utils.Scotty.Auth(
  headerSecure
  ,cookieSecure
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Maybe
import Data.Char

import qualified Utils.Auth.JWT as JWT

cookieSecure :: String -> Maybe T.Text -> IO (Maybe T.Text)
cookieSecure secret auth = do
  case auth of
    Nothing -> return $ Nothing
    Just claims -> verify secret $  T.unpack claims

headerSecure :: String -> Maybe LT.Text -> IO (Maybe T.Text)
headerSecure secret auth = do
  case auth of
    Nothing -> return $ Nothing
    Just token ->
      case C8.break isSpace $ C8.pack $ LT.unpack token of
        (strategy,claims)
          | C8.map toLower strategy == "bearer" -> verify secret $ C8.unpack claims
          | otherwise -> return $ Nothing

verify :: String -> String -> IO (Maybe T.Text)
verify secret claims = do
  info <- JWT.verify secret claims
  case info of
    Nothing -> return $ Nothing
    _ -> return $ info
