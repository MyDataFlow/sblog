{-# LANGUAGE OverloadedStrings #-}
module Utils.Auth.Password (
  hash
  ,validate
) where

import Data.Maybe

import Crypto.BCrypt as BC
import Data.ByteString.Char8 as BSC

defaultPolicy ::  HashingPolicy
defaultPolicy = 
  HashingPolicy 12 (BSC.pack "$2a$")

hash :: String -> IO (Maybe String)
hash pass = do
  cr <- BC.hashPasswordUsingPolicy defaultPolicy (BSC.pack pass)
  case cr of
    Just cpass -> return $ Just (BSC.unpack cpass)
    Nothing -> return $ Nothing

validate :: String -> String -> Bool
validate pass cpass =
  BC.validatePassword (BSC.pack cpass) (BSC.pack pass) 
