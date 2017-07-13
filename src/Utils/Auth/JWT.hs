{-# LANGUAGE OverloadedStrings #-}
module Utils.Auth.JWT (
  issued
  ,verify
) where

import Data.Text as T
import Data.Maybe

import Data.Time as DT
import Data.Time.Clock.POSIX as PClock
import qualified Web.JWT as JWT

timestamp :: IO Int
timestamp = round <$> PClock.getPOSIXTime

expiredTime :: Int
expiredTime = 57600

ifExpired :: Maybe (JWT.JWT JWT.VerifiedJWT) -> Int-> Bool
ifExpired vj ts =
  fromJust $ vj >>= return . JWT.claims >>= \i->
    if maybe (0) (round . toRational . JWT.secondsSinceEpoch ) (JWT.exp i)  <= ts
      then return True
      else return False

createClaims :: String -> Int -> JWT.JWTClaimsSet
createClaims payload ts =
  JWT.def {
    JWT.exp = JWT.numericDate $ fromInteger $ toInteger (ts + expiredTime)
    ,JWT.iat = JWT.numericDate $ fromInteger $ toInteger ts
    ,JWT.jti = JWT.stringOrURI (T.pack payload)
  }

issued :: String ->String -> IO JWT.JSON
issued key payload = do
  ts <- timestamp
  return $ JWT.encodeSigned JWT.HS256 (JWT.secret $ T.pack key) $ createClaims payload ts

verify :: String -> String -> IO (Maybe T.Text)
verify key claims  = do
  ts <- timestamp
  let
    jwt =  JWT.decodeAndVerifySignature (JWT.secret $ T.pack key) $ T.pack claims
  case jwt of
    Nothing -> return Nothing
    _ ->
      if ifExpired jwt ts
        then return Nothing
        else return $ (jwt >>=  return . JWT.claims >>= JWT.jti >>= return . JWT.stringOrURIToText )
