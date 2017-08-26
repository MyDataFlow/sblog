{-# LANGUAGE OverloadedStrings #-}
module Utils.Auth.JWT (
  JWTConf(..)
  ,issued
  ,verify
) where

import Data.Text as T
import Data.Maybe

import Data.Time as DT
import Data.Time.Clock.POSIX as PClock
import qualified Web.JWT as JWT

data JWTConf = JWTConf {
  jwtExpired :: Int
  ,jwtSecret :: T.Text
  ,jwtPayload :: T.Text
}

timestamp :: IO Int
timestamp = round <$> PClock.getPOSIXTime


ifExpired :: Maybe (JWT.JWT JWT.VerifiedJWT) -> Int-> Bool
ifExpired vj ts =
  fromJust $ vj >>= return . JWT.claims >>= \i->
    if maybe (0) (round . toRational . JWT.secondsSinceEpoch ) (JWT.exp i)  <= ts
      then return True
      else return False

createClaims :: T.Text -> Int -> Int -> JWT.JWTClaimsSet
createClaims payload ts et =
  JWT.def {
    JWT.exp = JWT.numericDate $ fromInteger $ toInteger et
    ,JWT.iat = JWT.numericDate $ fromInteger $ toInteger ts
    ,JWT.jti = JWT.stringOrURI payload
  }

issued :: JWTConf -> IO JWT.JSON
issued conf = do
  ts <- timestamp
  let et = ts + (jwtExpired conf)
  return $ JWT.encodeSigned JWT.HS256 (JWT.secret $ jwtSecret conf) $ createClaims (jwtPayload conf) ts et

verify :: JWTConf -> T.Text -> IO (Maybe T.Text)
verify conf claims  = do
  ts <- timestamp
  let
    jwt =  JWT.decodeAndVerifySignature (JWT.secret $ jwtSecret conf) claims
  case jwt of
    Nothing -> return Nothing
    _ ->
      if ifExpired jwt ts
        then return Nothing
        else return $ (jwt >>=  return . JWT.claims >>= JWT.jti >>= return . JWT.stringOrURIToText )
