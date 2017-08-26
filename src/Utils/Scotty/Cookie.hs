{-# LANGUAGE OverloadedStrings #-}
module Utils.Scotty.Cookie( 
  makeSimpleCookie
  ,makeRootSimpleCookie
  ,setCookie
  ,setSimpleCookie
  ,getCookie
  ,getCookies
  ,deleteCookie
) where

import Control.Monad ( liftM )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as BSL

import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import Blaze.ByteString.Builder ( toLazyByteString )

import Web.Scotty.Trans
import Web.Cookie


makeRootSimpleCookie :: T.Text -> T.Text -> SetCookie
makeRootSimpleCookie n v = 
  def { setCookieName  = T.encodeUtf8 n
      , setCookieValue = T.encodeUtf8 v
      , setCookiePath = Just $ T.encodeUtf8 "/"
      }
makeSimpleCookie :: T.Text -> T.Text -> SetCookie
makeSimpleCookie n v = 
  def { setCookieName  = T.encodeUtf8 n
      , setCookieValue = T.encodeUtf8 v
      }

setCookie :: (Monad m, ScottyError e) => SetCookie -> ActionT e m ()
setCookie c = addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie c)

setSimpleCookie :: (Monad m, ScottyError e)=> T.Text -> T.Text -> ActionT e m ()
setSimpleCookie n v = setCookie $ makeSimpleCookie n v


getCookie :: (Monad m, ScottyError e) => T.Text -> ActionT e m (Maybe T.Text)
getCookie c = liftM (Map.lookup c) getCookies

getCookies :: (Monad m, ScottyError e) => ActionT e m (Map.Map T.Text T.Text)
getCookies = 
    liftM (Map.fromList . maybe [] parse) $ header "Cookie"
  where 
    parse = parseCookiesText . BSL.toStrict . TL.encodeUtf8


deleteCookie :: (Monad m, ScottyError e)=> T.Text -> ActionT e m ()
deleteCookie c = setCookie $ (makeSimpleCookie c "") { setCookieExpires = Just $ posixSecondsToUTCTime 0 }