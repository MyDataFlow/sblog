{-# LANGUAGE OverloadedStrings #-}
module Handlers.Actions.Common(
  api
  ,view
  ,withJSON
  ,withParams
  ,withAuthorization
  ,withGeneratedCSRF
  ,withCSRFVerified
)where

import Control.Monad.Trans(lift)
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Reader(asks)
import Control.Monad.State (get,put)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.Maybe
import Data.Aeson ((.=), object, FromJSON, ToJSON)


import qualified Web.Scotty.Trans as Web
import Network.HTTP.Types.Status

import App.Types
import App.Context

import qualified Utils.Scotty.Auth  as Auth
import qualified Utils.Scotty.CSRF as CSRF
import qualified Utils.Scotty.Cookie as Cookie
import Handlers.Actions.Types

api :: (ToJSON response) => Response (Status, response) -> Response response
api with = do
  (stat, resp) <- with
  Web.json resp
  Web.status stat
  return resp

view :: Response (Status,LT.Text) -> Response LT.Text
view with = do
  (stat, resp) <- with
  if stat == status302 || stat == status301
    then Web.redirect resp
    else do
      Web.html resp
      Web.status stat
  return resp

withJSON :: (FromJSON request, ToJSON response) => (Processor request response) -> Response (Status,response)
withJSON with = do
  req <- Web.jsonData
  with req

withParams :: (FormParams request) => (Processor request response) -> Response (Status,response)
withParams with = do
  paramAssoc <- M.fromList <$> Web.params
  let ps = M.mapKeys LT.toStrict $ LT.toStrict <$> paramAssoc
  case fromParams ps of
    Nothing -> Web.raise $ Exception status400 "Expected request in params"
    Just req -> with req

withGeneratedCSRF :: Processor request response -> Processor request response
withGeneratedCSRF with req = do
    sessionCSRF <- Cookie.getCookie "_csrf_token"
    st <- lift $ get
    case sessionCSRF of
      Just token ->  lift $ put st {csrfToken = token}
      Nothing -> makeCSRFToken
    with req
  where
    makeCSRFToken = do
      secret <- lift (asks csrfKey)
      csrf <- liftIO $ CSRF.generateCSRF secret
      st <- lift $ get
      Cookie.setCookie $ Cookie.makeRootSimpleCookie  "_csrf_token" csrf
      lift $ put st {csrfToken = csrf}
withCSRFVerified :: Processor request response -> Processor request response
withCSRFVerified with req = do
  sessionCSRF <- Cookie.getCookie "_csrf_token"
  token <- Web.rescue (Web.param "_csrf_token")
    (\e -> Web.header "x-csrf-token"  >>= \t -> return $ fromMaybe "" t)
  if (token == "") && (isNothing sessionCSRF )
    then with req
    else case sessionCSRF of
      Nothing ->  Web.raise $ Exception status500 "Expected request in CSRF"
      Just csrf ->
        if csrf == ( LT.toStrict token)
        then with req
        else Web.raise $ Exception status500 "Expected request in CSRF"


--  Authorized auth request response = auth -> Processor request response
-- Processor request response = request -> Response (Status, response)
withAuthorization :: Authorized T.Text request response -> Processor request response
withAuthorization with req = do
    auth <- Web.header "Authorization"
    cookie <- Cookie.getCookie "Authorization"
    secret <- lift (asks jwtKey)
    case auth of
      Nothing -> cookieAuth secret cookie
      _ -> headerAuth secret auth
  where
    authAction info = do
      case info of
        Nothing -> Web.raise $ Exception status401 "Authorization required"
        Just payload -> with payload req
    cookieAuth secret cookie = do
      info <- liftIO $ Auth.cookieSecure secret cookie
      authAction info
    headerAuth secret auth = do
      info <-  liftIO $ Auth.headerSecure secret auth
      authAction info
