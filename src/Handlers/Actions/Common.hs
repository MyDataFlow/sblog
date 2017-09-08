{-# LANGUAGE OverloadedStrings #-}
module Handlers.Actions.Types where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.Maybe

import Control.Monad.Trans(lift)
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Reader(asks)
import Control.Monad.State (get,put,modify)

import Data.Aeson ((.=), object, FromJSON, ToJSON)

import Network.HTTP.Types.Status
import qualified Web.Scotty.Trans as Web

import App.Types

import qualified Utils.Scotty.Auth  as Auth
import qualified Utils.Scotty.CSRF as CSRF
import qualified Utils.Scotty.Cookie as Cookie

class FromParams a where
  fromParams :: M.Map T.Text T.Text -> Maybe a

data ParamsMap = ParamsMap (M.Map T.Text T.Text)

instance FromParams ParamsMap where
  fromParams m = Just $ ParamsMap m

type Processor request response = request -> Response (Status, response)
type Render response = Response (Status,response) ->  Response response
type Authorized auth request response = auth -> Processor request response


api :: (ToJSON response) => Response (Status, response) -> Response ()
api with = do
  (stat, resp) <- with
  Web.status stat
  Web.json resp

view :: Response (Status,LT.Text) -> Response ()
view with = do
  (stat, resp) <- with
  Web.status stat
  if stat == status302 || stat == status301
    then Web.redirect resp
    else Web.html resp

withJSON :: (FromJSON request, ToJSON response) => (Processor request response) -> Response (Status,response)
withJSON with = do
  req <- Web.jsonData
  with req

withParams :: (FromParams request) => (Processor request response) -> Response (Status,response)
withParams with = do
  paramAssoc <- M.fromList <$> Web.params
  let ps = M.mapKeys LT.toStrict $ LT.toStrict <$> paramAssoc
  case fromParams ps of
    Nothing -> Web.raise $ Exception status400 "Expected request in params"
    Just req -> with req

withGeneratedCSRF :: Processor request response -> Processor request response
withGeneratedCSRF with req = do
    sessionCSRF <- Cookie.getCookie "_csrf_token"
    case sessionCSRF of
      Just token ->  do
        setTplValue "_csrf_token" token
        lift $ modify $ \s -> s{csrfToken = token}
      Nothing -> makeCSRFToken
    with req
  where
    makeCSRFToken = do
      s <- lift (asks site)
      csrf <- liftIO $ CSRF.generateCSRF $ csrfSecret s
      setTplValue "_csrf_token" csrf
      Cookie.setCookie $ Cookie.makeRootSimpleCookie  "_csrf_token" csrf
      lift $ modify $ \s -> s {csrfToken = csrf}

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
    s <- lift (asks site)
    case auth of
      Nothing -> cookieAuth (jwtSecret s) cookie
      _ -> headerAuth (jwtSecret s) auth
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
