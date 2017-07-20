{-# LANGUAGE OverloadedStrings #-}
module Handlers.Actions.Common(
  api
  ,view
  ,withParams
  ,withAuthorization
)where

import Control.Monad.Trans(lift)
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Reader(asks)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Data.Aeson ((.=), object, FromJSON, ToJSON)


import qualified Web.Scotty.Trans as Web
import qualified Web.Scotty.Cookie as Cookie
import Network.HTTP.Types.Status

import App.Types
import App.Context

import qualified Utils.Scotty.Auth  as Auth

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

withParams :: (FormParams request) => (Processor request response) -> Response (Status,response)
withParams with = do
  paramAssoc <- M.fromList <$> Web.params
  let ps = M.mapKeys LT.toStrict $ LT.toStrict <$> paramAssoc
  case fromParams ps of
    Nothing -> Web.raise $ Exception badRequest400 "Expected request in params"
    Just req -> with req

withAuthorization :: Authorized T.Text request response -> Processor request response
withAuthorization with req = do
    auth <- Web.header "Authorization"
    cookie <- Cookie.getCookie "Authorization"
    secret <- lift (asks secret)
    case auth of
      Nothing -> cookieAuth secret cookie
      _ -> headerAuth secret auth
  where
    authAction info = do
      case info of
        Nothing -> Web.raise $ Exception unauthorized401 "Authorization required"
        Just userID -> with userID req
    cookieAuth secret cookie = do
      info <- liftIO $ Auth.cookieSecure secret cookie
      authAction info
    headerAuth secret auth = do
      info <-  liftIO $ Auth.headerSecure secret auth
      authAction info
