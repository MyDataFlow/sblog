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


import Web.Scotty.Trans as Web
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

withParams :: (FromParams request) => (Processor request response) -> (Render response) -> Response  response
withParams with render = do
  paramAssoc <- M.fromList <$> params
  let ps = M.mapKeys LT.toStrict $ LT.toStrict <$> paramAssoc
  case fromParams ps of
    Nothing -> raise $ Exception badRequest400 "Expected request in params"
    Just req -> render $ with req

withAuthorization :: Authorized T.Text request response -> Processor request response
withAuthorization with req = do
  auth <- Web.header "Authorization"
  secret <- lift (asks secret)
  info <-  liftIO $ Auth.secure secret auth
  case info of
    Nothing -> raise $ Exception unauthorized401 "Authorization required"
    Just userID -> with userID req
