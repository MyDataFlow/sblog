{-# LANGUAGE OverloadedStrings #-}
module Handlers.Actions.Common(
  api
  ,view
  ,withParams
)where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT 
import qualified Data.Map as M

import Data.Aeson ((.=), object, FromJSON, ToJSON)

import Web.Scotty.Trans as Web
import Network.HTTP.Types.Status

import App.Types
import App.Context

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
  Web.html resp
  Web.status stat
  return resp

withParams :: (FromParams request) => (Processor request response) -> (Render response) -> Response  response 
withParams with render = do 
  paramAssoc <- M.fromList <$> params
  let ps = M.mapKeys LT.toStrict $ LT.toStrict <$> paramAssoc
  case fromParams ps of
    Nothing -> raise $ Exception "Expected request in params"
    Just req -> render $ with req