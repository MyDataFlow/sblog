{-# LANGUAGE OverloadedStrings #-}
module Handlers.Actions.Types where

import Data.Text(Text)
import qualified Data.Map as M

import Network.HTTP.Types.Status

import App.Types
import App.Context

class FromParams a where
  fromParams :: M.Map Text Text -> Maybe a

type Processor request response = request -> Response (Status, response)
type Render response = Response (Status,response) ->  Response response
type Authorized auth request response = auth -> Processor request response