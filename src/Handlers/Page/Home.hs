{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Handlers.Page.Home(
  indexR
)where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as C8
import Data.Either
import Data.Maybe

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)

import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status
import Network.OAuth.OAuth2
import URI.ByteString
import qualified Network.URI as URI

import App.Types

import Utils.URI.String
import Utils.URI.Params

import Handlers.Actions.Common


import Models.Schemas
import qualified Models.DB as DB
import Views.Common.Render

indexProcessor user req = do
  setTpl "page/home.html"
  p <- render
  return (status200,p)

indexR :: Response ()
indexR = do
  view $ withAuthorization (withUser indexProcessor)  ()
