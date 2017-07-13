{-# LANGUAGE OverloadedStrings #-}
module Handlers.ArticleWriter(
  articleWriter
)where

import Control.Monad.Trans
import Control.Monad.Except

import qualified Data.Text as T
import Data.Text.Read
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.Maybe
import Data.String

import Network.URI
import Network.HTTP.Types.Status
import qualified Text.Blaze.Html5 as H

import App.Types
import App.Context

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Tags as VT
import qualified Views.Articles as VA
import qualified Views.Layout as VL
import qualified Views.ArticleWriter as VAW

indexProcessor :: Response (Status,LT.Text)
indexProcessor  =  do
    let writer = VAW.render
    return $ (status200, VL.render "TTalk即时通信" [] [] [writer])


authUser userID req =
  if  userID == "1"
    then indexProcessor
    else return $ (status302,"/")

articleWriter :: Response LT.Text
articleWriter = do
  view $ withAuthorization authUser ()
