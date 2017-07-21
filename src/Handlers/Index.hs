{-# LANGUAGE OverloadedStrings #-}
module Handlers.Index(
  indexR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Control.Monad.Except (catchError)

import Network.HTTP.Types.Status

import App.Types
import App.Context

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Layout as VL
import qualified Views.Index as VI
indexProcessor :: Response (Status,LT.Text)
indexProcessor  =  do
  bookmarks <- DB.runDBTry $ DB.fetchBookmarks 1 5
  articles <- DB.runDBTry $ DB.fetchArticles True 1 5
  let index = VI.renderMain bookmarks articles
  return $ (status200,VL.render 1 "首页-TTalkIM" [] [] [index])

indexR :: Response LT.Text
indexR = do
  view $ indexProcessor
