{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Articles.Edit(
  editR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Network.HTTP.Types.Status

import App.Types
import App.Context

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Admin.Article as VAA

data ArticleEdit  = ArticleEdit {
  aid :: Integer
}

instance FormParams ArticleEdit where
  fromParams m = ArticleEdit <$>
    lookupInt "id" 0 m


editProcessor :: Processor ArticleEdit LT.Text
editProcessor req =  do
  let intBid = fromInteger (aid req)
  if intBid == 0
    then return  (status302,"/admin/articles/new")
    else do
      ar <- DB.runDBTry $ DB.fetchArticle $ intBid
      return $ (status200,(VAA.renderWriter ar "/admin/articles/create"))

authUser user req =
  if  user == "admin"
    then editProcessor req
    else return $ (status302,"/admin/login")

editR :: Response LT.Text
editR = do
  view $ withParams $ withAuthorization authUser
