{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Bookmarks.Edit(
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

import qualified Views.Admin.Bookmark as VAB

data BookmarkEdit  = BookmarkEdit {
  bid :: Integer
}

instance FormParams BookmarkEdit where
  fromParams m = BookmarkEdit <$>
    lookupInt "id" 0 m


editProcessor :: Processor BookmarkEdit LT.Text
editProcessor req =  do
  let intBid = fromInteger (bid req)
  if intBid == 0
    then return  (status302,"/admin/bookmarks/new")
    else do
      bs <- DB.runDBTry $ DB.fetchBookmark $ intBid
      return $ (status200,(VAB.renderWriter bs "/admin/bookmarks/create"))

authUser user req =
  if  user == "admin"
    then editProcessor req
    else return $ (status302,"/admin/login")

editR :: Response LT.Text
editR = do
  view $ withParams $ withAuthorization authUser
