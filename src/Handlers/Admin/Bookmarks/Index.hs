{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Bookmarks.Index(
  indexR
)where

import Data.Default
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Network.HTTP.Types.Status

import App.Types
import App.Context
import Utils.URI.String

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB
import Utils.BlazeExtra.Pagination as Pagination

import qualified Views.Layout as VL
import qualified Views.Admin.Bookmark as VAB

data BookmarkIndex = BookmarkIndex {
  page :: Integer
  ,count :: Integer
}


instance FormParams BookmarkIndex where
  fromParams m = BookmarkIndex <$>
    lookupInt "_page" 1 m <*>
    lookupInt "_count" 10 m

indexProcessor :: Processor BookmarkIndex LT.Text
indexProcessor req = do
    bv <-  renderBookmarks
    return $ (status200,
              VL.renderAdmin 1
                ["/bower_components/editor.md/css/editormd.min.css"]
                ["/bower_components/editor.md/editormd.min.js"
                ,"/assets/admin/index.js"]
                [bv] )
  where
    p = fromInteger $ page req
    c = fromInteger $ count req
    base = (toURI "/admin/bookmarks")
    renderBookmarks = do
      a <- DB.runDBTry $ DB.fetchBookmarks p c
      total <- DB.runDBTry $ DB.fetchBookmarksCount
      let pn = def {
        pnTotal = (toInteger total)
        ,pnPerPage = (count req)
        ,pnMenuClass = "ui right floated pagination menu"
      }
      return $ VAB.renderIndex a base pn

authUser user req =
  if  user == "admin"
    then indexProcessor req
    else return $ (status302,"/admin/login")

indexR :: Response LT.Text
indexR = do
  -- view $ withParams $ withAuthorization authUser
  view $ withParams $ indexProcessor
