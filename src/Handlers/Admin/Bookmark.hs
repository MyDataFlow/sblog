{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Bookmark(
  bookmarkNew
  ,bookmarkCreate
  ,bookmarkIndex
  ,bookmarkEditor
)where

import Control.Exception
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
import qualified Web.Scotty.Trans  as Web

import Database.PostgreSQL.Simple.Errors

import App.Types
import App.Context

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Layout as VL
import qualified Views.Admin.Bookmark as VAB

data BookmarkForm = BookmarkForm {
  fbid :: T.Text
  ,title :: T.Text
  ,url :: T.Text
  ,markdown :: T.Text
  ,tags :: T.Text
}
data BookmarkIndex = BookmarkIndex {
  page :: Integer
  ,count :: Integer
}
data BookmarkEditor  = BookmarkEditor {
  bid :: Integer
}

instance FormParams BookmarkForm where
    fromParams m = BookmarkForm <$>
      M.lookup "id" m <*>
      M.lookup "title"  m <*>
      M.lookup "url" m <*>
      M.lookup "editor-markdown-doc" m <*>
      M.lookup "tags" m

instance FormParams BookmarkIndex where
  fromParams m = BookmarkIndex <$>
    lookupInt "_page" 1 m <*>
    lookupInt "_count" 10 m
instance FormParams BookmarkEditor where
  fromParams m = BookmarkEditor <$>
    lookupInt "id" 0 m

newProcessor :: Response (Status,LT.Text)
newProcessor  =  do
    bookmark <- liftIO $ DB.defBookmark
    let writer = VAB.renderWriter bookmark "/admin/bookmarks/create"
    return $ (status200, VL.renderAdmin
      ["/bower_components/editor.md/css/editormd.min.css"]
      ["/bower_components/editor.md/editormd.min.js"
      ,"/bookmark/editor.js"]
      [writer])

bookmarkNew :: Response LT.Text
bookmarkNew = do
  view $ newProcessor

createProcessor :: Processor BookmarkForm LT.Text
createProcessor req =  do
    catchError action (\e -> return (status400,"unknown"))
    -- action
  where
    t = T.unpack $ title req
    u = T.unpack $ url req
    m = T.unpack $ markdown req
    upackTags = map T.unpack $ T.split (==',') $ tags req
    action = do
      c <-  DB.runDBTry $ DB.addBookmark t u m upackTags
      return $ (status302,"/admin/bookmarks")


bookmarkCreate :: Response LT.Text
bookmarkCreate = do
  withParams createProcessor view

renderBookmarks :: Int -> Int -> Response H.Html
renderBookmarks page count = do
  a <- DB.runDBTry $ DB.fetchBookmarks page count
  return $ VAB.renderIndex a

indexProcessor :: Processor BookmarkIndex LT.Text
indexProcessor req = do
    bv <-  renderBookmarks p c
    return $ (status200, VL.renderAdmin
      ["/bower_components/editor.md/css/editormd.min.css"]
      ["/bower_components/editor.md/editormd.min.js"
      ,"/bookmark/editor.js"]
      [bv])
  where
    p = fromInteger $ page req
    c = fromInteger $ count req
    base = (toUrl "/admin/bookmarks")

bookmarkIndex :: Response LT.Text
bookmarkIndex = do
  withParams indexProcessor view

editorProcessor :: Processor BookmarkEditor LT.Text
editorProcessor req =  do
  let intBid = fromInteger (bid req)
  if intBid == 0
    then return  (status302,"/admin/bookmarks/new")
    else do
      bs <- DB.runDBTry $ DB.fetchBookmark $ intBid
      let writer = VAB.renderWriter bs "/admin/bookmarks/create"
      return $ (status200, VL.renderAdmin
        ["/bower_components/editor.md/css/editormd.min.css"]
        ["/bower_components/editor.md/editormd.min.js"
        ,"/bookmark/editor.js"]
        [writer])

bookmarkEditor :: Response LT.Text
bookmarkEditor = do
  withParams editorProcessor view
