{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Bookmarks.Create(
  createR
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
import qualified Views.Admin.Bookmark as VAB

data BookmarkForm = BookmarkForm {
  bid :: T.Text
  ,title :: T.Text
  ,url :: T.Text
  ,markdown :: T.Text
  ,tags :: T.Text
}
instance FormParams BookmarkForm where
    fromParams m = BookmarkForm <$>
      M.lookup "id" m <*>
      M.lookup "title"  m <*>
      M.lookup "url" m <*>
      M.lookup "editor-markdown-doc" m <*>
      M.lookup "tags" m


createProcessor :: Processor BookmarkForm LT.Text
createProcessor req =  do
    catchError action (\e -> return (status400,"unknown"))
  where
    t = T.unpack $ title req
    u = T.unpack $ url req
    m = T.unpack $ markdown req
    upackTags = map T.unpack $ T.split (==',') $ tags req
    action = do
      c <-  DB.runDBTry $ DB.addBookmark t u m upackTags
      return $ (status302,"/admin/bookmarks")

authUser user req =
  if  user == "admin"
    then createProcessor req
    else return $ (status302,"/admin/login")

createR :: Response LT.Text
createR = do
    view $ withParams $ withAuthorization authUser
