{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Bookmarks.Create(
  createR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Control.Monad.Except (catchError)
import Control.Monad.IO.Class(liftIO)
import Network.URI
import Network.HTTP.Types.Status

import App.Types
import App.Context
import Utils.URI.Params
import Utils.URI.String

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Layout as VL
import qualified Views.Admin.Bookmark as VAB

data BookmarkForm = BookmarkForm {
  bid :: Integer
  ,title :: T.Text
  ,url :: T.Text
  ,markdown :: T.Text
  ,summary :: T.Text
  ,tags :: T.Text
}
instance FormParams BookmarkForm where
    fromParams m = BookmarkForm <$>
      lookupInt "id" 0 m <*>
      M.lookup "title"  m <*>
      M.lookup "url" m <*>
      M.lookup "editor-markdown-doc" m <*>
      M.lookup "editor-html-code" m <*>
      M.lookup "tags" m


createProcessor :: Processor BookmarkForm LT.Text
createProcessor req =  do
    catchError action (\e -> return (status400,"unknown"))
  where
    params = [("utm_source","ttalk.im")
             ,("utm_campaign","TTalkIM")
             ,("utm_medium","website")]
    t = T.unpack $ title req
    u = show $ updateUrlParams params $ toURI (T.unpack $ url req)
    s = T.unpack $ summary req
    m = T.unpack $ markdown req
    upackTags = map T.unpack $ T.split (==',') $ tags req
    action = do
      liftIO $ putStrLn u
      c <-  DB.runDBTry $ DB.addBookmark t u s m upackTags
      return $ (status302,"/admin/bookmarks")

authUser user req =
  if  user == "admin"
    then createProcessor req
    else return $ (status302,"/admin/login")

createR :: Response LT.Text
createR = do
  -- view $ withParams $ withAuthorization authUser
  view $ withParams createProcessor
