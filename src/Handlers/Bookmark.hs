{-# LANGUAGE OverloadedStrings #-}
module Handlers.Bookmark(
  indexR
)where

import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Except (catchError)
import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status

import App.Types
import App.Context

import Utils.BlazeExtra.Pagination as Pagination

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Bookmark as VB

data BookmarkIndex = BookmarkIndex {
  page :: Integer
  ,count :: Integer
  ,tag :: Maybe T.Text
}


instance FormParams BookmarkIndex where
  fromParams m = BookmarkIndex <$>
    lookupInt "_page" 1 m <*>
    lookupInt "_count" 10 m <*>
    Just (M.lookup "tag" m)

indexProcessor :: Processor BookmarkIndex LT.Text
indexProcessor req =  do
  (ars,total,tid) <- fetchData
  tags <- DB.runDBTry $ DB.fetchTags 1
  host <- lift (asks siteHost)
  name <- lift (asks siteName)
  let pn = def {
    pnCurrentPage = (page req)
    ,pnTotal = (toInteger total)
    ,pnPerPage = (count req)
    ,pnMenuClass = "ui right floated pagination menu"
  }
  let r = VB.renderIndex host name (tag req) tid pn tags ars
  return $ (status200,r)
  where
    p = fromInteger $ page req
    c = fromInteger $ count req
    normalData = do
      ars <- DB.runDBTry  $ DB.fetchBookmarks p c
      total <- DB.runDBTry $ DB.fetchBookmarksCount
      return (ars,total,0)
    withTagData t = do
      tagID <- DB.runDBTry  $ DB.fetchTagID (T.unpack t)
      ars <- DB.runDBTry $ DB.fetchTagBookmarks tagID p c
      total <- DB.runDBTry $ DB.fetchTagBookmarksCount tagID
      return (ars,total,tagID)
    fetchData =
      case tag req of
        Nothing -> normalData
        Just t -> withTagData t



indexR :: Response LT.Text
indexR = do
  view $ withParams $ indexProcessor
