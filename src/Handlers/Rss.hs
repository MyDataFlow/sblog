{-# LANGUAGE OverloadedStrings #-}
module Handlers.Rss(
  feedR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.Int
import Data.Time (UTCTime,LocalTime,localTimeToUTC,utc)

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class(MonadIO,liftIO)

import Network.URI
import Network.HTTP.Types.Status
import qualified Web.Scotty.Trans as Web


import App.Types
import App.Context
import Utils.URI.String
import Utils.URI.Params

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB
import Views.Common.Rss

feedProcessor :: Response (Status,LT.Text)
feedProcessor  =  do
    host <- lift (asks siteHost)
    name <- lift (asks siteName)
    brs <- DB.runDBTry $ DB.fetchBookmarks 1 10
    ars <- DB.runDBTry $ DB.fetchArticles True 1 10
    let feeds = (map (fromArticle host name) ars) ++ (map (fromBookmark host name) brs)
    Web.setHeader "Content-Type" "text/xml"
    return (status200,LT.pack $ renderFeed host name feeds)
  where
    url host path = show $ updateUrlParam "s" "rss" $ relativeTo (toURI path) (toURI host)
    fromArticle host name ar =
      let
        u = url host $ "/articles/" ++ (show $ DB.articleID ar)
        t = (DB.articleTitle ar) ++ "-" ++ name
        s = (DB.articleSummary ar)
        time = localTimeToUTC utc (DB.articleUpdatedAt ar)
      in
        (u,t,s,time)
    fromBookmark host name br =
      let
        u = url host $ "/bookmarks/" ++ (show $ DB.bookmarkID br)
        t = (DB.bookmarkTitle br) ++ "-" ++ name
        s = (DB.bookmarkTitle br)
        time = localTimeToUTC utc (DB.bookmarkUpdatedAt br)
      in
        (u,t,s,time)

feedR :: Response LT.Text
feedR = do
  view $ feedProcessor
