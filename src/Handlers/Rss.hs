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
import Models.Schema
import qualified Models.DB as DB
import Views.Common.Rss

url host path = show $ updateUrlParam "s" "rss" $ relativeTo (toURI path) (toURI host)
fromEntry host name e =
  let
    u = url host $ toPath ["entries",show $ entryID e,T.unpack $ entryTitle e]
    t = T.unpack $ T.intercalate "-" [entryTitle e, name]
    s = case entrySummary e of
        Nothing -> ""
        Just s -> T.unpack s
    time = localTimeToUTC utc (entryUpdatedAt e)
  in
    (u,t,s,time)
feedProcessor :: Response (Status,LT.Text)
feedProcessor  =  do
  host <- lift (asks siteHost)
  name <- lift (asks siteName)
  es <- DB.runDBTry $ DB.fetchEntries True 1 10
  let feeds = map (fromEntry host (T.pack name)) es
  Web.setHeader "Content-Type" "text/xml"
  return (status200,LT.pack $ renderFeed host name "精选英文技术、创业文章，各种教程" feeds)


feedR :: Response LT.Text
feedR = do
  view $ feedProcessor
