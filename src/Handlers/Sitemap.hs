{-# LANGUAGE OverloadedStrings #-}
module Handlers.Sitemap(
  sitemapR
  ,robotsR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.Time (UTCTime,localTimeToUTC,utc)

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)
import Control.Monad.Except (catchError)

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
import Views.Common.Sitemap
--data SitemapUrl = SitemapUrl
--    { sitemapLoc :: String
--    , sitemapLastMod :: Maybe UTCTime
--    , sitemapChangeFreq :: Maybe SitemapChangeFreq
--    , sitemapPriority :: Maybe Double
--    }

sitemapProcessor :: Response (Status,LT.Text)
sitemapProcessor  =  do
    host <- lift (asks siteHost)
    ars <- DB.runDBTry $ DB.fetchArticlesSitemap
    brs <- DB.runDBTry $ DB.fetchBookmarksSitemap
    let sa = map (fromArticle host) ars
    let sb = map (fromBookmark host) brs
    Web.setHeader "Content-Type" "text/xml"
    return (status200,LT.pack $ sitemap $ (baseItems host) ++ sa ++ sb)
  where
    url host path = show $ relativeTo (toURI path) (toURI host)
    fromBookmark host (bid,t) = SitemapUrl{
                    sitemapLoc = url host $ "/bookmarks/" ++ (show $ bid)
                    ,sitemapLastMod = Just $ localTimeToUTC utc t
                    ,sitemapChangeFreq = Just Weekly
                    ,sitemapPriority = Nothing
                  }
    fromArticle host (aid,t) =
                  SitemapUrl{
                    sitemapLoc = url host $ "/articles/" ++ (show $ aid)
                    ,sitemapLastMod = Just $ localTimeToUTC utc t
                    ,sitemapChangeFreq = Just Weekly
                    ,sitemapPriority = Nothing
                  }
    baseItems host =
      [
         SitemapUrl{
                  sitemapLoc = url host "/"
                  ,sitemapLastMod = Nothing
                  ,sitemapChangeFreq = Just Daily
                  ,sitemapPriority = Nothing
                  },
          SitemapUrl{
                    sitemapLoc = url host "/articles"
                    ,sitemapLastMod = Nothing
                    ,sitemapChangeFreq = Just Weekly
                    ,sitemapPriority = Nothing
                    },
          SitemapUrl{
                    sitemapLoc = url host "/bookmarks"
                    ,sitemapLastMod = Nothing
                    ,sitemapChangeFreq = Just Daily
                    ,sitemapPriority = Nothing
                    }
      ]

sitemapR :: Response LT.Text
sitemapR = do
  view $ sitemapProcessor

robotsR :: Response LT.Text
robotsR = do
  view $ do
    host <- lift (asks siteHost)
    Web.setHeader "Content-Type" "text/plain"
    return (status200,LT.pack $ T.unpack $ robots host "sitemap.xml")
