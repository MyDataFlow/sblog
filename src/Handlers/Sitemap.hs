{-# LANGUAGE OverloadedStrings #-}
module Handlers.Sitemap(
  sitemapR
  ,robotsR
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
import Views.Common.Sitemap
--data SitemapUrl = SitemapUrl
--    { sitemapLoc :: String
--    , sitemapLastMod :: Maybe UTCTime
--    , sitemapChangeFreq :: Maybe SitemapChangeFreq
--    , sitemapPriority :: Maybe Double
--    }

url host path = show $ relativeTo (toURI path) (toURI host)
tagURI base t = showURI $ updateUrlParam "tag" t $ toURI base

generateSitemapUrl :: (String,Maybe UTCTime,SitemapChangeFreq,Double) -> SitemapUrl
generateSitemapUrl (url,time,cf,priority)=
  SitemapUrl{
            sitemapLoc = url
            ,sitemapLastMod = time
            ,sitemapChangeFreq = Just cf
            ,sitemapPriority = Just priority
          }
fromRecord :: String -> String -> Int -> (Int64,LocalTime) -> Response [ (String,Maybe UTCTime,SitemapChangeFreq,Double)]
fromRecord host path rt (rid,t) = do
  tags <- DB.runDBTry $ DB.fetchRelatedTags rid rt
  let bst = map generate tags
  return $ bs ++ bst
  where
    base = url host $ path ++ (show $ rid)
    lastMod = Just $ localTimeToUTC utc t
    bs = [(base,lastMod,Monthly,0.6)]
    generate t = (tagURI base (DB.tagName t),lastMod,Monthly,0.5)

fromTag :: String -> String  -> DB.Tag -> (String,Maybe UTCTime,SitemapChangeFreq,Double)
fromTag host path t =
  (tagURI base (DB.tagName t),Nothing,Daily,0.8)

sitemapProcessor :: Response (Status,LT.Text)
sitemapProcessor  =  do
    host <- lift (asks siteHost)
    ars <- DB.runDBTry $ DB.fetchArticlesSitemap
    brs <- DB.runDBTry $ DB.fetchBookmarksSitemap
    ats <- DB.runDBTry $ DB.fetchTags 2
    bts <- DB.runDBTry $ DB.fetchTags 1
    let baseItems = map generateSitemapUrl (bs host)
    let atItems = map generateSitemapUrl $ map fromTag ats
    let btItems = map generateSitemapUrl $ map fromTag bts
    arTuples <- mapM (fromRecord host "/articles/"  2) ars
    brTuple <- mapM (fromRecord host "/bookmarks/" 1) brs
    let arItems = map generateSitemapUrl $ concat arTuples
    let brItems = map generateSitemapUrl $ concat brTuples
    Web.setHeader "Content-Type" "text/xml"
    return (status200,LT.pack $ sitemap $ baseItems ++ atItems ++ btItems ++ arItems ++ brItems)
  where
    bs host = [
              (url host "/",Nothing,Daily,1)
              ,(url host "/articles",Nothing,Daily,1)
              ,(url host "/bookmarks",Nothing,Daily,1)
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
