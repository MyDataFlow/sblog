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

import Utils.URI.String
import Utils.URI.Params

import Handlers.Actions.Types

import Models.Schema
import qualified Models.DB as DB
import Views.Common.Sitemap
--data SitemapUrl = SitemapUrl
--    { sitemapLoc :: String
--    , sitemapLastMod :: Maybe UTCTime
--    , sitemapChangeFreq :: Maybe SitemapChangeFreq
--    , sitemapPriority :: Maybe Double
--    }

url host path = show $ relativeTo (toURI path) (toURI host)
tagURI base t = show $ updateUrlParam "tag" t $ toURI base

generateSitemapUrl :: (String,Maybe UTCTime,SitemapChangeFreq,Double) -> SitemapUrl
generateSitemapUrl (url,time,cf,priority)=
  SitemapUrl{
            sitemapLoc = url
            ,sitemapLastMod = time
            ,sitemapChangeFreq = Just cf
            ,sitemapPriority = Just priority
          }
fromRecord :: String -> String -> (Int64,T.Text,LocalTime) -> Response [ (String,Maybe UTCTime,SitemapChangeFreq,Double)]
fromRecord host path  (rid,title,t) = do
  tags <- DB.runDBTry $ DB.fetchTags rid
  let bst = map generate tags
  return $ bs ++ bst
  where
    base = url host $ toPath [path, show $ rid,T.unpack title]
    lastMod = Just $ localTimeToUTC utc t
    bs = [(base,lastMod,Monthly,0.6)]
    generate t = (tagURI base (T.unpack $ tagName t),lastMod,Monthly,0.5)

fromTag :: String -> String  -> Tag -> (String,Maybe UTCTime,SitemapChangeFreq,Double)
fromTag host path t =
    (tagURI base (T.unpack $ tagName t),Nothing,Daily,0.8)
  where
    base = url host path

sitemapProcessor :: Response (Status,LT.Text)
sitemapProcessor  =  do
    s <- lift $ asks site
    let host = siteHost s
    es <- DB.runDBTry $ DB.fetchSitemap
    ts <- DB.runDBTry $ DB.fetchAllTags
    let baseItems = map generateSitemapUrl (bs host)
    let tItems = map generateSitemapUrl $ map (fromTag host "/entries") ts
    eTuples <- mapM (fromRecord host "entries" ) es
    let eItems = map generateSitemapUrl $ concat eTuples
    Web.setHeader "Content-Type" "text/xml"
    return (status200,LT.pack $ sitemap $ baseItems ++ tItems ++ eItems)
  where
    bs host = [
              (url host "/",Nothing,Daily,1)
              ,(url host "/entries",Nothing,Daily,1)
              ]


sitemapR :: Response ()
sitemapR = do
  view $ sitemapProcessor

robotsR :: Response ()
robotsR = do
  view $ do
    s <- lift $ asks site
    let host = siteHost s
    Web.setHeader "Content-Type" "text/plain"
    return (status200,LT.pack $ T.unpack $ robots host "sitemap.xml")
