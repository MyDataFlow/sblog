{-# LANGUAGE OverloadedStrings #-}
module Handlers.Articles.Show(
  showR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Int

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)
import Control.Monad.Except (catchError)

import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status

import App.Types
import App.Context
import Utils.URI.Params
import Utils.URI.String

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Article as VA

data ArticleShow = ArticleShow {
  aid :: Integer
  ,tag :: Maybe T.Text
}


instance FormParams ArticleShow where
  fromParams m = ArticleShow <$>
    lookupInt "id" 0 m <*> Just (M.lookup "tag" m)

recommand:: Int64 -> [DB.Tag] -> Response [(String,String)]
recommand aid tags =
    mapM fetchRecommand tags >>= \rcs ->
      return $  removeDumps $ concat rcs
  where
    removeDumps = map head.group.sort
    toRecommand p (i,t) = (p ++ show i,t)
    fetchRecommand :: DB.Tag -> Response [(String,String)]
    fetchRecommand t = do
      ars <- DB.runDBTry $ DB.fetchRecommandArticle (DB.tagID t) aid
      brs <- DB.runDBTry $ DB.fetchRandRecommandBookmark (DB.tagID t)
      return $ union (map (toRecommand "/articles/") ars) (map (toRecommand "/bookmarks/") brs)
showProcessor :: Processor ArticleShow LT.Text
showProcessor req =  do
  ar <- DB.runDBTry $ DB.fetchArticle intBid
  bs <- breadcrumbs
  host <- lift (asks siteHost)
  name <- lift (asks siteName)
  rcs <- recommand (DB.articleID ar) (DB.articleTags ar)
  let r = VA.renderArticle host name bs (tag req /= Nothing) rcs ar
  return $ (status200,r)
  where
    intBid = fromInteger (aid req)
    breadcrumbs =
      case tag req of
        Nothing -> return [("文章","/articles")]
        Just t -> ref $ T.unpack t
    ref t = do
      r  <- Web.header "Referer"
      case r of
        Nothing -> return [("文章","/articles"),(t,tagURI t)]
        Just u -> return [("文章","/articles") ,(t,LT.unpack u)]
    tagURI t =
        showURI $ updateUrlParam "tag" t (toURI $ "/articles")


showR :: Response LT.Text
showR = do
  view $ withParams $ showProcessor
