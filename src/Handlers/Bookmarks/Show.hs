{-# LANGUAGE OverloadedStrings #-}
module Handlers.Bookmarks.Show(
  showR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
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

import qualified Views.Bookmark as VB

data BookmarkShow = BookmarkShow {
  bid :: Integer
  ,tag :: Maybe T.Text
}


instance FormParams BookmarkShow where
  fromParams m = BookmarkShow <$>
    lookupInt "id" 0 m <*> Just (M.lookup "tag" m)

recommand:: Int64 -> [DB.Tag] -> Response [(String,String)]
recommand bid tags =
  mapM fetchRecommand tags >>= \rcs ->
    return $  removeDumps $ concat rcs
  where
    removeDumps = map head.group.sort -- (map head).group sort
    toRecommand p (i,t) = (p ++ show i,t)
    fetchRecommand :: DB.Tag -> Response [(String,String)]
    fetchRecommand t = do
      ars <- DB.runDBTry $ DB.fetchRandRecommandArticle (DB.tagID t)
      brs <- DB.runDBTry $ DB.fetchRecommandBookmark (DB.tagID t) bid
      return $ union (map (toRecommand "/articles/") ars) (map (toRecommand "/bookmarks/") brs)
showProcessor :: Processor BookmarkShow LT.Text
showProcessor req =  do
  br <- DB.runDBTry $ DB.fetchBookmark intBid
  bs <- breadcrumbs
  rcs <- recommand (DB.bookmarkID br) (DB.bookmarkTags br)
  r <- VB.renderBookmark bs (tag req /= Nothing) rcs br
  return $ (status200,r)
  where
    intBid = fromInteger (bid req)
    breadcrumbs =
      case tag req of
        Nothing -> return [("书签","/bookmarks")]
        Just t -> ref $ T.unpack t
    ref t = do
      r  <- Web.header "Referer"
      case r of
        Nothing -> return [("书签","/bookmarks"),(t,tagURI t)]
        Just u -> return [("书签","/bookmarks") ,(t,LT.unpack u)]
    tagURI t =
      showURI $ updateUrlParam "tag" t (toURI $ "/bookmarks")
showR :: Response LT.Text
showR = do
  view $ withParams $ showProcessor
