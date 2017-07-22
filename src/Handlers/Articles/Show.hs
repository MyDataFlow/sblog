{-# LANGUAGE OverloadedStrings #-}
module Handlers.Articles.Show(
  showR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Control.Monad.Except (catchError)
import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status

import App.Types
import App.Context

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

showProcessor :: Processor ArticleShow LT.Text
showProcessor req =  do
  ar <- DB.runDBTry $ DB.fetchArticle intBid
  bs <- breadcrumbs
  let r = VA.renderArticle bs ar
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
        Nothing -> return [("文章","/articles")]
        Just u -> return [("文章","/articles") ,(t,LT.unpack u)]

showR :: Response LT.Text
showR = do
  view $ withParams $ showProcessor
