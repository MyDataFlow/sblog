{-# LANGUAGE OverloadedStrings #-}
module Handlers.Articles.Index(
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

import qualified Views.Article as VA

data ArticleIndex = ArticleIndex {
  page :: Integer
  ,count :: Integer
  ,tag :: Maybe T.Text
}


instance FormParams ArticleIndex where
  fromParams m = ArticleIndex <$>
    lookupInt "_page" 1 m <*>
    lookupInt "_count" 10 m <*>
    Just (M.lookup "tag" m)

indexProcessor :: Processor ArticleIndex LT.Text
indexProcessor req =  do
  (ars,total,tid) <- fetchData
  tags <- DB.runDBTry $ DB.fetchTags 2
  name <- lift (asks siteName)
  let pn = def {
    pnTotal = (toInteger total)
    ,pnPerPage = (count req)
    ,pnMenuClass = "ui right floated pagination menu"
  }
  let r = VA.renderIndex name (tag req) tid pn tags ars
  return $ (status200,r)
  where
    p = fromInteger $ page req
    c = fromInteger $ count req
    normalData = do
      ars <- DB.runDBTry  $ DB.fetchArticles True p c
      total <- DB.runDBTry $ DB.fetchArticlesCount True
      return (ars,total,0)
    withTagData t = do
      tagID <- DB.runDBTry  $ DB.fetchTagID (T.unpack t)
      ars <- DB.runDBTry $ DB.fetchTagArticles True tagID p c
      total <- DB.runDBTry $ DB.fetchTagArticlesCount True tagID
      return (ars,total,tagID)
    fetchData =
      case tag req of
        Nothing -> normalData
        Just t -> withTagData t



indexR :: Response LT.Text
indexR = do
  view $ withParams $ indexProcessor
