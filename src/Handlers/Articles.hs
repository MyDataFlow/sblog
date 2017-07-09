{-# LANGUAGE OverloadedStrings #-}
module Handlers.Articles(
  articlesIndex
)where

import Control.Monad.Trans
import Control.Monad.Except

import qualified Data.Text as T
import Data.Text.Read
import qualified Data.Text.Lazy as LT 
import qualified Data.Map as M
import Data.Maybe 
import Data.String

import Network.URI
import Network.HTTP.Types.Status
import qualified Text.Blaze.Html5 as H

import App.Types
import App.Context

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Tags as VT
import qualified Views.Articles as VA
import qualified Views.Layout as VL


data ArticlesIndex = ArticlesIndex {
  page :: Integer
  ,count :: Integer
}

instance FromParams ArticlesIndex where
    fromParams m = ArticlesIndex <$> 
      lookupInt "page" 1 m <*> 
      lookupInt "count" 10 m

pagination :: URI -> Int -> Int -> Response H.Html
pagination base page count = do 
  c <- DB.runDB DB.fetchArticlesCount 
  return $ VA.renderPagination base page count $ (head c)

renderArticles :: Int -> Int -> Response H.Html
renderArticles page count = do 
  a <- DB.runDB $ DB.fetchArticles page count
  return $ VA.render a

indexProcessor :: Processor ArticlesIndex LT.Text
indexProcessor req =  do
    tags <- renderTags 0
    articles <- renderArticles p c
    paging <- pagination base p c 
    return $ (status200, VL.render "TTalk即时通信" [] [tags] [articles,paging])
  where
    p = fromInteger $ page req
    c = fromInteger $ count req
    base = (toUrl "/")


articlesIndex :: Response LT.Text
articlesIndex = do 
  withParams indexProcessor view
