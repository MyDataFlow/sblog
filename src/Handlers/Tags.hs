{-# LANGUAGE OverloadedStrings #-}
module Handlers.Tags(
  tagsIndex
)where

import Control.Monad.Trans
import Control.Monad.Except

import qualified Data.Text as T
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


data TagsShow = TagsShow {
  tid :: Integer
  ,page :: Integer
  ,count :: Integer
}

instance FromParams TagsShow where
    fromParams m = TagsShow <$> 
      lookupInt "id" 0 m <*> 
      lookupInt "page" 1 m <*> 
      lookupInt "count" 10 m

pagination :: URI -> Int-> Int -> Int -> Response H.Html
pagination base tagID page count = do 
  c <- DB.runDB $ DB.fetchTagArticlesCount tagID
  return $ VA.renderPagination base page count $ (head c)

renderArticles :: Int -> Int -> Int -> Response H.Html
renderArticles tagID page count = do 
  a <- DB.runDB $ DB.fetchTagArticles tagID page count
  return $ VA.render a

indexProcessor :: Processor TagsShow LT.Text
indexProcessor req =  do
    tags <- renderTags tagID
    articles <- renderArticles tagID p c
    paging <- pagination base tagID p c 
    return $ (status200, VL.render "TTalk即时通信" [] [tags] [articles,paging])
  where
    tagID = fromInteger $ tid req
    p = fromInteger $ page req
    c = fromInteger $ count req
    base = toUrl ("/tags/" ++ (show tagID))


tagsIndex :: Response LT.Text
tagsIndex = do 
  withParams indexProcessor view