
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where

import Data.Text.Lazy(Text)
import Control.Monad.IO.Class(liftIO)
import Data.String(fromString)
import Data.Maybe

import Network.URI

import Web.Scotty as S
import Models.DB as DB

import qualified Views.Tags as VT
import qualified Views.Articles as VA
import qualified Views.Layout as VL

indexPage :: DB.PoolT -> URI -> Int -> IO Text
indexPage db base page =  do
  t <- tags
  a <- articles
  p <- pagination
  return $ VL.render "TTalk即时通信" [] [t] [a,p]
  where
    tags = DB.fetchTags db >>= return . (flip VT.render $ 0)
    articles = DB.fetchArticles db page 20 >>= return . VA.render
    pagination = DB.fetchArticlesCount db
      >>= \x -> return $ VA.renderPagination base page 20 $ (head x)

tagPage :: DB.PoolT -> Int -> IO Text
tagPage db tagID = do
  t <- tags
  a <- articles
  return $ VL.render "TTalk即时通信" [] [t] [a]
  where
    tags = DB.fetchTags db >>= return . (flip VT.render $ tagID)
    articles = DB.fetchTagArticles db tagID 1 20 >>=  return . VA.render

routing db = do
  get "/" $ do
    ref <- param "page" `rescue` (const next)
    let page = read ref
    liftIO ( indexPage db  (fromJust (parseRelativeReference "/"))  page) >>= S.html
  get "/" $ do
    liftIO ( indexPage db  (fromJust (parseRelativeReference "/")) 1) >>= S.html
  get "/tags/:id" $ do
    ref <- param "id"
    let tagID = read ref
    liftIO (tagPage db tagID) >>= S.html
