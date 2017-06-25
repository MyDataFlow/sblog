
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where

import Data.Text.Lazy(Text)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Class(lift)
import Control.Exception

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
    articles = DB.fetchArticles db page 10 >>= return . VA.render
    pagination = DB.fetchArticlesCount db
      >>= \x -> return $ VA.renderPagination base page 10 $ (head x)

tagPage :: DB.PoolT -> URI -> Int -> Int -> IO Text
tagPage db base tagID page = do
  t <- tags
  a <- articles
  p <- pagination
  return $ VL.render "TTalk即时通信" [] [t] [a,p]
  where
    tags = DB.fetchTags db >>= return . (flip VT.render $ tagID)
    articles = DB.fetchTagArticles db tagID 1 10 >>=  return . VA.render
    pagination = DB.fetchTagArticlesCount db tagID
      >>= \x -> return $ VA.renderPagination base page 10 $ (head x)
routing db = do
  get "/" $ do
    ref <- rescue (param "page")  (\e -> return "1")
    let page = read ref
    liftIO ( indexPage db (toUrl "/")  page) >>= S.html
  get "/tags/:id" $ do
    ref <- param "id"
    refPage <- param "page" `rescue` (\e -> return "1")
    let page = read refPage
    let tagID = read ref
    liftIO (tagPage db (toUrl $ "/tags/" ++ (show tagID)) tagID page) >>= S.html
  get "/tags/:id" $ do
      ref <- param "id"
      let tagID = read ref
      liftIO (tagPage db (toUrl $ "/tags/" ++ (show tagID)) tagID 1) >>= S.html
  where
    toUrl u = fromJust $ parseRelativeReference u
