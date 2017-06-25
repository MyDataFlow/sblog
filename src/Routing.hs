
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where

import Data.Text.Lazy(Text)
import Control.Monad.IO.Class(liftIO)

import Web.Scotty as S
import Models.DB as DB

import qualified Views.Tags as VT
import qualified Views.Articles as VA
import qualified Views.Layout as VL

indexPage :: DB.PoolT -> IO Text
indexPage db =  do
  t <- tags
  a <- articles
  return $ VL.render "TTalk即时通信" [] t a
  where
    tags = DB.fetchTags db >>= return . VT.render
    articles = DB.fetchArticles db 1 20 >>= return . VA.render


routing db = do
  get "/" $ do
    liftIO ( indexPage db ) >>= S.html
