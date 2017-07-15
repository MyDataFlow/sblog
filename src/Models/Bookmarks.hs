{-# LANGUAGE OverloadedStrings #-}

module Models.Bookmarks where

import Control.Applicative
import Control.Monad

import Data.Maybe

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import App.Types
import Models.DB.Schema

import qualified Models.Tags as Tags


fetchBookmarks ::  Int -> Int -> Connection -> IO [Article]
fetchBookmarks page count c = do
  let offset = (page - 1) * count
  let digest (bid,title,summary) = do
        tags <- Tags.fetchRelatedTags bid 1 c
        return $ Bookmark bid title summary url tags
  rs <- query c "SELECT id,title,summary,url \
  \ FROM bookmarks OFFSET ? LIMIT ?" (offset,count)
  mapM digest rs
