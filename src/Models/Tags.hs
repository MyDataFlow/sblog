{-# LANGUAGE OverloadedStrings #-}

module Models.Tags where

import Control.Applicative
import Control.Monad

import Data.Maybe

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import App.Types
import Models.DB.Schema

fetchTags :: Connection -> IO [Tag]
fetchTags c = do
  let digest (tid,name,count) = return $ Tag tid name count
  let q = "SELECT t.id,t.name,count(tg.tag_id) c \
  \ FROM tags as t, taggings as tg \
  \ WHERE tg.tag_id = t.id group by t.id \
  \ ORDER BY c DESC" :: Query
  rs  <- query_ c q
  mapM digest rs

fetchRelatedTags ::  Int -> Int ->  Connection -> IO [Tag]
fetchRelatedTags pkID t c = do
      rs <- query c "SELECT t.id,t.name FROM tags as t, taggings as tg \
      \ WHERE tg.related_type = ? AND tg.related_id = ? AND t.id = tg.tag_id" (t,pkID)
      mapM digest rs
    where
      digest (tid,name) = do
        return $ Tag tid name 1
