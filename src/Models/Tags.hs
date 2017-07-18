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
fetchRelatedTags rid t c = do
    rs <- query c " SELECT t.id,t.name FROM tags as t, taggings as tg \
    \ WHERE tg.related_type = ? AND tg.related_id = ? AND t.id = tg.tag_id" (t,rid)
    mapM digest rs
  where
    digest (tid,name) = return $ Tag tid name 1

findOrAddTag :: String -> Connection ->  IO Int
findOrAddTag name c = do
    rs <- query c "SELECT t.id FROM tags as t WHERE t.name = ? " (Only name)
    if length rs  == 1
      then return $ fromOnly $ head rs
      else addTag
  where
    addTag = do
      rs <- query c "INSERT INTO tags ( name ) VALUES ( ? ) RETURNING id " (Only name)
      return $ fromOnly $ head rs

linkTags :: Int -> Int -> [Int] -> Connection -> IO [Int]
linkTags rid rt tags c = do
  mapM tagging tags
  where
    tagging tid = do
      rs <- query c "INSERT INTO taggings (tag_id,related_type,related_id) \
      \ VALUES (?,?,?) RETURNING id" (tid,rt,rid)
      return $ fromOnly $  head rs
