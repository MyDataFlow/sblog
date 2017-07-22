{-# LANGUAGE OverloadedStrings #-}

module Models.Tags where

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.Int

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import App.Types
import Models.DB.Schema

fetchTagID :: String-> Connection -> IO Int64
fetchTagID name c = do
  let q = "SELECT id FROM tags WHERE name = ?"
  rs <- query c q (Only name)
  return $ fromOnly $ head rs

fetchTags :: Int -> Connection -> IO [Tag]
fetchTags rt c = do
  let q = "SELECT t.id,t.name,count(tg.tag_id) c \
  \ FROM tags as t, taggings as tg \
  \ WHERE tg.related_type =? AND tg.tag_id = t.id group by t.id \
  \ ORDER BY c DESC"
  query c q (Only rt)


fetchRelatedTags ::  Int64 -> Int ->  Connection -> IO [Tag]
fetchRelatedTags rid t c = do
    query c " SELECT t.id,t.name,count(t.id) FROM tags as t, taggings as tg \
    \ WHERE t.id = tg.tag_id GROUP BY t.id HAVING \
    \t.id IN (SELECT tag_id FROM taggings WHERE related_type = ? AND related_id = ? )" (t,rid)

findOrAddTag :: String -> Connection ->  IO Int64
findOrAddTag name c = do
    rs <- query c "SELECT t.id FROM tags as t WHERE t.name = ? " (Only name)
    if length rs  == 1
      then return $ fromOnly $ head rs
      else addTag
  where
    addTag = do
      rs <- query c "INSERT INTO tags ( name ) VALUES ( ? ) RETURNING id " (Only name)
      return $ fromOnly $ head rs

addTaggings :: Int64 -> Int -> [Int64] -> Connection -> IO [Int64]
addTaggings rid rt tags c = do
  mapM tagging tags
  where
    tagging tid = do
      rs <- query c "INSERT INTO taggings (tag_id,related_type,related_id) \
      \ VALUES (?,?,?) RETURNING id" (tid,rt,rid)
      return $ fromOnly $  head rs

removeAllTaggings :: Int64 -> Int -> Connection -> IO Int64
removeAllTaggings rid rt c = do
  execute c "DELETE FROM taggings WHERE \
    \related_type = ? AND related_id = ?"  (rt,rid)

removeTaggings :: Int64 -> Int64 -> Int -> Connection -> IO Int64
removeTaggings rid tid rt c = do
  execute c "DELETE FROM taggings WHERE \
    \related_type = ? AND tag_id = ? AND related_id = ?"  (rt,tid,rid)

removeTaggingsWithName :: Int64 -> Int -> [String] -> Connection -> IO Int64
removeTaggingsWithName rid rt names c = do
    execute c "DELETE FROM taggings WHERE \
      \related_type = ? AND related_id = ? AND \
      \tag_id IN (SELECT id FROM tags WHERE name IN ?) " (rt,rid,toNames)
  where
    toNames = foldr1 (\w s -> w ++ ',':s) names
