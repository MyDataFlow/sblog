{-# LANGUAGE OverloadedStrings #-}

module Models.DB.Tags where

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.Int
import Data.Time
import qualified Data.Text as T

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import App.Types
import Models.Schemas

fetchTagID :: T.Text -> Connection -> IO Int64
fetchTagID name c = do
  let q = "SELECT id FROM tags WHERE name = ?"
  rs <- query c q (Only name)
  return $ fromOnly $ head rs
fetchAllTags :: Connection -> IO [Tag]
fetchAllTags conn = do
  query_ conn "SELECT t.id,t.name,count(t.id) FROM tags AS t GROUP BY t.id"
fetchTags :: Int64 ->  Connection -> IO [Tag]
fetchTags rid c = do
  query c " SELECT t.id,t.name,count(t.id) FROM tags as t, taggings as tg \
    \ WHERE t.id = tg.tag_id AND tg.entry_id = ? GROUP BY t.id " (Only rid)

findOrAddTag :: T.Text -> Connection ->  IO Int64
findOrAddTag name c = do
    rs <- query c "SELECT t.id FROM tags as t WHERE t.name = ? " (Only name)
    if length rs  == 1
      then return $ fromOnly $ head rs
      else addTag
  where
    addTag = do
      rs <- query c "INSERT INTO tags ( name ) VALUES ( ? ) RETURNING id " (Only name)
      return $ fromOnly $ head rs

addTaggings :: Int64  -> [Int64] -> Connection -> IO [Int64]
addTaggings rid tags c = do
  mapM tagging tags
  where
    tagging tid = do
      rs <- query c "INSERT INTO taggings (entry_id,tag_id) \
      \ VALUES (?,?) RETURNING id" (rid,tid)
      return $ fromOnly $  head rs

removeAllTaggings :: Int64 -> Connection -> IO Int64
removeAllTaggings rid  c = do
  execute c "DELETE FROM taggings WHERE \
    \ entry_id = ?"  (Only rid)

removeTaggings :: Int64 -> Int64 -> Connection -> IO Int64
removeTaggings rid tid c = do
  execute c "DELETE FROM taggings WHERE \
    \ entry_id = ?  AND tag_id = ?"  (rid,tid)

removeTaggingsWithName :: Int64 -> [T.Text] -> Connection -> IO Int64
removeTaggingsWithName rid names c = do
    execute c "DELETE FROM taggings WHERE \
      \ entry_id = ?  AND tag_id IN \
      \ (SELECT id FROM tags WHERE name IN ?)" (rid,(In names))
