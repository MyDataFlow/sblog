
{-# LANGUAGE OverloadedStrings #-}
module Models.DB.Entries where

import Data.Maybe
import Data.Int
import Data.Time
import qualified Data.Set as Set
import qualified Data.Text as T

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow

import App.Types
import Models.Schemas
import qualified Models.DB.Tags as Tags

digest c r = do
  tags <- Tags.fetchTags (entryID r) c
  return $ r { entryTags = tags}

fetchSitemap:: Connection -> IO [(Int64,T.Text,LocalTime)]
fetchSitemap c = do
  query_ c "SELECT id,title,updated_at FROM entries WHERE published = true"

fetchAllEntries ::   Int -> Int -> Connection -> IO [Entry]
fetchAllEntries page count  c = do
  let offset = (page - 1) * count
  rs <- query c "SELECT * FROM entries \
    \ ORDER BY id DESC OFFSET ? LIMIT ?" (offset,count)
  mapM (digest c) rs

fetchAllEntriesCount :: Connection -> IO Int64
fetchAllEntriesCount  c = do
  rs <- query_ c "SELECT count(id) FROM entries "
  return $ fromOnly $ head rs

fetchEntries ::  Bool -> Int -> Int -> Connection -> IO [Entry]
fetchEntries published page count  c = do
  let offset = (page - 1) * count
  rs <- query c "SELECT * FROM entries \
    \ WHERE published = ?  ORDER BY id DESC OFFSET ? LIMIT ?" (published,offset,count)
  mapM (digest c) rs

fetchEntriesCount :: Bool -> Connection -> IO Int64
fetchEntriesCount published c = do
  rs <- query c "SELECT count(id) FROM entries WHERE published = ? " (Only published)
  return $ fromOnly $ head rs
fetchEntry :: Int64 -> Connection -> IO Entry
fetchEntry eid c = do
  rs <- query c "SELECT * FROM entries \
    \ WHERE id = ?" (Only eid)
  head $ map (digest c) rs

addEntry :: Entry -> [T.Text] -> Connection-> IO Int64
addEntry e tags conn = do
    rs <- query conn "INSERT INTO entries (title,url,summary,body,markdown \
      \ ,published,created_at,updated_at) \
      \ VALUES (?,?,?,?,?,?,?,?) RETURNING id" e
    let aid =  fromOnly $ head  rs
    tagsID <- mapM (flip Tags.findOrAddTag conn) tags
    Tags.addTaggings aid tagsID conn
    return $ aid

removeEntry :: Int64 -> Connection -> IO Int64
removeEntry eid c = do
  Tags.removeAllTaggings eid  c
  execute c "DELETE FROM entries WHERE id = ?" (Only eid)

updateEntry :: Entry -> [T.Text] -> Connection -> IO Int64
updateEntry e tags conn = do
    storedTags <- Tags.fetchTags (entryID e) conn
    let ts = map (\tag -> (tagName tag)) storedTags
    let storedSet = Set.fromList ts
    let newSet = Set.fromList tags
    let is = Set.intersection newSet storedSet
    let toDelete = Set.difference storedSet is
    let toInsert = Set.difference newSet is
    deleteTags (Set.toList toDelete)
    addTags (Set.toList toInsert)
    execute conn "UPDATE articles SET title = ? , url = ?, summary = ? , \
      \body = ? , markdown = ? , published = ? , \
      \updated_at = ? WHERE id = ? " e
  where
    addTags [] = return [0]
    addTags ts = do
      tagsID <- mapM (flip Tags.findOrAddTag conn) ts
      Tags.addTaggings (entryID e) tagsID conn
    deleteTags [] = return 0
    deleteTags ts = do
      Tags.removeTaggingsWithName (entryID e) ts conn


fetchRandRecommandArticle :: Int64 -> Connection -> IO [(Int64,String)]
fetchRandRecommandArticle tid c = do
    rs <- query c "SELECT a.id,a.title FROM articles a ,taggings t  \
      \ WHERE t.tag_id = ? AND t.related_type = 2 AND a.id = t.related_id \
      \ AND a.published = true  ORDER BY random() LIMIT 1" (Only tid)
    mapM toResult rs
  where
    toResult (i,t) = return (i,t)

fetchRecommandArticle :: Int64 -> Int64 -> Connection -> IO [(Int64,String)]
fetchRecommandArticle tid aid c = do
    rs <- query c "SELECT a.id,a.title FROM articles a ,taggings t  \
    \ WHERE t.tag_id = ? AND t.related_type = 2 AND a.id = t.related_id \
    \ AND a.id <> ? AND a.published = true  ORDER BY random() LIMIT 1" (tid,aid)
    mapM toResult rs
  where
    toResult (i,t) = return (i,t)
