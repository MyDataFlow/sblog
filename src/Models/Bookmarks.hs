{-# LANGUAGE OverloadedStrings #-}

module Models.Bookmarks where

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.Int
import qualified Data.Set as Set
import qualified Data.Time as DT

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import App.Types
import Models.DB.Schema

import qualified Models.Tags as Tags


digest c br = do
  tags <- Tags.fetchRelatedTags (bookmarkID br) 1 c
  return $ br {bookmarkTags =  tags}
fetchBookmarksSitemap :: Connection -> IO [(Int64,DT.LocalTime)]
fetchBookmarksSitemap c = do
  query_ c "SELECT id,updated_at FROM bookmarks"

fetchBookmarks ::  Int -> Int -> Connection -> IO [Bookmark]
fetchBookmarks page count c = do
  let offset = (page - 1) * count
  rs <- query c "SELECT id,title,summary,markdown,url,created_at,updated_at FROM bookmarks \
    \ ORDER BY id DESC OFFSET ? LIMIT ?" (offset,count)
  mapM (digest c) rs

fetchBookmarksCount :: Connection -> IO Int64
fetchBookmarksCount c = do
  rs <- query_ c "SELECT count(id) FROM bookmarks"
  return $ fromOnly $ head rs

fetchBookmark :: Int64 -> Connection -> IO Bookmark
fetchBookmark bid c = do
  rs <- query c "SELECT id,title,summary,markdown,url,created_at,updated_at FROM bookmarks \
  \ WHERE id = ?" (Only bid)
  head $ map (digest c) rs

addBookmark :: String -> String -> String -> String ->[String] -> Connection-> IO Int64
addBookmark title url summary markdown tags c = do
  rs <- query c "INSERT INTO bookmarks (title,summary,markdown,url) \
    \ VALUES (?,?,?,?) RETURNING id" (title,summary,markdown,url)
  let tid =  fromOnly $ head  rs
  tagsID <- mapM (flip Tags.findOrAddTag c) tags
  Tags.addTaggings tid 1 tagsID c
  return $ fromOnly $ head rs

updateBookmark :: Int64-> String -> String -> String
  -> String ->[String] -> Connection-> IO Int64
updateBookmark bid title url summary markdown tags c = do
      storedTags <- Tags.fetchRelatedTags bid 1 c
      utc <- DT.getCurrentTime
      let now = DT.utcToLocalTime DT.utc utc
      let ts = map (\tag -> (tagName tag)) storedTags
      let storedSet = Set.fromList ts
      let newSet = Set.fromList tags
      let is = Set.intersection newSet storedSet
      let toDelete = Set.difference storedSet is
      let toInsert = Set.difference newSet is
      deleteTags (Set.toList toDelete)
      addTags (Set.toList toInsert)
      execute c "UPDATE bookmarks SET title = ? , summary = ? , \
        \url = ? , markdown = ? , \
        \updated_at = ? WHERE id = ? " (title,summary,url,markdown,now,bid)
    where
      addTags [] = return [0]
      addTags ts = do
        tagsID <- mapM (flip Tags.findOrAddTag c) ts
        Tags.addTaggings bid 1 tagsID c
      deleteTags [] = return 0
      deleteTags ts = do
        Tags.removeTaggingsWithName bid 1 ts c

removeBookmark :: Int64 -> Connection -> IO Int64
removeBookmark bid c = do
  Tags.removeAllTaggings bid 1 c
  execute c "DELETE FROM bookmarks WHERE id = ?" (Only bid)


fetchTagBookmarks :: Int64 -> Int -> Int -> Connection -> IO [Bookmark]
fetchTagBookmarks tagID page count c = do
    let offset = (page - 1) * count
    rs <- query c "SELECT b.id,b.title,b.summary,b.markdown,b.url,\
      \b.created_at,b.updated_at FROM  taggings AS tg , bookmarks AS b \
      \ WHERE tg.tag_id = ? AND b.id = tg.related_id \
      \ AND tg.related_type = 1 OFFSET ? LIMIT ?" (tagID,offset,count)
    mapM (digest c) rs

fetchTagBookmarksCount :: Int64 -> Connection -> IO Int64
fetchTagBookmarksCount tagID c = do
    rs <- query c "SELECT count(b.id) FROM taggings AS tg, bookmarks AS b WHERE \
      \tg.tag_id = ? AND tg.related_type = 1 \
      \AND b.id = tg.related_id" (Only tagID)
    return $ fromOnly $ head rs

fetchRandRecommandBookmark :: Int64 -> Connection -> IO [(Int64,String)]
fetchRandRecommandBookmark tid c = do
    rs <- query c "SELECT b.id,b.title FROM bookmarks b ,taggings t  \
      \ WHERE t.tag_id = ? AND t.related_type = 1 AND b.id = t.related_id \
      \ ORDER BY random() LIMIT 1" (Only tid)
    mapM toResult rs
  where
    toResult (i,t) = return (i,t)

fetchRecommandBookmark :: Int64 -> Int64 -> Connection -> IO [(Int64,String)]
fetchRecommandBookmark tid aid c = do
    rs <- query c "SELECT b.id,b.title FROM bookmarks b ,taggings t  \
      \ WHERE t.tag_id = ? AND t.related_type = 1 AND b.id = t.related_id \
      \ AND b.id <> ? ORDER BY random() LIMIT 1" (tid,aid)
    mapM toResult rs
  where
    toResult (i,t) = return (i,t)
