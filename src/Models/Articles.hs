
{-# LANGUAGE OverloadedStrings #-}
module Models.Articles where

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

digest c ar = do
  tags <- Tags.fetchRelatedTags (articleID ar) 2 c
  return $ ar {articleTags = tags}

fetchArticlesSitemap:: Connection -> IO [(Int64,DT.LocalTime)]
fetchArticlesSitemap c = do
  query_ c "SELECT id,updated_at FROM articles WHERE published = true"

fetchAllArticles ::   Int -> Int -> Connection -> IO [Article]
fetchAllArticles page count  c = do
  let offset = (page - 1) * count
  rs <- query c "SELECT id,title,summary,body,markdown,published,created_at,updated_at \
    \ FROM articles  ORDER BY id DESC OFFSET ? LIMIT ?" (offset,count)
  mapM (digest c) rs

fetchAllArticlesCount :: Connection -> IO Int64
fetchAllArticlesCount  c = do
  rs <- query_ c "SELECT count(id) FROM articles "
  return $ fromOnly $ head rs

fetchArticles ::  Bool -> Int -> Int -> Connection -> IO [Article]
fetchArticles published page count  c = do
  let offset = (page - 1) * count
  rs <- query c "SELECT id,title,summary,body,markdown,published,created_at,updated_at \
  \ FROM articles WHERE published = ?  ORDER BY id DESC OFFSET ? LIMIT ?" (published,offset,count)
  mapM (digest c) rs

fetchArticlesCount :: Bool -> Connection -> IO Int64
fetchArticlesCount published c = do
  rs <- query c "SELECT count(id) FROM articles WHERE published = ? " (Only published)
  return $ fromOnly $ head rs

fetchTagArticles :: Bool ->  Int64 -> Int -> Int -> Connection -> IO [Article]
fetchTagArticles published tagID page count c = do
  let offset = (page - 1) * count
  rs <- query c "SELECT a.id,a.title,a.summary,a.body,a.markdown,a.published,\
    \a.created_at,a.updated_at FROM  taggings AS tg , articles AS a \
    \ WHERE tg.tag_id = ? AND a.id = tg.related_id AND a.published = ?\
    \ AND tg.related_type = 2 ORDER BY id DESC OFFSET ? LIMIT ?" (tagID,published,offset,count)
  mapM (digest c) rs
fetchTagArticlesCount :: Bool -> Int64 -> Connection -> IO Int64
fetchTagArticlesCount published tagID c = do
  rs <- query c "SELECT count(a.id) FROM taggings AS tg, articles AS a WHERE \
    \tg.tag_id = ? AND tg.related_type = 2 \
    \AND a.id = tg.related_id AND a.published = ?" (tagID,published)
  return $ fromOnly $ head rs

addArticle ::String -> String -> String -> String -> Bool-> [String] -> Connection-> IO Int64
addArticle title summary markdown body published tags c = do
    rs <- query c "INSERT INTO articles (title,summary,body,markdown,published) \
      \ VALUES (?,?,?,?,?) RETURNING id" (title,summary,body,markdown,published)
    let aid =  fromOnly $ head  rs
    tagsID <- mapM (flip Tags.findOrAddTag c) tags
    Tags.addTaggings aid 2 tagsID c
    return $ fromOnly $ head rs

removeArticle :: Int64 -> Connection -> IO Int64
removeArticle aid c = do
  Tags.removeAllTaggings aid 2 c
  execute c "DELETE FROM articles WHERE id = ?" (Only aid)

updateArticle :: Int64->String -> String -> String ->
  String -> Bool-> [String] -> Connection-> IO Int64
updateArticle aid title summary markdown body published tags c = do
    storedTags <- Tags.fetchRelatedTags aid 2 c
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
    execute c "UPDATE articles SET title = ? , summary = ? , \
      \body = ? , markdown = ? , published = ? , \
      \updated_at = ? WHERE id = ? " (title,summary,body,markdown,published,now,aid)
  where
    addTags [] = return [0]
    addTags ts = do
      tagsID <- mapM (flip Tags.findOrAddTag c) ts
      Tags.addTaggings aid 2 tagsID c
    deleteTags [] = return 0
    deleteTags ts = do
      Tags.removeTaggingsWithName aid 2 ts c

fetchArticle :: Int64 -> Connection -> IO Article
fetchArticle aid c = do
  rs <- query c "SELECT id,title,summary,body,markdown,published,created_at,updated_at FROM articles \
    \ WHERE id = ?" (Only aid)
  head $ map (digest c) rs
