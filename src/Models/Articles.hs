
{-# LANGUAGE OverloadedStrings #-}
module Models.Articles where

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.Int

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
    \ AND tg.related_type = 2 OFFSET ? LIMIT ?" (tagID,published,offset,count)
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
    let tid =  fromOnly $ head  rs
    tagsID <- mapM (flip Tags.findOrAddTag c) tags
    Tags.addTaggings tid 2 tagsID c
    return $ fromOnly $ head rs

removeArticle :: Int64 -> Connection -> IO Int64
removeArticle aid c = do
  Tags.removeTaggings aid 2 c
  execute c "DELETE FROM articles WHERE id = ?" (Only aid)

fetchArticle :: Int64 -> Connection -> IO Article
fetchArticle aid c = do
  rs <- query c "SELECT id,title,summary,body,markdown,published,created_at,updated_at FROM articles \
    \ WHERE id = ?" (Only aid)
  head $ map (digest c) rs
