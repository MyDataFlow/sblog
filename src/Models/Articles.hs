
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

digest c (aid,title,summary,body,markdown,published,createdAt,updatedAt) = do
        tags <- Tags.fetchRelatedTags aid 2 c
        return $ Article aid title summary  body markdown published createdAt updatedAt tags

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
    \ WHERE tg.tag_id = ? AND a.id = tg.realted_id AND a.published = ?\
    \ OFFSET ? LIMIT ?" (tagID,published,page,count)
  mapM (digest c) rs

addArticle ::String -> String -> String -> String -> Bool-> [String] -> Connection-> IO Int64
addArticle title summary markdown body published tags c = do
    tagsID <- mapM (flip Tags.findOrAddTag c) tags
    rs <- query c "INSERT INTO articles (title,summary,body,markdown,published) \
      \ VALUES (?,?,?,?,?) RETURNING id" (title,summary,body,markdown,published)
    let tid =  fromOnly $ head  rs
    Tags.addTaggings tid 2 tagsID c
    return $ fromOnly $ head rs

fetchArticle :: Int64 -> Connection -> IO Article
fetchArticle aid c = do
  rs <- query c "SELECT id,title,summary,body,markdown,published,created_at,updated_at FROM articles \
    \ WHERE id = ?" (Only aid)
  head $ map (digest c) rs
