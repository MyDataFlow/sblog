{-# LANGUAGE OverloadedStrings #-}
module Models.Articles where

import Control.Applicative
import Control.Monad

import Data.Maybe

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import App.Types
import Models.DB.Schema

fetchArticleTags ::  Int -> Connection -> IO [Tag]
fetchArticleTags aid c = do
    rs <- query c "SELECT t.id,t.name FROM tags as t, taggings as tg \
    \ WHERE tg.bookmark_id = ? AND t.id = tg.tag_id" (Only aid)
    mapM digest rs
  where
    digest (tid,name) = do
      return $ Tag tid name 1

fetchArticles ::  Int -> Int -> Connection -> IO [Article]
fetchArticles page count c = do
  let offset = (page - 1) * count
  let digest (aid,title,summary) = do
        tags <- fetchArticleTags aid c
        return $ Article aid title summary tags
  rs <- query c "SELECT id,title,summary \
  \ FROM bookmarks OFFSET ? LIMIT ?" (offset,count)
  mapM digest rs


fetchArticlesCount :: Connection -> IO [Int]
fetchArticlesCount c = do
  rs <- query_ c "SELECT count(id) FROM bookmarks"
  return $ map fromOnly rs


fetchTagArticles ::  Int -> Int -> Int -> Connection -> IO [Article]
fetchTagArticles tagID page count c = do
  let offset = (page - 1) * count
  let digest (aid,title,summary) = do
        tags <- fetchArticleTags aid c
        return $ Article aid title summary tags
  rs <- query c "SELECT b.id,b.title,b.summary FROM \
  \ taggings as tg , bookmarks as b where tg.tag_id = ? \
  \ AND b.id = tg.bookmark_id OFFSET ? LIMIT ?" (tagID,page,count)
  mapM digest rs

fetchTagArticlesCount :: Int -> Connection -> IO [Int]
fetchTagArticlesCount tagID c = do
  rs <- query c "SELECT count(bookmark_id) FROM \
  \ taggings WHERE taggings.tag_id = ? " (Only tagID)
  return $ map fromOnly  rs
