{-# LANGUAGE OverloadedStrings #-}

module Models.Bookmarks where

import Control.Applicative
import Control.Monad

import Data.Maybe

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import App.Types
import Models.DB.Schema

import qualified Models.Tags as Tags


fetchBookmarks ::  Int -> Int -> Connection -> IO [Bookmark]
fetchBookmarks page count c = do
  let offset = (page - 1) * count
  let digest (bid,title,summary,url,createdAt,updatedAt) = do
        tags <- Tags.fetchRelatedTags bid 1 c
        return $ Bookmark bid title summary url createdAt updatedAt tags
  rs <- query c "SELECT id,title,summary,url,created_at,updated_at FROM bookmarks \
    \ ORDER BY id DESC OFFSET ? LIMIT ?" (offset,count)
  mapM digest rs

addBookmark :: String -> String -> String -> [String] -> Connection-> IO [Int]
addBookmark title url summary tags c = do

  tagsID <- mapM (flip Tags.findOrAddTag c) tags
  rs <- query c "INSERT INTO bookmarks (title,summary,url) \
    \ VALUES (?,?,?) RETURNING id" (title,summary,url)
  let tid =  fromOnly $ head  rs
  Tags.linkTags tid 1 tagsID c
  return $ map fromOnly  rs
