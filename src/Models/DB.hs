{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Models.DB(
  createConnections,
  fetchTags,
  fetchArticles
)where

import Control.Applicative
import Control.Monad

import Data.Char(isNumber)
import Data.List(sortBy)
import Data.Maybe
import Data.Ord(comparing)
import Data.Pool(Pool, withResource, createPool)
import Data.String(fromString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Int
import Config

import Models.Tables

type PoolT = Pool Connection

createConnections :: AppConf -> IO PoolT
createConnections cfg = do
        db <- createPool (connect connInfo) close 1 30 10
        return db
    where
        connInfo = ConnectInfo {
            connectHost = dbHost cfg
            ,connectPort = fromIntegral (dbPort cfg)
            ,connectUser = dbUser cfg
            ,connectPassword = dbPassword cfg
            ,connectDatabase = dbDatabase cfg
        }

fetchTags :: PoolT -> IO [Tag]
fetchTags p = withResource p $ \c -> do
  let digest (tid,name,count) = do
        return $ Tag tid name count
  let q = "SELECT t.id,t.name,count(tg.tag_id) c \
  \ FROM tags as t, taggings as tg \
  \ WHERE tg.tag_id = t.id group by t.id \
  \ ORDER BY c DESC" :: Query
  rs  <- query_ c q
  mapM digest rs

fetchArticleTags :: Connection -> Int -> IO [String]
fetchArticleTags c aid = do
  rs <- query c "SELECT t.name FROM tags as t, taggings as tg \
  \ WHERE tg.bookmark_id = ? and t.id = tg.tag_id" (Only aid)
  return $ map fromOnly rs

fetchArticles :: PoolT -> IO [Article]
fetchArticles p = withResource p $ \c -> do
  let digest (aid,title,summary) = do
        tgs <- fetchArticleTags c aid
        return $ Article aid title summary tgs
  let q = "SELECT id,title,summary FROM bookmarks where id > 251" :: Query
  rs <- query_ c q
  mapM digest rs
