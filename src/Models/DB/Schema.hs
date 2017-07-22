{-# LANGUAGE OverloadedStrings #-}

module Models.DB.Schema where

import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import qualified Data.Time as DT

data Tag = Tag {
  tagID :: Int64
  ,tagName :: String
  ,tagCount :: Int
} deriving (Show,Eq)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field
instance ToRow Tag where
  toRow r = toRow (tagID r,tagName r,tagCount r)

data Article = Article {
  articleID :: Int64
  ,articleTitle :: String
  ,articleSummary :: String
  ,articleBody :: String
  ,articleMarkdown :: String
  ,articlePublished :: Bool
  ,articleCreatedAt ::  DT.LocalTime
  ,articleUpdatedAt :: DT.LocalTime
  ,articleTags :: [Tag]
} deriving (Show,Eq)

instance FromRow Article where
  fromRow = Article <$> field <*> field <*> field
                    <*> field <*> field <*> field
                    <*> field <*> field <*> pure []
instance ToRow Article where
  toRow r = toRow (articleID r,articleTitle r
                  ,articleSummary r,articleBody r
                  ,articleMarkdown r,articlePublished r
                  ,articleCreatedAt r,articleUpdatedAt r)

data Bookmark = Bookmark {
  bookmarkID :: Int64
  ,bookmarkTitle :: String
  ,bookmarkSummary :: String
  ,bookmarkMarkdown :: String
  ,bookmarkUrl :: String
  ,bookmarkCreatedAt ::  DT.LocalTime
  ,bookmarkUpdatedAt :: DT.LocalTime
  ,bookmarkTags :: [Tag]
} deriving (Show,Eq)

instance FromRow Bookmark where
  fromRow = Bookmark <$> field <*> field <*> field
                     <*> field <*> field <*> field
                     <*> field <*> pure []
instance ToRow Bookmark where
  toRow r = toRow (bookmarkID r,bookmarkTitle r
                  ,bookmarkSummary r,bookmarkMarkdown r
                  ,bookmarkUrl r,bookmarkCreatedAt r
                  ,bookmarkUpdatedAt r)

defBookmark :: IO Bookmark
defBookmark = do
  utc <- DT.getCurrentTime
  let now = DT.utcToLocalTime DT.utc utc
  return $ Bookmark 0 "" "" "" "" now now []

defArticle :: IO Article
defArticle = do
  utc <- DT.getCurrentTime
  let now = DT.utcToLocalTime DT.utc utc
  return $ Article 0 "" "" "" "" False now now []
