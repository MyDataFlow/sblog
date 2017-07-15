{-# LANGUAGE OverloadedStrings #-}

module Models.DB.Schema where

import qualified Data.Time as DT

data Tag = Tag {
  tid :: Int
  ,name :: String
  ,count :: Int
} deriving (Show)

data Article = Article {
  aid :: Int
  ,atitle :: String
  ,asummary :: String
  ,apublished :: Bool
  ,acreatedAt ::  DT.LocalTime
  ,aupdatedAt :: DT.LocalTime
  ,atags :: [Tag]
} deriving (Show)

data Bookmark = Bookmark {
  bid :: Int
  ,btitle :: String
  ,bsummary :: String
  ,burl :: String
  ,bcreatedAt ::  DT.LocalTime
  ,bupdatedAt :: DT.LocalTime
  ,btags :: [Tag]
} deriving (Show)

defBookmark :: IO Bookmark
defBookmark = do
  utc <- DT.getCurrentTime
  let now = DT.utcToLocalTime DT.utc utc
  return $ Bookmark 0 "" "" "" now now []
