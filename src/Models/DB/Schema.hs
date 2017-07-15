{-# LANGUAGE OverloadedStrings #-}

module Models.DB.Schema where

data Tag = Tag {
  tid :: Int
  ,name :: String
  ,count :: Int
} deriving (Show,Eq)

data Article = Article {
  aid :: Int
  ,title :: String
  ,summary :: String
  ,tags :: [Tag]
} deriving (Show,Eq)

data Bookmark = Bookmark {
  bid :: Int
  ,btitle :: String
  ,bsummary :: String
  ,burl :: String
  ,btags :: [Tag]
}
