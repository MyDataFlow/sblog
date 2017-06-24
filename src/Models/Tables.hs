{-# LANGUAGE OverloadedStrings #-}

module Models.Tables(
  Article(..),
  Tag(..)
)where

data Article = Article {
  aid :: Int
  ,title :: String
  ,summary :: String
  ,tags :: [String]
} deriving (Show,Eq)

data Tag = Tag {
  tid :: Int
  ,name :: String
  ,count :: Int
}
