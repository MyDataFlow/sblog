{-# LANGUAGE OverloadedStrings #-}

module Models.Schemas.Tag where
import Data.Int
import Data.Time
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow

data Tag = Tag {
  tagID :: Int64
  ,tagName :: T.Text
  ,tagCount :: Int
} deriving (Show,Eq)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field
