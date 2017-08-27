{-# LANGUAGE OverloadedStrings #-}

module Models.Schemas.Entry where
import Data.Int
import Data.Time
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow

data Entry = Entry {
  entryID :: Int64
  ,entryTitle :: T.Text
  ,entrySummary :: T.Text
  ,entryURL :: T.Text
  ,entryBody :: T.Text
  ,entryMarkdown :: T.Text
  ,entryPublished :: Bool
  ,entryCreatedAt :: LocalTime
  ,entryUpdatedAt :: LocalTime
  ,entryTags :: [Tag]
} deriving (Show,Eq)

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field
                    <*> field <*> field <*> field
                    <*> field <*> field <*> pure []
