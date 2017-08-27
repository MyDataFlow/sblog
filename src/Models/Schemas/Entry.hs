{-# LANGUAGE OverloadedStrings #-}

module Models.Schemas.Entry where
import Data.Int
import Data.Time
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Models.Schemas.Tag

data Entry = Entry {
  entryID :: Int64
  ,entryTitle :: T.Text
  ,entryURL :: Maybe T.Text
  ,entrySummary :: T.Text
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
                    <*> field <*> field <*> field <*> pure []
instance ToRow Entry where
  toRow r =
    if (entryID r) == 0
      then toRow (
            entryTitle r
            ,entryURL r
            ,entrySummary r
            ,entryBody r
            ,entryMarkdown r
            ,entryPublished r
            ,entryCreatedAt r
            ,entryUpdatedAt r
            )
      else toRow (
            entryTitle r
            ,entryURL r
            ,entrySummary r
            ,entryBody r
            ,entryMarkdown r
            ,entryPublished r
            ,entryUpdatedAt r
            ,entryID r
            )
