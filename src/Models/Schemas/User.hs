{-# LANGUAGE OverloadedStrings #-}
module Models.Schemas.User where

import Data.Int
import Data.Default
import Data.Time
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Models.Schemas.Common

data User = User {
  userID :: Int64
  ,userUID :: Int64
  ,userName :: T.Text
  ,userEmail :: T.Text
  ,userAvatar :: T.Text
  ,userCreatedAt :: LocalTime
  ,userUpdatedAt :: LocalTime
} deriving (Show,Eq)


instance Default User where
  def = User { userID = 0
              , userUID = 0
              , userName = ""
              , userEmail = ""
              , userAvatar = ""
              , userCreatedAt = defaultDate
              , userUpdatedAt = defaultDate
              }

instance FromRow User where
  fromRow = User  <$> field <*> field <*> field <*> field
                      <*> field <*> field <*> field 
                  
instance ToRow User where
  toRow r = 
      if (userID r) == 0
        then toRow (userUID r
                    ,userName r
                    ,userEmail r
                    ,userAvatar r
                    ,userCreatedAt r
                    ,userUpdatedAt r)
        else toRow (userName r
                    ,userEmail r
                    ,userAvatar r
                    ,userUpdatedAt r
                    ,userID r)