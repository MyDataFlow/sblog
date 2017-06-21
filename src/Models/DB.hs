{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Models.DB (
    createConnections,
    fetchTags
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

type PoolT = Pool Connection

newtype Tag = Tag String deriving (Show)

deriving instance FromField Tag
deriving instance ToField Tag

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

fetchTags :: PoolT -> IO [String]
fetchTags p = withResource p $ \c -> do
        let q = "SELECT name FROM tags" :: Query
        rs  <- query_ c q
        let tags = map fromOnly rs
        return tags