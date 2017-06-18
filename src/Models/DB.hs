module Models.DB (

)where

import Control.Applicative
import Control.Monad(when)
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
import System.Random(randomRIO)

type PoolT = Pool Connection

createPool :: DBConf -> IO PoolT
createPool cfg = do
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
