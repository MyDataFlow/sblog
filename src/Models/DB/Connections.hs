{-# LANGUAGE OverloadedStrings #-}
module Models.DB.Connections(
  createConnections
  ,runDB
  ,runDBTry
)where

import Control.Exception
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)

import Data.Pool(Pool, withResource, createPool)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import App.Types


createConnections :: AppConf -> IO DBConnections
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

-- MonadIO (m ReaderT AppContext IO) -> m ReaderT AppContext IO
runDB :: (MonadTrans m,MonadIO (m App)) => (Connection -> IO b) -> m App b
runDB q = do
  conns <- lift (asks dbConns)
  liftIO  $ withResource conns $ \c -> q c
runDBTry :: (Exception e, MonadTrans m,MonadIO (m App)) =>
  (Connection -> IO b) -> m App (Either e b)
runDBTry q = do
  conns <- lift (asks dbConns)
  liftIO  $ try $ withResource conns $ \c -> q c
