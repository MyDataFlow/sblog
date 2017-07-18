{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Models.DB.Connections(
  createConnections
  ,runDB
  ,runDBTry
  ,catcher
)where

import Control.Exception as E
import Control.Monad(liftM)
import Control.Monad.Except
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Reader (MonadReader(..),asks)
import System.IO.Error(tryIOError)
import Data.Pool(Pool, withResource, createPool)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors

import Network.HTTP.Types.Status
import qualified Web.Scotty.Trans  as Web

import App.Types
import App.Context


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

runDBTry :: (Connection -> IO b) -> Response b
runDBTry q = do
    conns <- lift (asks dbConns)
    e <- liftIO $ withResource conns $ \c -> catchViolation' catcher . liftIO . liftM Right $  q c
    either Web.raise return e


runDB :: ( MonadTrans m,MonadIO (m App)) => (Connection -> IO b) -> m App b
runDB q = do
  conns <- lift (asks dbConns)
  liftIO  $ withResource conns $ \c -> q c

--catcher :: MonadTrans IO m => SqlError -> ConstraintViolation -> m (Either ServerError a)
catcher e = f
  where
    f c = return . Left $ DBError c

--catchViolation' :: (MonadCatchIO m) => (SqlError -> ConstraintViolation -> m a) -> m a -> m a
catchViolation' f m = catch m (\e -> maybe (throw e) (f e) $ constraintViolation e)
