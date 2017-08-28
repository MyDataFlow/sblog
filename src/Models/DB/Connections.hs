{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Models.DB.Connections(
  createConnections
  ,runDB
  ,runDBTry
)where

import Control.Exception as E
import Control.Monad(liftM)
import Control.Monad.Except
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Reader (MonadReader(..),asks)

import qualified Data.Text.Lazy as LT

import System.IO.Error(tryIOError)
import Data.Pool(Pool, withResource, createPool)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors

import Network.HTTP.Types.Status
import qualified Web.Scotty.Trans  as Web

import App.Types

createConnections :: AppConf -> IO DBConnections
createConnections cfg = do
    db <- createPool (connect connInfo) close 1 30 10
    return db
  where
    dbCfg = dbConf cfg
    connInfo = ConnectInfo {
        connectHost = dbHost dbCfg
        ,connectPort = fromIntegral (dbPort dbCfg)
        ,connectUser = dbUser dbCfg
        ,connectPassword = dbPassword dbCfg
        ,connectDatabase = dbDatabase dbCfg
    }

-- MonadIO (m ReaderT AppContext IO) -> m ReaderT AppContext IO

runDBTry :: (Connection -> IO b) -> Response b
runDBTry q = do
    -- lift :: App -> ActionT ServerError App
    conns <- lift (asks dbConns)
    -- liftIO :: IO Either ->  ActionT ServerError  Either
    r <- liftIO $ withResource conns $ \c ->
      withTransaction c $ do
        catchViolation' catcher $ liftIO . liftM Right $ q c
    either Web.raise return r

runDB :: ( MonadTrans m,MonadIO (m App)) => (Connection -> IO b) -> m App b
runDB q = do
  conns <- lift (asks dbConns)
  liftIO  $ withResource conns $ \c ->
    withTransaction c $ q c
catcher :: SqlError -> ConstraintViolation -> IO (Either ServerError a)
catcher e = f
  where
    f c = return . Left $ DBError c

catchViolation' :: (SqlError -> ConstraintViolation -> IO (Either ServerError b))
  ->IO (Either ServerError b) -> IO (Either ServerError b)
catchViolation' f m =
    m `E.catches` [E.Handler (\e -> maybe (wrap e) (f e) $ constraintViolation e)
                  ,E.Handler (\e -> wrap (e :: SomeException))]
  where
    wrap e = return $ Left $ Exception status500 $ LT.pack $ show e
