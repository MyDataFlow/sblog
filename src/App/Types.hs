{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module App.Types where
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import Data.Default
import Data.Maybe

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.State (MonadState(..),StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Except

import Text.Mustache

import Data.Pool(Pool)
import Database.PostgreSQL.Simple(Connection)

import Network.HTTP.Types.Status
import Web.Scotty.Trans (ScottyT, ActionT, ScottyError(..))
import Database.PostgreSQL.Simple.Errors

data DBConf = DBConf {
  dbHost :: String
  ,dbPort :: Int
  ,dbUser :: String
  ,dbPassword :: String
  ,dbDatabase :: String
} deriving (Show,Eq)

data ServerConf = ServerConf {
  serverPort :: Int
  ,serverJWT :: String
  ,serverCSRF ::String
}deriving (Show,Eq)

data BlogConf = BlogConf {
  blogName :: String
  ,blogHost :: String
  ,blogPassword :: String
} deriving (Show)

data AppConf = AppConf {
  dbConf :: DBConf
  ,serverConf :: ServerConf
  ,blogConf :: BlogConf
} deriving (Show)

type DBConnections = Pool Connection

data AppContext = AppContext {
  dbConns :: DBConnections
  ,jwtKey :: String
  ,csrfKey :: String
  ,siteName :: String
  ,siteHost :: String
  ,sitePassword :: String
}
data AppState = AppState {
  csrfToken :: ST.Text
  ,layout :: FilePath
}

instance Default AppState where
  def = AppState "" "layout.html"

newtype App a = App {
  runTheApp :: ReaderT AppContext (StateT AppState IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext,MonadState AppState )

data ServerError = RouteNotFound
  | Exception Status LT.Text
  | DBError ConstraintViolation
  | AppError LT.Text

instance ScottyError ServerError where
  showError = message
  stringError = Exception internalServerError500 . LT.pack

message :: ServerError -> LT.Text
message RouteNotFound = "route not found"
message (Exception s t)
  | s == status500  = LT.append "internal server error " t
  | otherwise = t
message (DBError e) = LT.pack $ show e
message (AppError e) = e

status :: ServerError -> Status
status RouteNotFound = status404
status (Exception s _) = s
status (DBError _) = status500
status (AppError _) = status500

type Server  = ScottyT ServerError App
type Response   = ActionT ServerError App
-- newtype ActionT e m a = ActionT { runAM :: ExceptT (ActionError e)
-- (ReaderT ActionEnv (StateT ScottyResponse m)) a }
--    deriving ( Functor, Applicative, MonadIO )
