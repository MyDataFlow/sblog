{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module App.Types where
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.Map as M
import Data.Default
import Data.Maybe

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT,runReaderT)
import Control.Monad.State (MonadState(..),StateT,evalStateT,modify)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Except

import qualified Data.Aeson as Aeson
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Types as Mustache

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
}deriving (Show,Eq)

data SiteConf = SiteConf {
  siteName :: String
  ,siteHost :: String
  ,jwtSecret :: String
  ,csrfSecret ::String
} deriving (Show)

data AppConf = AppConf {
  dbConf :: DBConf
  ,serverConf :: ServerConf
  ,siteConf :: SiteConf
} deriving (Show)

type DBConnections = Pool Connection


data AppContext = AppContext {
  dbConns :: DBConnections
  ,site :: SiteConf
}

data AppState = AppState {
  csrfToken :: ST.Text
  ,tplLayout :: FilePath
  ,tplName :: FilePath 
  ,tplCtx :: M.Map ST.Text Mustache.Value
}

instance Default AppState where
  def = AppState "" "layout.html" "" M.empty

newtype App a = App {
  runApp :: ReaderT AppContext (StateT AppState IO) a
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

runAppToIO :: AppContext ->App a -> IO a
runAppToIO ctx app = evalStateT (runReaderT (runApp app) ctx) def


setTplValue :: Mustache.ToMustache v => ST.Text -> v ->  Response ()
setTplValue k v = do 
  lift $ modify $ (\s -> 
      let ctx = tplCtx s in s { tplCtx = M.insert k (Mustache.toMustache v) ctx }
    )
setTplJSONValue :: Aeson.ToJSON v => ST.Text -> v -> Response ()
setTplJSONValue k v = do 
  lift $ modify $ (\s ->  
      let ctx = tplCtx s in  s { tplCtx = M.insert k (Mustache.mFromJSON v) ctx}
    )
setTpl :: FilePath -> Response ()
setTpl tpl = do
  lift $ modify $ (\s -> s { tplName = tpl }
    )
setTplLayout :: FilePath -> Response ()
setTplLayout layout = do 
  lift $ modify $ (\s -> s {tplLayout = layout} )

