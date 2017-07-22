{-# LANGUAGE OverloadedStrings #-}
module App.Types where
import qualified Data.Text.Lazy as T
import Control.Monad.Reader (ReaderT)

import Data.Pool(Pool)
import Database.PostgreSQL.Simple(Connection)

import Network.HTTP.Types.Status
import Web.Scotty.Trans (ScottyT, ActionT, ScottyError(..))
import Database.PostgreSQL.Simple.Errors

data AppConf = AppConf {
    port :: Int
    ,jwtKey :: String
    ,adminPassword :: String
    ,blogHost :: String
    ,blogName :: String
    ,dbHost :: String
    ,dbPort :: Int
    ,dbUser :: String
    ,dbPassword :: String
    ,dbDatabase :: String
} deriving (Show,Eq)

type DBConnections = Pool Connection

data AppContext = AppContext {
  dbConns :: DBConnections
  ,secret :: String
  ,admin :: String
  ,siteHost :: String
  ,siteName :: String
}
type App = ReaderT AppContext IO

data ServerError = RouteNotFound
  | Exception Status T.Text
  | DBError ConstraintViolation

instance ScottyError ServerError where
  showError = message
  stringError = Exception internalServerError500 . T.pack

message :: ServerError -> T.Text
message RouteNotFound = "route not found"
message (Exception s t)
  | s == status500  = T.append "internal server error " t
  | otherwise = t
message (DBError e) = T.pack $ show e

status :: ServerError -> Status
status RouteNotFound = status404
status (Exception s _) = s
status (DBError _) = status500

type Server = ScottyT ServerError App
type Response  = ActionT ServerError App
-- newtype ActionT e m a = ActionT { runAM :: ExceptT (ActionError e)
-- (ReaderT ActionEnv (StateT ScottyResponse m)) a }
--    deriving ( Functor, Applicative, MonadIO )
