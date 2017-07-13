{-# LANGUAGE OverloadedStrings #-}

module  App.Context(
  ServerError(..)
  ,status
  ,message
  ,createContext
  ,runApp
) where

import Control.Monad.Reader (runReaderT)
import qualified Data.Text.Lazy as T

import qualified Network.HTTP.Types as Http
import Web.Scotty.Trans (ScottyT, ActionT, ScottyError(..))

import App.Types

instance ScottyError ServerError where
  showError = message
  stringError = Exception Http.internalServerError500 . T.pack

message :: ServerError -> T.Text
message RouteNotFound = "route not found"
message (Exception s t)
  | s == Http.status500  = T.append "internal server error " t
  | otherwise = t

status :: ServerError -> Http.Status
status RouteNotFound = Http.status404
status (Exception s _) = s

createContext ::DBConnections -> String -> AppContext
createContext conns key =
  AppContext {
    dbConns = conns
    ,secret = key
}

runApp :: AppContext ->App a -> IO a
runApp ctx app = runReaderT app ctx
