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

import Web.Scotty.Trans (ScottyT, ActionT, ScottyError(..))

import App.Types

createContext ::DBConnections -> String -> String -> AppContext
createContext conns key adminPassword =
  AppContext {
    dbConns = conns
    ,secret = key
    ,admin = adminPassword
}

runApp :: AppContext ->App a -> IO a
runApp ctx app = runReaderT app ctx
