{-# LANGUAGE OverloadedStrings #-}
module  App.Context(
  ServerError(..)
  ,createContext
  ,runApp
) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)

import qualified Data.Text.Lazy as T
import Data.Default

import Web.Scotty.Trans (ScottyT, ActionT, ScottyError(..))

import App.Types

createContext ::DBConnections -> AppConf-> AppContext
createContext conns conf =
  AppContext {
    dbConns = conns
    ,jwtKey = jwtConf $ serverConf conf
    ,csrfKey = csrfConf $ serverConf conf
    ,siteName = blogName $ blogConf conf
    ,siteHost = blogHost $ blogConf conf 
  }

runApp :: AppContext ->App a -> IO a
runApp ctx app = evalStateT (runReaderT (runTheApp app) ctx) def
