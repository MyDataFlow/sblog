{-# LANGUAGE OverloadedStrings #-}

module Main(
  main
)where

import Control.Monad.Trans (lift)

import System.IO (BufferMode (..),hSetBuffering,stderr,stdout,stdin)


import qualified Web.Scotty.Trans  as Web
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import App.Types
import App.Config
import App.Context

import qualified Models.DB as DB
import qualified Routing as R

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin  LineBuffering
    hSetBuffering stderr NoBuffering
    conf <- readOptions
    conns <- DB.createConnections conf
    let ctx = createContext conns (jwtKey conf) (adminPassword conf)
    Web.scottyT (port conf) (runApp ctx) R.routing
    return ()
