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
import Config

import qualified Routing as R
import qualified Models.DB as DB

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering
  conf <- readOptions
  conns <- DB.createConnections conf
  let ctx = AppContext {
    dbConns = conns
    ,site = siteConf conf
    ,github = githubConf conf
  }
  Web.scottyT (serverPort $ serverConf  conf) (runAppToIO ctx) R.routing
  return ()
