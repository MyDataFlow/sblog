{-# LANGUAGE OverloadedStrings #-}

module Main
(
    main
)
where

import Control.Monad
import System.IO (BufferMode (..),hSetBuffering,stderr,stdout,stdin)


import Web.Scotty as S

import Config
import qualified Models.DB as DB
import qualified Routing as R

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin  LineBuffering
    hSetBuffering stderr NoBuffering
    conf <- readOptions
    db <- DB.createConnections conf
    S.scotty (port conf) $ R.routing db
    return ()
