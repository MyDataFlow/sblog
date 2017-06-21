{-# LANGUAGE OverloadedStrings #-}

module Main 
(
    main
)
where

import Control.Monad
import System.IO (BufferMode (..),hSetBuffering,stderr,stdout,stdin)

import Config (readOptions)
import qualified Models.DB as DB

main :: IO ()
main = do
    --hSetBuffering stdout LineBuffering
    --hSetBuffering stdin  LineBuffering
    --hSetBuffering stderr NoBuffering
    conf <- readOptions
    db <- DB.createConnections conf
    tags <- DB.fetchTags db
    forM_ tags $ putStrLn  
    return ()