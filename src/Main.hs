{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.IO (BufferMode (..),hSetBuffering,stderr,stdout,stdin)
import Config (readOptions)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin  LineBuffering
    hSetBuffering stderr NoBuffering
    conf <- readOptions
    putStrLn $ show conf
    return ()