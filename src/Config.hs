{-# LANGUAGE OverloadedStrings #-}
module Config (
  AppConf(..)
  ,readOptions
) where

import System.Environment
import System.IO
import System.Exit

import Data.Monoid
import Control.Applicative
import Control.Exception
import Control.Monad

import Options.Applicative
import qualified Data.Configurator as C
import qualified Data.Configurator.Parser as C

data AppConf = AppConf {
    port :: Int
    ,dbHost :: String
    ,dbPort :: Int
    ,dbUser :: String
    ,dbPassword :: String
    ,dbDatabase :: String
} deriving (Show,Eq)


pathParser :: Parser FilePath
pathParser =
    strArgument $
        metavar "FILENAME" <>
        help "Path to configuration file"

readOptions :: IO AppConf
readOptions = do
    cfgPath <- execParser opts
    putStrLn $  show cfgPath
    conf <- catch
        (C.readConfig =<< C.load [C.Required cfgPath])
        configNotfoundHint
    let (mAppConf, errs) = flip C.runParserA conf $
            AppConf <$> C.key "port"
            <*> C.key "dbHost"
            <*> C.key "dbPort"
            <*> C.key "dbUser"
            <*> C.key "dbPassword"
            <*> C.key "dbDatabase"

    case mAppConf of
        Nothing -> do
            forM_ errs $ hPrint stderr
            exitFailure
        Just appConf ->
            return appConf
    where
        opts = info (helper <*> pathParser)
            ( fullDesc
            <> progDesc "Print a greeting for TARGET")
        configNotfoundHint :: IOError -> IO a
        configNotfoundHint e = do
            hPutStrLn stderr $ "Cannot open config file:\n\t" <> show e
            exitFailure
