module Config (
    readOptions
) where

import System.Environment
import System.IO
import System.Exit

import Data.Monoid
import Control.Applicative
import Control.Exception

import Options.Applicative
import qualified Data.Configurator as C 

data DBConf = DBConf {
    dbHost :: String
    ,dbPort :: Int
    ,dbUser :: String
    ,dbPassword :: String
    ,dbDatabase :: String
} deriving (Show, Eq)

data AppConf = AppConf {
    port :: Int
    ,db :: DBConf
} deriving (Show,Eq) 


pathParser :: Parser FilePath
pathParser =
    strArgument $
        metavar "FILENAME" <>
        help "Path to configuration file"

readOptions :: IO FilePath
readOptions = do
        cfgPath <- execParser opts
        putStrLn $  show cfgPath
        conf <- catch
            (C.load [C.Required cfgPath])
            configNotfoundHint
        return cfgPath
    where
        opts = info (helper <*> pathParser)
            ( fullDesc
            <> progDesc "Print a greeting for TARGET")
        configNotfoundHint :: IOError -> IO a
        configNotfoundHint e = do
            hPutStrLn stderr $ "Cannot open config file:\n\t" <> show e
            exitFailure