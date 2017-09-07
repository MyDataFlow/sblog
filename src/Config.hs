{-# LANGUAGE OverloadedStrings #-}
module Config (
  readOptions
) where

import System.Environment
import System.IO
import System.Exit

import Data.Monoid
import Control.Applicative
import Control.Exception
import Control.Monad

import Options.Applicative
import qualified Data.Yaml.Config as C

import App.Types
import qualified Models.DB as DB


pathParser :: Parser FilePath
pathParser =
    strArgument $
        metavar "FILENAME" <>
        help "Path to configuration file"

readOptions :: IO AppConf
readOptions = do
    cfgPath <- execParser opts
    cfg <- catch (C.load cfgPath) configNotfoundHint
    serverCfg <-  C.subconfig "server" cfg
    dbCfg <- C.subconfig "db" cfg
    siteCfg <- C.subconfig "site" cfg
    let db = DBConf {
          dbHost = C.lookupDefault "host" "127.0.0.1" dbCfg
          ,dbPort = C.lookupDefault "port" 5432 dbCfg
          ,dbUser = C.lookupDefault "user" "postgres" dbCfg
          ,dbPassword = C.lookupDefault "password" "" dbCfg
          ,dbDatabase = C.lookupDefault "database" "postgres" dbCfg
        }
    let server = ServerConf {
          serverPort = C.lookupDefault "port" 3000 serverCfg
        }
    let site = SiteConf {
          siteName = C.lookupDefault "name" "" siteCfg
          ,siteHost = C.lookupDefault "host" "" siteCfg
          ,jwtSecret = C.lookupDefault "jwt" "" siteCfg
          ,csrfSecret = C.lookupDefault  "csrf" "" siteCfg
        }
    return AppConf {
        dbConf = db
        ,serverConf = server
        ,siteConf = site
    }
  where
    opts = info (helper <*> pathParser)
        ( fullDesc <> progDesc "Server of Haskell")
    configNotfoundHint :: IOError -> IO a
    configNotfoundHint e = do
      hPutStrLn stderr $ "Cannot open config file:\n\t" <> show e
      exitFailure
