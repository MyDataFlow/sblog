{-# LANGUAGE OverloadedStrings #-}
module App.Config (
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
    blogCfg <- C.subconfig "blog" cfg
    let db = DBConf {
        dbHost = C.lookupDefault "host" "127.0.0.1" dbCfg
        ,dbPort = C.lookupDefault "port" 5432 dbCfg
        ,dbUser = C.lookupDefault "user" "postgres" dbCfg
        ,dbPassword = C.lookupDefault "password" "" dbCfg
        ,dbDatabase = C.lookupDefault "database" "postgres" dbCfg
        }
    let server = ServerConf {
        serverPort = C.lookupDefault "port" 3000 serverCfg
        ,serverJWT = C.lookupDefault "jwt" "" serverCfg
        ,serverCSRF = C.lookupDefault  "csrf" "" serverCfg
        }
    let blog = BlogConf {
        blogNme = C.lookupDefault "name" "" blogCfg
        blogHost = C.lookupDefault "host" "" blogCfg
        }
    return AppConf {
        dbConf = db
        ,serverConf = server
        ,blogConf = blog
    }
  where
    opts = info (helper <*> pathParser)
        ( fullDesc <> progDesc "Server of Haskell")
    configNotfoundHint :: IOError -> IO a
    configNotfoundHint e = do
      hPutStrLn stderr $ "Cannot open config file:\n\t" <> show e
      exitFailure
