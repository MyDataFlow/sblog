{-# LANGUAGE OverloadedStrings #-}
module Handlers.Auth.Login(
  indexR
)where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as C8
import Data.Either
import Data.Maybe

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)

import Network.HTTP.Types.Status
import Network.OAuth.OAuth2
import URI.ByteString

import App.Types

import Utils.URI.String
import Utils.URI.Params

import Handlers.Actions.Common
import Handlers.Common

githubKey ::  GithubConf -> OAuth2
githubKey g =
  let 
    callback = eitherToMaybe $  parseURI strictURIParserOptions "http://127.0.0.1:8080/auth/callback"
    authEndpoint = fromJust $ eitherToMaybe $ parseURI strictURIParserOptions "https://github.com/login/oauth/authorize"
    tokenEndpoint = fromJust $  eitherToMaybe $ parseURI strictURIParserOptions "https://github.com/login/oauth/access_token"
  in
    OAuth2 { oauthClientId = T.pack $  githubClientID g
            , oauthClientSecret = T.pack $ githubClientSecret g
            , oauthCallback = callback
            , oauthOAuthorizeEndpoint = authEndpoint 
            , oauthAccessTokenEndpoint =  tokenEndpoint 
            }

indexR :: Response ()
indexR = do
  g <-  lift $ asks github
  let oauth = githubKey g
  let url = LT.pack $ C8.unpack $ serializeURIRef' $ authorizationUrl oauth
  view $ do return (status302,url)