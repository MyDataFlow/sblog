{-# LANGUAGE OverloadedStrings #-}
module Handlers.Auth.Callback(
  indexR
)where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as C8
import Data.Either
import Data.Maybe

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader(..),asks)

import Data.Aeson
import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status
import qualified Network.HTTP.Conduit as Conduit
import Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.AuthorizationRequest as AR
import URI.ByteString

import qualified GitHub
import qualified GitHub.Endpoints.Users as GitHub

import App.Types

import Utils.URI.String
import Utils.URI.Params

import Handlers.Actions.Common
import Handlers.Common
import Views.Common.Render

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
getToken :: String -> OAuth2 -> Conduit.Manager -> IO (OAuth2Result AR.Errors OAuth2Token)
getToken code oauth mgr = do
  let (url, body) = accessTokenUrl oauth $ ExchangeToken $ T.pack code
  doJSONPostRequest mgr oauth url body

getUser (Right token) = do 
  let auth = GitHub.OAuth $ C8.pack $ T.unpack $ atoken $ accessToken token 
  user <- liftIO $ GitHub.userInfoCurrent' auth
  case user of
    Right u -> return $ show u
    Left e -> return $ show e
getUser (Left e) = do return $ show e

indexR :: Response ()
indexR = do
  mgr <- liftIO $ Conduit.newManager Conduit.tlsManagerSettings
  g <-  lift $ asks github
  code <- Web.param "code"
  let oauth = githubKey g
  token <- liftIO $ getToken (LT.unpack code) oauth mgr
  r <- getUser token 
  view $ do return (status200,LT.pack r)