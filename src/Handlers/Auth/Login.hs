{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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

import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status
import Network.OAuth.OAuth2
import URI.ByteString
import qualified Network.URI as URI

import App.Types

import Utils.URI.String
import Utils.URI.Params

import Handlers.Actions.Common
import Handlers.Common


import Models.Schemas
import qualified Models.DB as DB


githubKey ::  URI.URI -> GithubConf -> OAuth2
githubKey url g =
  let
    callback = eitherToMaybe $ parseURI strictURIParserOptions $ C8.pack $ show $ url
    authEndpoint = fromJust $ eitherToMaybe $ parseURI strictURIParserOptions "https://github.com/login/oauth/authorize"
    tokenEndpoint = fromJust $  eitherToMaybe $ parseURI strictURIParserOptions "https://github.com/login/oauth/access_token"
  in
    OAuth2 { oauthClientId = T.pack $  githubClientID g
            , oauthClientSecret = T.pack $ githubClientSecret g
            , oauthCallback = callback
            , oauthOAuthorizeEndpoint = authEndpoint
            , oauthAccessTokenEndpoint =  tokenEndpoint
            }
indexProcessor req = do
  g <- lift $ asks github
  s <- lift $ asks site
  let callbackURL = updateUrlParam "state" (LT.unpack req)
        $ URI.relativeTo (toURI "/auth/callback") (toURI $ siteHost s)
  let oauth = githubKey callbackURL g
  let authURL = LT.pack $ C8.unpack $ serializeURIRef' $ authorizationUrl oauth
  return (status302,authURL)

authUser req = do
  Web.rescue  (withAuthorization toRoot req) catcher
  where
    catcher (Exception status401 _ ) = indexProcessor req
    catcher e = Web.raise e
    toRoot u r = do
      let userID = read $ T.unpack u
      users <- DB.runDBTry $ DB.retrieveUserByID userID
      if length users == 0
        then indexProcessor req
        else return (status302,req)

indexR :: Response ()
indexR = do
  req <-  Web.param "_r" `Web.rescue` (\e -> return "/")
  view $ authUser req
