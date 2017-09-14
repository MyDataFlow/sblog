{-# LANGUAGE OverloadedStrings #-}
module Handlers.Auth.Callback(
  indexR
)where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as C8
import Data.Either
import Data.Maybe
import Data.Time
import Data.Default
import Data.Aeson

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader(..),asks)


import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status
import qualified Network.HTTP.Conduit as Conduit
import Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.AuthorizationRequest as AR
import URI.ByteString
import qualified Network.URI as URI

import qualified GitHub
import qualified GitHub.Endpoints.Users as GitHub
import qualified GitHub.Data.Id as GitHub
import qualified GitHub.Data.URL as GitHub

import App.Types

import Utils.URI.String
import Utils.URI.Params
import Utils.Extra.EitherMaybe

import qualified Utils.Scotty.Auth as Auth
import qualified Utils.Scotty.Cookie as Cookie

import Handlers.Actions.Common

import Models.Schemas
import qualified Models.DB as DB

import Views.Common.Render

githubKey :: URI.URI ->  GithubConf -> OAuth2
githubKey url g =
  let
    callback = eitherToMaybe $  parseURI strictURIParserOptions $ C8.pack $ show url
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
    Right u -> return u
    Left e -> Web.raise $ Exception status500  $ LT.pack $ show e
getUser (Left e) = do Web.raise $ Exception status500 $ LT.pack $ show e


login user req = do
    users <- DB.runDBTry $ DB.retrieveUserByUID $ fromInteger uid
    u <- if length users == 0 then newUser else mayUpdate $ head users
    s <- lift $ asks site
    cookie <- liftIO $ Auth.generateCookie $
      def { Auth.jwtSecret = T.pack $ jwtSecret s
          , Auth.jwtPayload = T.pack $ show $ userID u}
    Cookie.setCookie $ Cookie.makeRootSimpleCookie "Authorization"  cookie
    return (status302,req)
  where
    mayUpdate du = do
      now <- liftIO $ localTimeNow
      let [(r,nu)] = [(False,du)]
            >>= (\(o,u) -> do
              if userName u /= name
                then return $ (True,u {userName = name})
                else return (o,u)
            ) >>= (\(o,u) -> do
              if userAvatar u /= avatar
                then return $(True, u {userAvatar = avatar})
                else return (o,u))
      when r $ void $ DB.runDBTry $ DB.updateUser $ nu {userUpdatedAt = now}
      return nu

    newUser = do
      now <- liftIO $ localTimeNow
      let u = def {userUID = fromInteger uid
                  ,userName = name
                  ,userEmail = email
                  ,userAvatar = avatar
                  ,userCreatedAt = now
                  ,userUpdatedAt = now
                  }
      liftIO $ putStrLn $ show u
      newID <- DB.runDBTry $ DB.createUser u
      return $ u {userID = newID}
    uid =  toInteger $ GitHub.untagId $ GitHub.userId user
    name =
      case GitHub.userName user of
        Nothing -> T.append (T.pack "github-") $ T.pack $ show uid
        Just n -> n
    email =
      case GitHub.userEmail user of
        Nothing -> ""
        Just i -> i
    avatar =  GitHub.getUrl $ GitHub.userAvatarUrl user

indexR :: Response ()
indexR = do
  code <- Web.param "code"
  req <-  Web.param "state" `Web.rescue` (\e -> return "/")
  g <-  lift $ asks github
  s <- lift $ asks site
  mgr <- liftIO $ Conduit.newManager Conduit.tlsManagerSettings
  let callbackURL = updateUrlParam "state" (LT.unpack req)
        $ URI.relativeTo (toURI "/auth/callback") (toURI $ siteHost s)
  let oauth = githubKey callbackURL g
  token <- liftIO $ getToken (LT.unpack code) oauth mgr
  user <- getUser token
  view $ login user req
