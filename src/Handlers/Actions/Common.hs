{-# LANGUAGE OverloadedStrings #-}
module Handlers.Actions.Common(
  api
  ,view
  ,withParams
  ,withGeneratedCSRF
  ,withCSRFVerified
  ,withAuthorization
  ,withUser
  ,preloadUser
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import Data.Maybe

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans(lift)
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Reader(asks)
import Control.Monad.State (get,put,modify)

import Data.Aeson ((.=), object, FromJSON, ToJSON)

import Network.HTTP.Types.Status
import Network.Wai (rawPathInfo,rawQueryString)
import qualified Web.Scotty.Trans as Web

import App.Types

import qualified Utils.Scotty.Auth  as Auth
import qualified Utils.Scotty.CSRF as CSRF
import qualified Utils.Scotty.Cookie as Cookie

import Models.Schemas
import qualified Models.DB as DB

class FromParams a where
  fromParams :: M.Map T.Text T.Text -> Maybe a

data ParamsMap = ParamsMap (M.Map T.Text T.Text)

instance FromParams ParamsMap where
  fromParams m = Just $ ParamsMap m

type Processor request response = request -> Response (Status, response)
type Render response = Response (Status,response) ->  Response response
type Authorized auth request response = auth -> Processor request response

pathAndQuery :: Response ()
pathAndQuery = do
  r <- Web.request
  let path = rawPathInfo r
  let query = rawQueryString r
  let url = C8.concat [path,query]
  lift $ modify $ \s -> s{urlPath = C8.unpack url}

api :: (ToJSON response) => Response (Status, response) -> Response ()
api with = do
  pathAndQuery
  (stat, resp) <- with
  Web.status stat
  Web.json resp

view :: Response (Status,LT.Text) -> Response ()
view with = do
  pathAndQuery
  (stat, resp) <- with
  Web.status stat
  if stat == status302 || stat == status301
    then Web.redirect resp
    else Web.html resp

withJSON :: (FromJSON request, ToJSON response) => (Processor request response) -> Response (Status,response)
withJSON with = do
  req <- Web.jsonData
  with req

withParams :: (FromParams request) => (Processor request response) -> Response (Status,response)
withParams with = do
  paramAssoc <- M.fromList <$> Web.params
  let ps = M.mapKeys LT.toStrict $ LT.toStrict <$> paramAssoc
  case fromParams ps of
    Nothing -> Web.raise $ Exception status400 "Expected request in params"
    Just req -> with req

withGeneratedCSRF :: Processor request response -> Processor request response
withGeneratedCSRF with req = do
    sessionCSRF <- Cookie.getCookie "_csrf_token"
    case sessionCSRF of
      Just token ->  do
        setTplValue "_csrf_token" token
        lift $ modify $ \s -> s{csrfToken = token}
      Nothing -> makeCSRFToken
    with req
  where
    makeCSRFToken = do
      s <- lift (asks site)
      csrf <- liftIO $ CSRF.generateCSRF $ csrfSecret s
      setTplValue "_csrf_token" csrf
      Cookie.setCookie $ Cookie.makeRootSimpleCookie  "_csrf_token" csrf
      lift $ modify $ \s -> s {csrfToken = csrf}

withCSRFVerified :: Processor request response -> Processor request response
withCSRFVerified with req = do
  sessionCSRF <- Cookie.getCookie "_csrf_token"
  token <- Web.rescue (Web.param "_csrf_token")
    (\e -> Web.header "x-csrf-token"  >>= \t -> return $ fromMaybe "" t)
  if (token == "") && (isNothing sessionCSRF )
    then with req
    else case sessionCSRF of
      Nothing ->  Web.raise $ Exception status500 "Expected request in CSRF"
      Just csrf ->
        if csrf == ( LT.toStrict token)
        then with req
        else Web.raise $ Exception status500 "Expected request in CSRF"

authWithHeader :: Response (Maybe T.Text)
authWithHeader = do
  liftIO $ putStrLn "authWithHeader"
  auth <- Web.header "Authorization"
  s <- lift (asks site)
  liftIO $ Auth.headerSecure (jwtSecret s) auth

authWithCookie ::  Response (Maybe T.Text)
authWithCookie = do
  cookie <- Cookie.getCookie "Authorization"
  s <- lift (asks site)
  liftIO $ Auth.cookieSecure (jwtSecret s) cookie

withAuthorization :: Authorized T.Text request response -> Processor request response
withAuthorization with req =
    (runMaybeT
      $ MaybeT authWithHeader
      <|> MaybeT authWithCookie
    ) >>= authAction
  where
    authAction info = do
      case info of
        Nothing -> Web.raise $ Exception status401 "Authorization required"
        Just payload -> with payload req

withUser :: Authorized (Maybe User) request response -> Processor request response
withUser with req = do
    (runMaybeT $ MaybeT authWithHeader <|> MaybeT authWithCookie) >>= authAction
  where
    authAction info = do
      case info of
        Nothing -> with Nothing req
        Just u -> preloadUser u >>= (\user -> with user req)

preloadUser :: T.Text -> Response (Maybe User)
preloadUser u = do
    let userID = read $ T.unpack u
    users <- DB.runDBTry $ DB.retrieveUserByID userID
    setUser users
  where
    setUser [] = return Nothing
    setUser [user] = do
      setTplValue "user" user
      return $ Just user
generateAuthURL :: Response ()
generateAuthURL = do
  me <- lift $ gets urlPath
  let url = show $ updateUrlParam "_r" me (toURI "/auth/login")
  setTplValue (T.pack "auth_url") url
