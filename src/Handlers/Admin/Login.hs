{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Login(
  indexR
  ,loginR
)where

import Data.Default
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)
import Control.Monad.IO.Class(MonadIO,liftIO)

import Network.HTTP.Types.Status
import qualified Web.Scotty.Trans as Web
import qualified Web.Scotty.Cookie as Cookie

import App.Types
import App.Context
import Utils.URI.String
import Utils.Scotty.Auth

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB
import Utils.BlazeExtra.Pagination as Pagination

import qualified Views.Admin.Login as VAL

data LoginForm = LoginForm {
  password :: T.Text
}

instance FormParams LoginForm where
  fromParams m = LoginForm <$>
    M.lookup "password" m

loginProcessor :: Processor LoginForm LT.Text
loginProcessor req = do
    p <- lift (asks admin)
    if p == T.unpack (password req)
      then doLogin
      else Web.raise $ Exception unauthorized401 "Authorization required"
  where
    doLogin = do
      s <- lift (asks secret)
      cookie <- liftIO $ generateCookie s "admin"
      Cookie.setCookie $
        Cookie.makeSimpleCookie "Authorization" (T.pack cookie)
      return (status302,"/admin")



loginR :: Response LT.Text
loginR = do
  view $ withParams loginProcessor

indexR :: Response LT.Text
indexR = do
  view $ return (status200,VAL.render)
