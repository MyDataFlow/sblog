
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Trans
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status

import Network.Wai (Middleware,rawPathInfo,rawQueryString)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import App.Types

import Utils.URI.Params
import Utils.URI.String
import Views.Common.Render

import qualified Handlers.Auth.Login as HLogin
import qualified Handlers.Auth.Callback as HCallback
import qualified Handlers.Page.Home as HPHome

onError :: ServerError -> Response ()
onError err = do
  Web.status (status err)
  if (status err) == unauthorized401
    then redirectToAuth
    else if (status err) == status500
      then renderPage  "500.html" "出错了" >>= Web.html
      else renderPage  "404.html" "页面不见了" >>= Web.html
  where
    redirectToAuth = do
      r <- Web.request
      let path = rawPathInfo r
      let query = rawQueryString r
      let destURL = C8.concat [path,query]
      let redirectURL = updateUrlParam "_r" ( C8.unpack destURL)  (toURI "/auth/login")
      Web.redirect $ LT.pack $ show redirectURL

    renderPage tpl title = do
      setTpl tpl
      setTplValue "title" $ T.pack title
      setTplValue "details" $ message err
      render

routing = do
  Web.defaultHandler onError
  Web.middleware $ logStdoutDev
  Web.middleware $ staticPolicy (noDots >-> addBase "static")
  Web.get "/auth/login" $ HLogin.indexR
  Web.get "/auth/callback" $ HCallback.indexR
  Web.get "/home" $ HPHome.indexR
  Web.notFound $ Web.raise RouteNotFound
