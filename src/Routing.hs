
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where

import Control.Monad.Trans
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status

import Network.Wai (Middleware)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import App.Types

import Views.Common.Render

import qualified Handlers.Auth.Login as HLogin
import qualified Handlers.Auth.Callback as HCallback

onError :: ServerError -> Response ()
onError err = do 
  Web.status (status err)
  if (status err) == unauthorized401
    then Web.redirect "/admin/login"
    else if (status err) == status500
      then renderPage  "500.html" "出错了" >>= Web.html
      else renderPage  "404.html" "页面不见了" >>= Web.html
  where
    renderPage tpl title = do
      setTpl tpl
      setTplValue "title" $ T.pack title
      render

routing = do
  Web.defaultHandler onError
  Web.middleware $ logStdoutDev
  Web.middleware $ staticPolicy (noDots >-> addBase "static")
  Web.get "/auth/login" $ HLogin.indexR
  Web.get "/auth/callback" $ HCallback.indexR
  Web.notFound $ Web.raise RouteNotFound
