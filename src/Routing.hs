
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where

import Control.Monad.Trans
import Control.Monad.Reader

import qualified Web.Scotty.Trans  as Web
import qualified Network.HTTP.Types as Http
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import App.Types
import App.Context

import qualified Utils.Scotty.Auth  as Auth
import Handlers.Articles
import Handlers.Tags
import Handlers.ArticleWriter



onError :: ServerError -> Response ()
onError err = do
    Web.status $ status err
    Web.text $ message err

routing = do
  Web.defaultHandler onError
  Web.middleware $ logStdoutDev
  Web.get "/" $ void $ articlesIndex
  Web.get "/tags/:id" $ void $ tagsIndex
  Web.get "/admin" $ void $ articleWriter
  Web.notFound $ Web.raise RouteNotFound
