
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where

import Control.Monad.Trans
import Control.Monad.Reader

import qualified Web.Scotty.Trans  as Web

import Network.Wai (Middleware)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import App.Types
import App.Context


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
  Web.middleware $ staticPolicy (noDots >-> addBase "static")
  Web.get "/" $ void $ articlesIndex
  Web.get "/tags/:id" $ void $ tagsIndex
  Web.get "/admin" $ void $ articleWriter
  Web.get "/admin/ariticle" $ do
    t <- Web.param "test"
    Web.text t
  Web.notFound $ Web.raise RouteNotFound
