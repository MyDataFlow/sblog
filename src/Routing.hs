
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

import Handlers.ArticleWriter
import Handlers.Admin.Bookmark



onError :: ServerError -> Response ()
onError err = do
    Web.status $ status err
    Web.text $ message err

routing = do
  Web.defaultHandler onError
  Web.middleware $ logStdoutDev
  Web.middleware $ staticPolicy (noDots >-> addBase "static")
  Web.get "/admin" $ void $ articleWriter
  Web.get "/admin/bookmarks" $ void $ bookmarkIndex
  Web.get "/admin/bookmarks/new" $ void $ bookmarkNew
  Web.get "/admin/bookmarks/:id/editor" $ void $ bookmarkEditor
  Web.post "/admin/bookmarks/create" $ void $ bookmarkCreate
  Web.notFound $ Web.raise RouteNotFound
