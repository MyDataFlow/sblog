
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where

import Control.Monad.Trans
import Control.Monad.Reader

import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status

import Network.Wai (Middleware)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import App.Types
import App.Context

import Handlers.ArticleWriter
import qualified Handlers.Admin.Bookmark as HAB
import qualified Handlers.Admin.Article as HAA
import qualified Handlers.Index as HI



onError :: ServerError -> Response ()
onError err = do
  if (status err) == unauthorized401
    then Web.redirect "/admin/login"
    else do
      Web.status $ status err
      Web.text $ message err

routing = do
  Web.defaultHandler onError
  Web.middleware $ logStdoutDev
  Web.middleware $ staticPolicy (noDots >-> addBase "static")
  Web.get "/" $ void $ HI.indexR
  Web.get "/admin" $ void $ HAB.indexR
  Web.get "/admin/bookmarks" $ void $ HAB.indexR
  Web.get "/admin/bookmarks/new" $ void $ HAB.newR
  Web.get "/admin/bookmarks/:id/edit" $ void $ HAB.editR
  Web.post "/admin/bookmarks/create" $ void $ HAB.createR
  Web.post "/admin/bookmarks/remove" $ void $ HAB.removeR
  Web.get "/admin/articles" $ void $ HAA.indexR
  Web.get "/admin/articles/new" $ void $ HAA.newR
  Web.get "/admin/articles/:id/edit" $ void $ HAA.editR
  Web.post "/admin/articles/create" $ void $ HAA.createR
  Web.post "/admin/articles/remove" $ void $ HAA.removeR
  Web.notFound $ Web.raise RouteNotFound
