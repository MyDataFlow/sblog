
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

import qualified Handlers.Admin.Bookmark as HAB
import qualified Handlers.Admin.Article as HAA
import qualified Handlers.Admin.Login as HAL
import qualified Handlers.Index as HI
import qualified Handlers.Article as HA
import qualified Handlers.Bookmark as HB
import qualified Handlers.Sitemap as HS

onError :: ServerError -> Response ()
onError err = do
  if (status err) == unauthorized401
    then Web.redirect "/admin/login"
    else if (status err) /= status500
      then Web.redirect "/"
      else do
        Web.status $ status err
        Web.text $ message err

routing = do
  Web.defaultHandler onError
  Web.middleware $ logStdoutDev
  Web.middleware $ staticPolicy (noDots >-> addBase "static")
  Web.get "/" $ void $ HI.indexR
  Web.get "/sitemap.xml" $ void $ HS.sitemapR
  Web.get "/robots.txt" $ void $ HS.robotsR
  Web.get "/bookmarks" $ void $ HB.indexR
  Web.get "/bookmarks/:id" $ void $ HB.showR
  Web.get "/articles" $ void $ HA.indexR
  Web.get "/articles/:id" $ void $ HA.showR
  Web.get "/admin" $ void $ HAB.indexR
  Web.get "/admin/login" $ void $ HAL.indexR
  Web.post "/admin/login" $ void $ HAL.loginR
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
