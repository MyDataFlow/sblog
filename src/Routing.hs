
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where

import Control.Monad.Trans
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status

import Network.Wai (Middleware)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import App.Types
import App.Context


import qualified Handlers.Sitemap as HS
import qualified Handlers.Rss as HR

import Views.Layout

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
  Web.get "/feed" $ void $ HR.feedR
  Web.get "/rss.xml" $ void $ HR.feedR
  Web.get "/robots.txt" $ void $ HS.robotsR

  Web.notFound $ Web.raise RouteNotFound
