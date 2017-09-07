
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

import qualified Handlers.Sitemap as HS
import qualified Handlers.Rss as HR
import qualified Handlers.Entries.Index as HEIndex
import qualified Handlers.Entries.Show as HEShow
import Views.Types
import Views.Layout

onError :: ServerError -> Response ()
onError err =
  if (status err) == unauthorized401
    then Web.redirect "/admin/login"
    else if (status err) == status500
      then renderError
      else renderNotFound
  where
    renderError = do
      Web.html =<<
        (renderWithTemplate "500.html" () >>= \p -> do
          setTplValue "title" $ T.pack "出错了"
          setTplValue "content" $ LT.toStrict p
          render)
    renderNotFound = do
      Web.html =<<
        (renderWithTemplate "404.html" () >>= \p -> do
          setTplValue "title" $ T.pack "页面不见了"
          setTplValue "content" $ LT.toStrict p
          render)



routing = do
  Web.defaultHandler onError
  Web.middleware $ logStdoutDev
  Web.middleware $ staticPolicy (noDots >-> addBase "static")
  Web.get "/" $ void $ HEIndex.indexR
  Web.get "/entries" $ void $ HEIndex.indexR
  Web.get "/entries/:id/:slug" $ void $ HEShow.indexR
  Web.get "/entries/:id" $ void $ HEShow.indexR
  Web.get "/sitemap.xml" $ void $ HS.sitemapR
  Web.get "/feed" $ void $ HR.feedR
  Web.get "/rss.xml" $ void $ HR.feedR
  Web.get "/robots.txt" $ void $ HS.robotsR

  Web.notFound $ Web.raise RouteNotFound
