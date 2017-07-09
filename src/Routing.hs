
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Routing(
  routing
)where

import Control.Monad.Trans
import Control.Monad.Reader

import qualified Web.Scotty.Trans  as Web
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import App.Types
import App.Context

import Handlers.Articles
import Handlers.Tags
import Handlers.ArticleWriter

{- secure :: ActionM () -> ActionM ()
secure nex = do
  auth <- S.header "Authorization"
  case auth of
    Nothing -> S.status unauthorized401
    Just token ->
      case BS.break isSpace $ C8.pack $ T.unpack token of
        (strategy,claims)
          | BS.map toLower strategy == "bearer" ->
            next
          | otherwise ->
            S.status unauthorized401
-}
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
