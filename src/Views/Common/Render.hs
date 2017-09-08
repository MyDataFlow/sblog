{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Views.Common.Render(
  render
)where
import Control.Monad
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.State (MonadState(..),StateT,gets)
import Control.Monad.Trans.Class (MonadTrans, lift)

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Maybe

import qualified Web.Scotty.Trans  as Web

import Utils.Scotty.MustacheRender

import App.Types

renderWithTemplate :: FilePath -> Response LT.Text
renderWithTemplate tpl  = do
  ctx <- lift $ gets tplCtx
  r <- liftIO $ hastache ["templates","templates/partials/"] tpl ctx
  case r of
    Just t ->  return $ LT.fromStrict t
    Nothing -> Web.raise $ AppError $ LT.pack $ "Can't find template " ++ tpl

render :: Response LT.Text
render = do
  layout <- lift $ gets tplLayout
  tpl <- lift $ gets tplName
  content <- renderWithTemplate tpl
  setTplValue "content" content
  renderWithTemplate layout
