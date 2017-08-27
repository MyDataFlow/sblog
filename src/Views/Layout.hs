{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Views.Layout(
  renderWithTemplate
  ,render
)where
import Control.Monad
import Control.Monad.IO.Class(MonadIO,liftIO)

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Maybe

import qualified Web.Scotty.Trans  as Web

import Utils.Scotty.MustacheRender

import App.Types

renderWithTemplate :: (ToMustache k) => FilePath -> k -> Response LT.Text
renderWithTemplate tpl k = do
  r <- liftIO $ hastache ["templates","templates/partials/"] tpl k
  case r of
    Just t ->  return $ LT.fromStrict t
    Nothing -> Web.raise $ AppError $ LT.pack $ "Can't find template " ++ tpl
render :: (ToMustache k) => k -> Response LT.Text
render k = do
  renderWithTemplate "layout.html" k
