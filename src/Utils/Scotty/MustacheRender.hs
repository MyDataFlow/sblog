{-# LANGUAGE OverloadedStrings #-}
module Utils.Scotty.MustacheRender(
  module Text.Mustache
  ,hastache
) where
import Data.Text as T
import Data.Maybe
import Text.Mustache

hastache :: (ToMustache k) => [FilePath] ->
  FilePath -> k -> IO (Maybe T.Text)
hastache searchSpace tpl ctx = do
  eitherTemplate <- automaticCompile searchSpace tpl
  case eitherTemplate of
    Left err -> return $ Nothing
    Right compiledTemplate ->
      return $ Just $ substitute compiledTemplate ctx
