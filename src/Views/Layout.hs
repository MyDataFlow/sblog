{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Layout
(
  render
)
where

import Data.Text.Lazy(Text)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

renderInner :: [Html] -> Html
renderInner inner = do
  H.html $ do
    H.body $ do
      mapM_ mapInner inner
  where
    mapInner i = i

render :: [Html] -> Text
render inner = do
  (renderHtml . renderInner) inner
