{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.TagsView
(
  render
)
where

import Data.Text.Lazy(Text)
import Data.String (fromString)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

render :: [String] -> Html
render tags =
  H.div ! A.class_ "four wide column" $ do
    H.div ! A.class_ "ui list" $ do
      mapM_ tag tags
  where
    tag t =
      let
        url = "/tags/" ++ t :: String
        l =  A.href $ fromString url
      in
        H.a ! A.class_ "item" ! l $ H.toHtml t
