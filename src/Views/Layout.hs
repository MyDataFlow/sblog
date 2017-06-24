{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Layout
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

cssLink :: String -> Html
cssLink ref =
  let
    l = A.href $ fromString ref
  in
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! l

jsLink :: String -> Html
jsLink ref =
  let
    l = A.src $ fromString ref
  in
    H.script ! A.type_ "text/javascript" ! l $ ""

renderMeta :: [Html]
renderMeta =
  [
    H.meta ! A.charset "utf-8",
    H.meta ! A.httpEquiv "X-UA-Compatible"
      ! A.content "IE=edge,chrome=1",
    H.meta ! A.name "viewport"
      ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0"
  ]

renderInner :: Html -> Html -> Html
renderInner tags articles =
  H.html $ do
    sequence_ renderMeta
    H.head $ do
      cssLink "https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.css"
    H.body $ do
      H.div ! A.class_ "ui container" $ do
        H.div ! A.class_ "ui grid" $ do
          articles
          tags
      jsLink "https://cdn.bootcss.com/jquery/3.2.1/jquery.min.js"
      jsLink "https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.js"

render :: Html -> Html -> Text
render tags articles =
  renderHtml $ renderInner tags articles
