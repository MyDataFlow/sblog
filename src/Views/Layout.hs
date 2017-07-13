{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Layout(
  render
)where

import Data.Text.Lazy(Text)
import Data.String (fromString)

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Html as EH

import Text.Blaze.Html.Renderer.Text

defaultMeta :: [H.Html]
defaultMeta =
  [
    H.meta ! A.charset "utf-8",
    H.meta ! A.httpEquiv "X-UA-Compatible"
      ! A.content "IE=edge,chrome=1",
    H.meta ! A.name "viewport"
      ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0"
  ]
renderHeader :: String -> [H.Html] -> H.Html
renderHeader title meta =
  H.head $ do
    sequence_ meta
    H.title $ H.toHtml title
    EH.cssLink "https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.css"
    EH.cssLink "/bower_components/editor.md/css/editormd.min.css"
    EH.cssLink "/bower_components/github-markdown-css/github-markdown.css"

renderInner :: String -> [H.Html] -> [H.Html] -> [H.Html] -> H.Html
renderInner title meta sidePart mainPart =
  H.html $ do
    renderHeader title meta
    H.body $ do
      H.div ! A.class_ "ui inverted menu" $ do
        H.div ! A.class_ "ui container" $ do
          H.a ! A.class_ "item" $ "Home"
          H.a ! A.class_ "item" $ "Tags"
      H.div ! A.class_ "ui container" $ do
        H.div ! A.class_ "ui grid" $ do
          H.div ! A.class_ "ten wide computer eleven wide tablet sixteen wide mobile column" $ do
            sequence_ mainPart
          H.div ! A.class_ "four wide computer five wide tablet sixteen wide mobile column" $ do
            sequence_ sidePart
      EH.jsLink "https://cdn.bootcss.com/jquery/3.2.1/jquery.min.js"
      EH.jsLink "https://cdn.jsdelivr.net/semantic-ui/2.2.10/semantic.min.js"
      EH.jsLink "/bower_components/editor.md/editormd.min.js"
      EH.jsLink "/init.js"



render :: String -> [H.Html] -> [H.Html] -> [H.Html] -> Text
render title meta sidePart mainPart =
  renderHtml $ renderInner title combineMeta sidePart mainPart
  where
    combineMeta = defaultMeta ++ meta
