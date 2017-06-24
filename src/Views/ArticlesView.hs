{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.ArticlesView
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
import Text.Markdown

import qualified Models.Tables as M

render :: [M.Article] -> Html
render articles =
  H.div ! A.class_ "ten wide column" $ do
    H.div ! A.class_ "ui items" $ do
      mapM_ article articles
  where
    link aid =
      let
        url = "/articles/" ++  (show aid) :: String
      in
        A.href $ fromString url
    tag t =
      H.a ! A.class_ "ui tag label" $ H.toHtml t
    article ar =
      let
        aid = M.aid ar
        title = M.title ar
        summary = M.summary ar
        tags = M.tags ar
      in
        H.div ! A.class_ "item" $ do
          H.div ! A.class_ "content" $ do
            H.div ! A.class_ "header" $ H.toHtml title
            H.div ! A.class_ "description" $ markdown def $ fromString summary
            H.div ! A.class_ "extra" $ do
              H.div ! A.class_ "ui tag labels" $ do
                mapM_ tag tags
              H.div ! A.class_ "ui right floated button" $ do
                H.a ! (link aid) $ "阅读全文"
