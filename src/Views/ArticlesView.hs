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

import qualified Models.DB as DB

render :: [DB.Article] -> Html
render articles =
  H.div ! A.class_ "eight wide column" $ do
    H.div ! A.class_ "ui items" $ do
      mapM_ article articles
  where
    tag t =
      H.a ! A.class_ "ui tag label" $ H.toHtml t
    article ar =
      let
        aid = DB.aid ar
        url = "/articles/" ++  (show aid) :: String
        l =  A.href $ fromString url
        title = DB.title ar
        summary = DB.summary ar
        tags = DB.tags ar
      in
        H.div ! A.class_ "item" $ do
          H.div ! A.class_ "content" $ do
            H.div ! A.class_ "header" $ H.toHtml title
            H.div ! A.class_ "description" $ H.toHtml summary
            H.div ! A.class_ "extra" $ do
              H.div ! A.class_ "ui tag labels" $ do
                mapM_ tag tags
              H.div ! A.class_ "ui right floated button" $ do
                H.a ! l $ "阅读全文"
