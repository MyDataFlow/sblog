{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.ArticlesView
(
  render
)
where

import Data.Text.Lazy(Text)
import Data.String (fromString)

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Text.Markdown

import qualified Models.Tables as M


tag :: M.Tag -> H.Html
tag t =
  let
    url = A.href $ fromString $ "/tags/" ++ (show $ M.tid t)
  in
    H.a ! url ! A.class_ "ui tag label" $ H.toHtml $ M.name t

article :: M.Article -> H.Html
article ar =
  H.div ! A.class_ "item" $ do
    H.div ! A.class_ "content" $ do
      H.div ! A.class_ "header" $ H.toHtml title
      H.div ! A.class_ "description" $ markdown def $ fromString summary
      H.div ! A.class_ "extra" $ do
        H.div ! A.class_ "ui tag labels" $ do
          mapM_ tag tags
        H.div ! A.class_ "ui right floated button" $ do
          H.a ! (link aid) $ "阅读全文"
  where
    aid = M.aid ar
    title = M.title ar
    summary = M.summary ar
    tags = M.tags ar
    link aid =
      let
        url = "/articles/" ++  (show aid) :: String
      in
        A.href $ fromString url

render :: [M.Article] -> H.Html
render articles =
  H.div ! A.class_ "ui items" $ do
    mapM_ article articles
