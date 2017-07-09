{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.ArticleWriter(
  render
)where

import Control.Monad
import Data.Text.Lazy(Text)
import Data.String (fromString)

import Network.URI

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text
import Text.Markdown

import qualified Models.DB.Schema as M

render :: H.Html
render =
  H.form ! A.class_ "ui form" ! A.method "POST" ! A.action "/admin/article" $ do
    H.div ! A.class_ "field" $ do
      H.label ! A.for "post-title" $ "Title"
      H.input ! A.type_ "text" ! A.id "post-title" ! A.name "post-title"
    H.div ! A.class_ "field" $ do
      H.label ! A.for "post-body" $ "Body"
      H.textarea ! A.class_ "form-control" ! A.id "post-body" ! A.rows "15" $ ""
    H.div ! A.class_ "field" $ do
      H.label ! A.for "post-tags" $ "Tags"
      H.input ! A.type_ "text" ! A.class_ "form-control" ! A.id "post-tags"
    H.button ! A.type_ "submit" ! A.class_ "ui button" $ "Save"
