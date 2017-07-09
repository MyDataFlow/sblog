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
  H.div ! A.class_ "row" $
    H.div ! A.class_ "col-md-12" $ do
      H.h2 "Write a Post"
      H.form $ do
        H.div ! A.class_ "form-group" $ do
          H.label ! A.for "post-title" $ "Title"
          H.input ! A.type_ "text" ! A.class_ "form-control" ! A.id "post-title"
        H.div ! A.class_ "form-group" $ do
          H.label ! A.for "post-body" $ "Body"
          H.textarea ! A.class_ "form-control" ! A.id "post-body" ! A.rows "15" $ ""
        H.div ! A.class_ "form-group" $ do
          H.label ! A.for "post-tags" $ "Tags"
          H.input ! A.type_ "text" ! A.class_ "form-control" ! A.id "post-tags"
        H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Save"
