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
  H.div $ do
    H.span ! A.id "mode" $
      H.i !  A.class_ "write icon" $ ""
    H.span ! A.id "hinted" $
      H.i ! A.class_ "browser icon" $ ""
    H.span ! A.id "save" $
      H.i ! A.class_ "save icon" $ ""
    H.form ! A.action "/admin/aritcle" ! A.method "POST" $ do
      H.div ! A.id "editor" $ "#This is a test "
