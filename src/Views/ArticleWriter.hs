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
    H.form ! A.class_ "ui form" ! A.action "/admin/ariticle" ! A.method "GET" $ do
      H.div ! A.class_ "filed" $ do
        H.div ! A.class_ "ui  multiple selection search dropdown"  ! A.id "test" $ do
          H.input ! A.type_ "hidden" ! A.name "test"
          H.i ! A.class_ "dropdown icon" $ ""
          H.div ! A.class_ "default text" $ "select"
          H.div ! A.class_ "menu" $ do
            H.div ! A.class_ "item"  ! H.dataAttribute "value" "A" $ "A"
            H.div ! A.class_ "item"  ! H.dataAttribute "value" "B" $ "B"
            H.div ! A.class_ "item"  ! H.dataAttribute "value" "C" $ "C"
            H.div ! A.class_ "item"  ! H.dataAttribute "value" "D" $ "D"
            H.div ! A.class_ "item"  ! H.dataAttribute "value" "E" $ "E"
      H.div ! A.class_ "filed" $ do
        H.button ! A.type_ "submit" $ "Submit"


    H.div ! A.id "editor" $ ""
