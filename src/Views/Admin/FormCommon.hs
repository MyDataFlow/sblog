{-# LANGUAGE OverloadedStrings #-}

module Views.Admin.FormCommon where

import Control.Monad
import Data.Text.Lazy(Text)
import Data.String (fromString)

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA

textField :: String -> String -> String -> H.Html
textField title name value  = do
  H.div ! A.class_ "field" $ do
    H.label $  H.toHtml title
    H.input ! A.type_ "text" ! A.name (H.toValue name) ! A.value (H.toValue value)

contentField :: String -> H.Html
contentField content = do
  H.div ! A.class_ "field" $ do
    H.div ! A.class_ "ui container" $ do
      H.div ! A.id "editor" ! A.name "content" $  H.toHtml content

tagsField :: String -> H.Html
tagsField tags = do
  H.div ! A.class_ "filed" $ do
    H.div ! A.class_ "ui  multiple selection search dropdown"  ! A.id "tags" $ do
      H.input ! A.type_ "hidden" ! A.name "tags" ! A.value (H.toValue tags)
      H.div ! A.class_ "default text" $ "Tags"
