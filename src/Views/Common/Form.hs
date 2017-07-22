{-# LANGUAGE OverloadedStrings #-}

module Views.Common.Form where

import Control.Monad
import Data.Text.Lazy(Text)
import Data.String (fromString)

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA

idField :: String -> H.Html
idField value =
  H.div ! A.class_ "field" $ do
    H.input ! A.class_ "hidden" ! A.type_ "hidden" ! A.name "id" ! A.value (H.toValue value)
textField :: String -> String -> String -> H.Html
textField title name value  =
  H.div ! A.class_ "field" $ do
    H.label $  H.toHtml title
    H.input ! A.type_ "text" ! A.name (H.toValue name) ! A.value (H.toValue value)

contentField :: String -> H.Html
contentField content =
  H.div ! A.class_ "field" $ do
    H.div ! A.id "editorContent" ! A.hidden "true" $ H.toHtml content
    H.div ! A.class_ "ui container" $
      H.div ! A.id "editor" ! A.name "content" $ ""

tagsField :: String -> H.Html
tagsField ts =
  H.div ! A.class_ "filed" $
    H.div ! A.class_ "ui  multiple selection search dropdown"  ! A.id "tags" $ do
      H.input ! A.type_ "hidden" ! A.name "tags" ! A.value (H.toValue ts)
      H.div ! A.class_ "default text" $ "Tags"
