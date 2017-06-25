{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Tags(
  render
)where

import Data.Text.Lazy(Text)
import Data.String (fromString)

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import qualified Models.Tables as M

render :: [M.Tag] -> H.Html
render tags =
  H.div ! A.class_ "ui segments" $ do
    mapM_ tag tags
  where
    tag t =
      let
        url = "/tags/" ++ (show $ M.tid t)
        l =  A.href $ fromString url
      in
        H.div ! A.class_ "ui segment" $ do
          H.a ! l $ H.toHtml $ M.name t
          H.div ! A.class_ "ui right floated red circular label" $ H.toHtml $ show $ M.count t
