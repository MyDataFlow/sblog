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

render :: [M.Tag] -> Int -> H.Html
render tags active =
  H.div ! A.class_ "ui vertical menu" $ do
    mapM_ tag tags
  where
    tag t =
      let
        url = "/tags/" ++ (show $ M.tid t)
        l =  A.href $ fromString url
        c = if active == M.tid t then "active teal item" else "item"
        cl = if active == M.tid t then "ui teal label" else "ui label"
      in
        H.a ! A.class_ c ! l $ do
          H.toHtml $ M.name t
          H.div ! A.class_ cl $ H.toHtml $ show $ M.count t
