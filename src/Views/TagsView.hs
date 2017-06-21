{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.TagsView
(
    render
)
where

import Data.Text.Lazy(Text)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

renderTags :: [String] -> Html
renderTags tags = do
    H.html $ do
        H.body $ do
            H.h1 "My Tags"
            H.ul $ do
                mapM_ tag tags
    where 
        tag t = H.li $ H.toHtml t

render :: [String] -> Text
render tags = do
    (renderHtml . renderTags) tags 

