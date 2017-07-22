{-# LANGUAGE OverloadedStrings #-}

module Views.Index(
  renderMain
)where

import Control.Monad
import Data.Text.Lazy(Text)
import Data.String

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text

import Views.Common.Widgets
import Utils.BlazeExtra.Pagination as Pagination


import qualified Models.DB.Schema as M

renderMain :: [M.Bookmark] -> [M.Article] -> H.Html
renderMain bookmarks articles =
    H.div $ do
      renderArticles
      renderBookmarks
  where
    renderArticles =
      if length articles == 0
        then H.span ""
        else
          H.div ! A.class_ "ui segments" $ do
            H.div ! A.class_ "ui segment" $ H.p $ "文章"
            mapM_ (segmentArticle Nothing) articles

    renderBookmarks =
      if length bookmarks == 0
        then H.span ""
        else
          H.div ! A.class_ "ui segments" $ do
            H.div ! A.class_ "ui segment" $ H.p $ "书签"
            mapM_ segmentBookmark bookmarks
