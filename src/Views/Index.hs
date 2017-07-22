{-# LANGUAGE OverloadedStrings #-}

module Views.Index(
  renderIndex
)where

import Control.Monad
import qualified Data.Text.Lazy as LT
import Data.String

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text

import Views.Common.Widgets
import Utils.BlazeExtra.Pagination as Pagination

import qualified Views.Layout as VL

import qualified Models.DB.Schema as M

renderIndex :: String -> [M.Bookmark] -> [M.Article] -> LT.Text
renderIndex name bookmarks articles =
    VL.render 1 title [] [] [(renderMain bookmarks articles)]
  where
    title = "首页-" ++ name
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
