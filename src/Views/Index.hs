{-# LANGUAGE OverloadedStrings #-}

module Views.Index(
  renderMain
)where

import Control.Monad
import Data.Text.Lazy(Text)
import Data.String

import Network.URI

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text

import Views.Common.Form
import Utils.BlazeExtra.Pagination as Pagination

import qualified Models.DB.Schema as M
tag :: M.Tag -> H.Html
tag t =
  H.a ! url ! A.class_ "ui tag label" $ do
    H.toHtml $ M.tagName t
    H.div ! A.class_ "detail" $ H.toHtml $ (M.tagCount t)
  where
    url = A.href $ fromString $ "/tags/" ++ (show $ M.tagID t)

indexArticle :: M.Article -> H.Html
indexArticle ar =
    H.div ! A.class_ "ui teal secondary segment" $
      H.div ! A.class_ "item" $
        H.div ! A.class_ "content" $ do
          H.div ! A.class_ "ui small right floated primary basic button" $
            H.a !  link $ "阅读原文"
          H.div ! A.class_ "header" $
            H.p $ H.toHtml title
          H.div ! A.class_ "description" $
            H.p $ H.toHtml summary
          H.div ! A.class_ "extra" $ do
            H.div ! A.class_ "ui  tag labels" $
              if length tags == 0
                then H.span ""
                else mapM_ tag tags

  where
    aid = M.articleID ar
    title = M.articleTitle ar
    summary = M.articleSummary ar
    tags = M.articleTags ar
    link = A.href $ fromString $ "/articles/" ++  (show $ M.articleID ar)

indexBookmark :: M.Bookmark -> H.Html
indexBookmark br =
    H.div ! A.class_ "ui olive secondary segment" $
      H.div ! A.class_ "item" $
        H.div ! A.class_ "content" $ do
          H.div ! A.class_ "ui small right floated primary basic button" $
            H.a ! A.rel "nofollow" ! link $ "原文"
          H.div ! A.class_ "header" $
            H.p $ H.toHtml title
          H.div ! A.class_ "description" $
            H.preEscapedToHtml summary
          H.div ! A.class_ "extra" $ do
            H.div ! A.class_ "ui tag labels" $
              mapM_ tag tags


  where
    bid = M.bookmarkID br
    title = M.bookmarkTitle br
    summary = M.bookmarkSummary br
    tags = M.bookmarkTags br
    link = A.href $ fromString $ (M.bookmarkUrl br)

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
            mapM_ indexArticle articles

    renderBookmarks =
      if length bookmarks == 0
        then H.span ""
        else
          H.div ! A.class_ "ui segments" $ do
            H.div ! A.class_ "ui segment" $ H.p $ "书签"
            mapM_ indexBookmark bookmarks
