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
import Text.Markdown

import Views.Common.Form
import Utils.BlazeExtra.Pagination as Pagination

import qualified Models.DB.Schema as M
tag :: M.Tag -> H.Html
tag t =
  H.a ! url ! A.class_ "ui tag label" $ do
    H.toHtml $ M.name t
    H.div ! A.class_ "detail" $ H.toHtml $ (M.tid t)
  where
    url = A.href $ fromString $ "/tags/" ++ (show $ M.tid t)

indexArticle :: M.Article -> H.Html
indexArticle ar =
  H.div ! A.class_ "item" $
    H.div ! A.class_ "content" $ do
      H.div ! A.class_ "header" $ H.toHtml title
      H.div ! A.class_ "description" $ H.toHtml summary
      H.div ! A.class_ "extra" $ do
        H.div ! A.class_ "ui right floated primary basic button" $
          H.a ! link $ "阅读全文"
        H.div ! A.class_ "ui tag labels" $
          mapM_ tag tags
  where
    aid = M.aid ar
    title = M.atitle ar
    summary = M.asummary ar
    tags = M.atags ar
    link = A.href $ fromString $ "/articles/" ++  (show $ M.aid ar)

indexBookmark :: M.Bookmark -> H.Html
indexBookmark br =
  H.div ! A.class_ "item" $
    H.div ! A.class_ "content" $ do
      H.div ! A.class_ "header" $ H.toHtml bid
      H.div ! A.class_ "description"  $ markdown def $ fromString summary
      H.div ! A.class_ "extra" $ do
        H.div ! A.class_ "ui right floated primary basic button" $
          H.a ! A.rel "nofollow" ! link $ "原文"
        H.div ! A.class_ "ui tag labels" $
          mapM_ tag tags
      where
        bid = M.bid br
        title = M.btitle br
        summary = M.bsummary br
        tags = M.btags br
        link = A.href $ fromString $ (M.burl br)

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
            H.div ! A.class_ "ui teal secondary segment" $
              H.div ! A.class_ "ui divided items" $ do
                mapM_ indexArticle articles
    renderBookmarks =
      if length bookmarks == 0
        then H.span ""
        else
          H.div ! A.class_ "ui segments" $ do
            H.div ! A.class_ "ui segment" $ H.p $ "书签"
            H.div ! A.class_ "ui olive secondary segment" $
              H.div ! A.class_ "ui divided items" $ do
                mapM_ indexBookmark bookmarks
