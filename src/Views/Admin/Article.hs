{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Admin.Article(
  renderWriter
  ,renderIndex
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

renderWriter :: M.Article -> String -> H.Html
renderWriter article url =
    H.div $ do
      H.form ! A.class_ "ui form" ! A.action (H.toValue url) ! A.method "POST" $ do
        idField $ show (M.aid article)
        textField "标题" "title" (M.atitle article)
        textField "摘要" "summary" (M.asummary article)
        contentField (M.abody article)
        H.div ! A.class_ "field" $
          H.div ! A.class_ checked $ do
            H.input  ! A.type_ "checkbox" ! A.name "published" ! A.value "1"
            H.label "发布"

        tagsField $ ts
        H.div ! A.class_ "filed" $ do
          H.button ! A.class_ "ui primary button"  ! A.type_ "submit" $ "保存"
          H.a ! A.class_ "ui  button" ! A.href "/admin/articles" $ "取消"
  where
    ts = map (\tag -> (M.name tag)) (M.atags article)
    checked =
      if M.apublished article
        then "ui checked checkbox"
        else "ui checkbox"


renderIndex :: [M.Article] -> URI -> Pagination ->  H.Html
renderIndex ariticles base pn =
  H.div $ do
    H.table ! A.class_ "ui celled table" $ do
      H.thead $ do
        H.tr $ do
          H.th "id"
          H.th "title"
          H.th "summary"
          H.th "updated_at"
          H.th "action"
      H.tbody $
        mapM_ renderArticle ariticles
      H.tfoot ! A.class_ "full-width" $ H.tr $
        H.th ! A.colspan "5" $ do
          H.div $ H.a ! A.class_ "ui small  positive basic button" ! A.href "/admin/articles/new" $ "新建"
          Pagination.render base pn
    rednerDeleteModal
  where
    renderArticle article =
      H.tr $ do
        H.td $ H.toHtml (M.aid article)
        H.td $ H.toHtml (M.atitle article)
        H.td $ H.toHtml (M.asummary article)
        H.td $ H.toHtml $ show (M.aupdatedAt article)
        H.td $ do
          H.a ! A.class_ "ui primary basic button"
            ! A.href (H.toValue  ("/admin/articles/" ++ (show $ M.aid article) ++ "/edit") ) $ "编辑"
          H.button ! A.id (H.toValue (M.aid article)) ! A.href "/admin/articles/remove/"
            ! A.class_ "ui negative basic button"  $ "删除"
    rednerDeleteModal  =
      H.div ! A.class_ "ui basic modal" $ do
        H.div ! A.class_ "ui icon header" $ do
          H.i ! A.class_ "archive icon" $ ""
          "删除文章"
        H.div ! A.class_ "content" $
          H.p $ "确定要删除该文章吗？"
        H.div ! A.class_ "actions" $ do
          H.div ! A.id "cancel" ! A.class_ "ui red basic cancel inverted button" $ do
            H.i ! A.class_ "remove icon" $ ""
            "否"
          H.div ! A.id "ok" ! A.class_ "ui green ok inverted button" $ do
            H.i ! A.class_ "checkmark icon" $ ""
            "是"
