{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Admin.Article(
  renderWriter
  ,renderIndex
)where

import Control.Monad
import qualified Data.Text.Lazy as LT
import Data.String

import Network.URI

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text
import Text.Markdown

import Views.Common.Form
import Views.Common.SEO
import Utils.BlazeExtra.Pagination as Pagination

import qualified Views.Admin.Layout as VL

import qualified Models.DB.Schema as M

renderWriter :: M.Article -> String -> LT.Text
renderWriter article url =
    VL.render 2
      ["/bower_components/editor.md/css/editormd.min.css"]
      ["/bower_components/editor.md/editormd.min.js"
      ,"/assets/admin/editor.js"]
      [renderForm]
  where
    checked =
      if M.articlePublished article
        then  A.checked "true"
        else  A.checked "false"
    renderForm =
      H.form ! A.class_ "ui form" ! A.action (H.toValue url) ! A.method "POST" $ do
        idField $ show (M.articleID article)
        textField "标题" "title" (M.articleTitle article)
        textField "摘要" "summary" (M.articleSummary article)
        contentField (M.articleMarkdown article)
        H.div ! A.class_ "field" $
          H.div ! A.class_ "ui checkbox" $ do
            H.input ! checked ! A.type_ "checkbox" ! A.name "published" ! A.value "1"
            H.label "发布"
        tagsField $ showTags (M.articleTags article)
        H.div ! A.class_ "filed" $ do
          H.button ! A.class_ "ui primary button"  ! A.type_ "submit" $ "保存"
          H.a ! A.class_ "ui  button" ! A.href "/admin/articles" $ "取消"

renderIndex :: [M.Article] -> URI -> Pagination ->  LT.Text
renderIndex ariticles base pn =
    VL.render 2
      ["/bower_components/editor.md/css/editormd.min.css"]
      ["/bower_components/editor.md/editormd.min.js"
      ,"/assets/admin/index.js"]
      [renderTable,rednerDeleteModal]
  where
    renderArticle article =
      H.tr $ do
        H.td $ H.toHtml (M.articleID article)
        H.td $ H.toHtml (M.articleTitle article)
        H.td $ H.toHtml (M.articleSummary article)
        H.td $ H.toHtml (M.articlePublished article)
        H.td $ do
          H.a ! A.class_ "ui primary basic button"
            ! A.href (H.toValue  ("/admin/articles/" ++ (show $ M.articleID article) ++ "/edit") ) $ "编辑"
          H.button ! A.id (H.toValue (M.articleID article)) ! A.href "/admin/articles/remove/"
            ! A.class_ "ui negative basic button"  $ "删除"
    renderTableHead =
      H.thead $ do
        H.tr $ do
          H.th "id"
          H.th "title"
          H.th "summary"
          H.th "published"
          H.th "action"
    renderTableFooter =
      H.tfoot ! A.class_ "full-width" $ H.tr $
        H.th ! A.colspan "5" $ do
          H.div $ H.a ! A.class_ "ui small  positive basic button" ! A.href "/admin/articles/new" $ "新建"
          Pagination.render base pn
    renderTable =
      H.table ! A.class_ "ui celled table" $ do
        renderTableHead
        H.tbody $
          mapM_ renderArticle ariticles
        renderTableFooter
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
