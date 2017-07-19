{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Admin.Bookmark(
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

import qualified Models.DB.Schema as M

renderWriter :: M.Bookmark -> String -> H.Html
renderWriter bookmark url =
    H.div $ do
      H.form ! A.class_ "ui form" ! A.action (H.toValue url) ! A.method "POST" $ do
        textField "标题" "title" (M.btitle bookmark)
        textField "源连接" "url" (M.burl bookmark)
        contentField (M.bsummary bookmark)
        tagsField $ ts
        H.div ! A.class_ "filed" $ do
          H.button ! A.class_ "ui primary button"  ! A.type_ "submit" $ "保存"
          H.a ! A.class_ "ui  button" ! A.href "/admin/bookmarks" $ "取消"
  where
    ts = map (\tag -> (M.name tag)) (M.btags bookmark)

renderIndex :: [M.Bookmark] ->  H.Html
renderIndex bookmarks =
  H.table ! A.class_ "ui celled table" $ do
    H.thead $ do
      H.tr $ do
        H.th "id"
        H.th "title"
        H.th "url"
        H.th "created_at"
        H.th "updated_at"
        H.th "action"
    H.tbody $ do
      mapM_ renderBookmark bookmarks
  where
    renderBookmark bookmark =
      H.tr $ do
        H.td $ H.toHtml (M.bid bookmark)
        H.td $ H.toHtml (M.btitle bookmark)
        H.td $ H.toHtml (M.burl bookmark)
        H.td $ H.toHtml $ show (M.bcreatedAt bookmark)
        H.td $ H.toHtml $ show (M.bupdatedAt bookmark)
        H.td $ do
          H.a ! A.class_ "ui  button" ! A.href (H.toValue  ("/admin/bookmarks/" ++ (show $ M.bid bookmark) ++ "/edit") ) $ "编辑"
          H.a ! A.class_ "ui  button" ! A.href (H.toValue  ("/admin/bookmarks/" ++ (show $ M.bid bookmark) ++ "/edit") ) $ "删除"
