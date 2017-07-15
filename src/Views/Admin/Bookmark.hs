{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Admin.Bookmark(
  renderNew
  ,renderIndex
)where

import Control.Monad
import Data.Text.Lazy(Text)
import Data.String (fromString)

import Network.URI

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text
import Text.Markdown

import qualified Models.DB.Schema as M

renderNew :: M.Bookmark -> H.Html
renderNew bookmark =
  H.div $ do
    H.form ! A.class_ "ui form" ! A.action "/admin/bookmarks/create" ! A.method "POST" $ do
      H.div ! A.class_ "field" $ do
        H.label "标题"
        H.input ! A.type_ "text" ! A.name "title" ! A.value (H.toValue $ M.btitle bookmark)
      H.div ! A.class_ "field" $ do
        H.label "源连接"
        H.input ! A.type_ "text" ! A.name "url" ! A.value (H.toValue $ M.burl bookmark)
      H.div ! A.class_ "field" $ do
        H.div ! A.class_ "ui container" $ do
          H.div ! A.id "editor" ! A.name "content" $  H.toHtml (M.bsummary bookmark)
      H.div ! A.class_ "filed" $ do
        H.button ! A.class_ "ui primary button"  ! A.type_ "submit" $ "保存"
        H.a ! A.class_ "ui  button" ! A.href "/admin/bookmarks" $ "取消"

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
