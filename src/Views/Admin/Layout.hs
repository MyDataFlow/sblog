{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Admin.Layout(
  render
  ,renderLogin
)where
import Control.Monad
import Data.Text.Lazy(Text)
import Data.String (fromString)

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Html as EH

import Text.Blaze.Html.Renderer.Text
import Views.Common.Widgets

defaultMeta :: [H.Html]
defaultMeta =
  [
    H.meta ! A.charset "utf-8",
    H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge,chrome=1",
    H.meta ! A.name "viewport"
     ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0"
  ]

renderAdminMenu :: Int -> H.Html
renderAdminMenu active =
    renderMenu  active menus
  where
    menus = [(1,"书签","/admin/bookmarks")
            ,(2,"文章","/admin/articles")]

renderHeader :: String -> [H.Html] -> H.Html
renderHeader title meta =
  H.head $ do
    sequence_ meta
    H.title $ H.toHtml title
    EH.cssLink "https://cdn.bootcss.com/semantic-ui/2.2.10/semantic.min.css"
    EH.cssLink "/bower_components/github-markdown-css/github-markdown.css"

renderInner :: [String] -> [String] -> [H.Html] -> H.Html -> H.Html
renderInner css js mainPart menu =
  H.html $ do
    renderHeader "管理后台" defaultMeta
    mapM_ EH.cssLink css
    H.body $ do
      menu
      H.div ! A.class_ "ui container" $ do
        H.div ! A.class_ "ui grid" $ do
          H.div ! A.class_ "sixteen wide column" $ do
            sequence_ mainPart
      EH.jsLink "https://cdn.bootcss.com/jquery/3.2.1/jquery.min.js"
      EH.jsLink "https://cdn.bootcss.com/semantic-ui/2.2.10/semantic.min.js"
      mapM_ EH.jsLink js

render :: Int -> [String] -> [String] -> [H.Html]  -> Text
render active css js mainPart  =
  renderHtml $ renderInner css js mainPart $ renderAdminMenu active

renderLoginInner :: [H.Html]  -> H.Html
renderLoginInner mainPart =
    H.html $ do
      renderHeader "登录" defaultMeta
      EH.cssLink "/assets/login.css"
      H.body $ do
        H.div ! A.class_ "ui middle aligned center aligned grid" $
          H.div ! A.class_ "login column" $ do
            sequence_ mainPart
        EH.jsLink "https://cdn.bootcss.com/jquery/3.2.1/jquery.min.js"
        EH.jsLink "https://cdn.bootcss.com/semantic-ui/2.2.10/semantic.min.js"

renderLogin :: [H.Html] -> Text
renderLogin inner =
  renderHtml $ renderLoginInner inner
