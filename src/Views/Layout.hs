{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Layout(
  render
  ,renderAdmin
)where
import Control.Monad
import Data.Text.Lazy(Text)
import Data.String (fromString)

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Html as EH

import Text.Blaze.Html.Renderer.Text

defaultMeta :: [H.Html]
defaultMeta =
  [
    H.meta ! A.charset "utf-8",
    H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge,chrome=1",
    H.meta ! A.name "viewport"
     ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0"
  ]

renderMenu :: [(Int,String,String)] -> Int -> H.Html
renderMenu menus active =
  H.div ! A.class_ "ui menu" $
    H.div ! A.class_ "ui container" $
      toItems
  where
    toItems =
      forM_ menus $ \(ord,menu,url) ->
        let
          theclass = if ord == active then "active blue item" else "item"
        in
          H.a ! A.class_  theclass ! A.href (H.toValue url) $ H.toHtml menu

renderAdminMenu :: Int -> H.Html
renderAdminMenu active =
    renderMenu menus active
  where
    menus = [(1,"书签","/admin/bookmarks")
            ,(2,"文章","/admin/articles")]
renderNormalMenu :: Int -> H.Html
renderNormalMenu active =
    renderMenu menus active
  where
    menus = [(1,"首页","/")
            ,(2,"书签","/bookmarks")
            ,(3,"文章","/articles")]

renderHeader :: String -> [H.Html] -> H.Html
renderHeader title meta =
  H.head $ do
    sequence_ meta
    H.title $ H.toHtml title
    EH.cssLink "https://cdn.bootcss.com/semantic-ui/2.2.10/semantic.min.css"
    EH.cssLink "/bower_components/github-markdown-css/github-markdown.css"

renderInner :: String -> [H.Html] -> [H.Html] -> [H.Html] -> H.Html -> H.Html
renderInner title meta sidePart mainPart menu =
  H.html $ do
    renderHeader title meta
    H.body $ do
      menu
      H.div ! A.class_ "ui container" $ do
        H.div ! A.class_ "ui grid" $ do
          H.div ! A.class_ "ten wide computer eleven wide tablet sixteen wide mobile column" $ do
            sequence_ mainPart
          H.div ! A.class_ "four wide computer five wide tablet sixteen wide mobile column" $ do
            sequence_ sidePart
      EH.jsLink "https://cdn.bootcss.com/jquery/3.2.1/jquery.min.js"
      EH.jsLink "https://cdn.bootcss.com/semantic-ui/2.2.10/semantic.min.js"

renderAdminInner css js mainPart menu =
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

render :: Int -> String -> [H.Html] -> [H.Html] -> [H.Html]  -> Text
render active title meta sidePart mainPart =
  renderHtml $
    renderInner title combineMeta sidePart mainPart $
      renderNormalMenu active
  where
    combineMeta = defaultMeta ++ meta

renderAdmin :: Int -> [String] -> [String] -> [H.Html]  -> Text
renderAdmin active css js mainPart  =
  renderHtml $ renderAdminInner css js mainPart $ renderAdminMenu active
