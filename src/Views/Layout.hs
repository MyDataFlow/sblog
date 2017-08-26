{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Layout(
  render
  ,renderMain
  ,renderWithTemplate
  ,renderPage
)where
import Control.Monad
import Control.Monad.IO.Class(MonadIO,liftIO)

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.String (fromString)
import Data.Maybe

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.Scotty.Trans as Web
import qualified Utils.BlazeExtra.Html as EH
import Utils.Scotty.MustacheRender

import Text.Blaze.Html.Renderer.Text
import Views.Common.Widgets
import App.Types

renderWithTemplate :: (ToMustache k) => FilePath -> k -> Response LT.Text
renderWithTemplate tpl k = do
  r <- liftIO $ hastache ["templates","templates/partials/"] tpl k
  case r of
    Just t ->  return $ LT.fromStrict t
    Nothing -> Web.raise $ AppError $ LT.pack $ "Can't find template " ++ tpl
renderPage :: (ToMustache k) => k -> Response LT.Text
renderPage k = do
  r <- liftIO $ hastache ["templates","templates/partials/"] "layout.html" k
  case r of
    Just t ->  return $ LT.fromStrict t
    Nothing -> Web.raise $ AppError $ LT.pack $ "Can't find template "

defaultMeta :: [H.Html]
defaultMeta =
  [
    H.meta ! A.charset "utf-8",
    H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge,chrome=1",
    H.meta ! A.name "viewport"
     ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0"
  ]

renderNormalMenu :: Int -> H.Html
renderNormalMenu active =
    renderMenu  active menus
  where
    menus = [(1,"首页","/")
            ,(2,"文章","/articles")
            ,(3,"书签","/bookmarks")
            ,(4,"RSS","/feed")
            ,(5,"微博","http://weibo.com/u/1900044837")]

renderHeader :: String -> [H.Html] -> H.Html
renderHeader title meta =
  H.head $ do
    sequence_ meta
    H.title $ H.toHtml title
    EH.cssLink "https://cdn.bootcss.com/semantic-ui/2.2.10/semantic.min.css"
    EH.cssLink "https://cdn.bootcss.com/github-markdown-css/2.8.0/github-markdown.min.css"

renderInner :: String -> [H.Html] -> [H.Html] -> [H.Html] -> H.Html -> H.Html
renderInner title meta sidePart mainPart menu =
  H.docTypeHtml $
    H.html ! A.lang "zh" $ do
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
      EH.jsLink "/assets/ga.js"

render :: Int -> String -> [H.Html] -> [H.Html] -> [H.Html]  -> LT.Text
render active title meta sidePart mainPart =
  renderHtml $
    renderInner title combineMeta sidePart mainPart $
      renderNormalMenu active
  where
    combineMeta = defaultMeta ++ meta

renderMainInner :: String -> [H.Html] -> [H.Html]  -> H.Html
renderMainInner title meta mainPart  =
  H.docTypeHtml $
    H.html ! A.lang "zh"  $ do
      renderHeader title meta
    --  EH.cssLink "/bower_components/editor.md/css/editormd.min.css"
      EH.cssLink "/assets/main.css"
      H.body $ do
        sequence_ mainPart
        EH.jsLink "https://cdn.bootcss.com/jquery/3.2.1/jquery.min.js"
        EH.jsLink "https://cdn.bootcss.com/semantic-ui/2.2.10/semantic.min.js"
        EH.jsLink "/assets/ga.js"

renderMain :: String -> [H.Html] -> [H.Html]  -> LT.Text
renderMain title meta mainPart =
    renderHtml $ renderMainInner title combineMeta  mainPart
  where
    combineMeta = defaultMeta ++ meta
