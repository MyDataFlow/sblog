{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Article(
  renderArticle
  ,renderIndex
)where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.String (fromString)

import Network.URI

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text

import Views.Common.Widgets
import qualified Views.Layout as VL

import Utils.BlazeExtra.Pagination as Pagination
import Utils.URI.String

import qualified Models.DB.Schema as M

renderArticle :: [(String,String)] -> M.Article  -> LT.Text
renderArticle prevs ar =
    VL.renderMain title [] [render]
  where
    title = (M.articleTitle ar) ++ "-TTalkIM"
    render =
      H.div $ do
        H.div ! A.class_ "ui main text container" $ do
          breadcrumb prevs (M.articleTitle ar)
          H.h1 ! A.class_ "ui header" $ H.toHtml (M.articleTitle ar)
          H.div ! A.class_ "ui segment" $ H.toHtml (M.articleSummary ar)
        H.div ! A.class_ "ui article text container" $
          H.div ! A.class_ "ui piled segment" $
            H.preEscapedToHtml (M.articleBody ar)
renderIndex :: (Maybe T.Text) -> Pagination -> [M.Article] -> LT.Text
renderIndex tag pn ars =
    VL.render 2 "文章-TTalkIM" [] [] [render]
  where
    base =
      case tag of
        Nothing -> toURI "/articles"
        Just t -> toURI $ "/articles?tag=" ++ (T.unpack t)
    render =
      H.div $ do
        renderArticles
        Pagination.render base pn
    renderArticles =
      if length ars == 0
        then H.span ""
        else
          H.div ! A.class_ "ui segments" $
            mapM_ (segmentArticle tag) ars
