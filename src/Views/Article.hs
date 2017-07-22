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
import Data.Int

import Network.URI

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text

import Views.Common.Widgets
import Views.Common.SEO
import qualified Views.Layout as VL

import Utils.BlazeExtra.Pagination as Pagination
import Utils.URI.String

import qualified Models.DB.Schema as M

renderArticle :: String -> String  -> [(String,String)] -> M.Article  -> LT.Text
renderArticle host name prevs ar =
    VL.renderMain title [seo] [render]
  where
    seo = do
      openGraph title (show fullURL) (M.articleSummary ar)
      keywordsAndDescription (showTags $ M.articleTags ar) (M.articleSummary ar)
    title = (M.articleTitle ar) ++ "-" ++ name
    fullURL =
      relativeTo (toURI $ "/articles/" ++ (show $ M.articleID ar)) (toURI host)
    render =
      H.div $ do
        H.div ! A.class_ "ui main text container" $ do
          breadcrumb prevs (M.articleTitle ar)
          H.h1 ! A.class_ "ui header" $ H.toHtml (M.articleTitle ar)
          H.div ! A.class_ "ui segment" $ H.toHtml (M.articleSummary ar)
        H.div ! A.class_ "ui article text container" $
          H.div ! A.class_ "ui piled segment" $
            H.preEscapedToHtml (M.articleBody ar)
renderIndex :: String -> (Maybe T.Text) -> Int64 ->
  Pagination -> [M.Tag] -> [M.Article] -> LT.Text
renderIndex name tag tid pn ts ars =
    VL.render 2 title [] [(sidebar base tid ts)] [render]
  where
    title = "文章-" ++ name
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
