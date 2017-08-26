{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Article(
  renderArticle
  ,renderIndex
  ,renderContent
  ,renderBreadcrumb
)where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as Map
import Data.Maybe
import Data.String (fromString)
import Data.Int
import Data.Time (UTCTime,LocalTime,localTimeToUTC,utc,formatTime,defaultTimeLocale)

import Network.URI

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text

import Views.Common.Widgets
import Views.Common.SEO
import Views.Common.Recommand
import qualified Views.Layout as VL

import Utils.BlazeExtra.Pagination as Pagination

import Utils.URI.String
import Utils.URI.Params
import App.Types
import Views.Types
import qualified Models.DB.Schema as M
renderBreadcrumb :: [(String,String)] -> M.Article -> Breadcrumb
renderBreadcrumb prevs ar = assemble
  where
    toLink (name,url) = Link name url
    assemble =
      let
        links = map toLink prevs
      in
        Breadcrumb links (M.articleTitle ar)
renderContent :: String -> String -> [(String,String)]->M.Article -> Response LT.Text
renderContent host name rcs ar = do
    VL.renderWithTemplate "articles/_content.html" assemble
  where
    assemble :: Map.Map String T.Text
    assemble = Map.fromList
        [("fullURL",T.pack $ show fullURL)
        ,("recommands",LT.toStrict $ renderHtml $ renderRecommand rcs )
        ,("content",T.pack $ M.articleBody ar)
        ,("site.name",T.pack name)
        ]
    fullURL =
      relativeTo (toURI $ "/articles/" ++ (show $ M.articleID ar)) (toURI host)


renderArticle :: String -> String  -> [(String,String)]
  ->Bool -> LT.Text ->M.Article  -> Response LT.Text
renderArticle host name bread canon content ar = do
    pageContent <- render
    let p = page pageContent
    VL.renderPage p
  where
    page c = Page (T.pack $ title)
      (Just $ renderBreadcrumb bread ar)
      (LT.toStrict $ renderHtml seo)
      (LT.toStrict c)
    assemble :: Map.Map String T.Text
    assemble = Map.fromList
      [("title",T.pack $ (M.articleTitle ar))
      ,("summary",T.pack $ M.articleSummary ar)
      ,("publish",T.pack $ formatTime defaultTimeLocale "%Y/%m/%d" time)
      ,("content",LT.toStrict content)
      ]
    time = localTimeToUTC utc $ M.articleUpdatedAt ar
    seo = do
      openGraph title (show fullURL) (M.articleSummary ar)
      keywordsAndDescription (showTags $ M.articleTags ar) (M.articleSummary ar)
      when canon $ canonical (show fullURL)
    title = (M.articleTitle ar) ++ "-" ++ name
    fullURL =
      relativeTo (toURI $ "/articles/" ++ (show $ M.articleID ar)) (toURI host)
    render = VL.renderWithTemplate "articles/show.html" assemble

renderIndex :: String -> String -> (Maybe T.Text) -> Int64 ->
  Pagination -> [M.Tag] -> Bool -> [M.Article] -> LT.Text
renderIndex host name tag tid pn ts canon ars =
    VL.render 2 title [renderCanonical] [(sidebar base tid ts)] [render]
  where
    title = case tag of
      Nothing -> "文章-" ++ name
      Just t -> (T.unpack t) ++ "相关的文章-" ++ name
    base =
      case tag of
        Nothing -> toURI "/articles"
        Just t -> updateUrlParam  "tag" (T.unpack t) $ toURI  "/articles"
    fullURL =
          relativeTo (toURI "/articles") (toURI host)
    renderCanonical = when canon $ canonical (show fullURL)
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
