{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Article(
  renderArticle
  ,renderIndex
  ,renderContent
  ,renderBreadcrumb
)where

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)
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

renderURL host ar =
  relativeTo (toURI $ "/articles/" ++ (show $ M.articleID ar)) (toURI host)

renderBreadcrumb :: [(String,String)] -> M.Article -> Breadcrumb
renderBreadcrumb prevs ar = assemble
  where
    toLink (name,url) = Link name url
    assemble =
      let
        links = map toLink prevs
      in
        Breadcrumb links (M.articleTitle ar)
renderContent :: [(String,String)]-> M.Article -> Response LT.Text
renderContent rcs ar = do
    VL.renderWithTemplate "common/_content.html" assemble
  where
    ts = map (\tag -> T.pack $ M.tagName tag ) $ M.articleTags ar
    assemble = ContentPage{
                            contentTitle = T.pack $ M.articleTitle ar
                            ,contentTags = ts
                            ,contentPublish = T.pack $ time
                            ,contentSummary = Just $ T.pack $ M.articleSummary ar
                            ,contentURL = Nothing
                            ,contentBody = T.pack $ M.articleBody ar
                            ,contentRecommands = LT.toStrict $ renderHtml $ renderRecommand rcs
                          }
    time = formatTime defaultTimeLocale "%Y/%m/%d" $
      localTimeToUTC utc $ M.articleUpdatedAt ar



renderArticle ::  [(String,String)] -> Bool ->
  [(String,String)] -> M.Article  -> Response LT.Text
renderArticle  bread canon rcs ar = do
    name <- lift (asks siteName)
    host <- lift (asks siteHost)
    pageContent <- renderContent rcs ar
    let p = page  host name pageContent
    VL.renderPage p
  where
    page host name c = Page (T.pack $ title name)
      (Just $ renderBreadcrumb bread ar)
      (LT.toStrict $ renderHtml $ seo name host)
      (LT.toStrict c)
    seo host name = do
      openGraph (title name) (show $ renderURL host ar ) (M.articleSummary ar)
      keywordsAndDescription (showTags $ M.articleTags ar) (M.articleSummary ar)
      when canon $ canonical (show $ renderURL host ar)
    title name = (M.articleTitle ar) ++ "-" ++ name

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
