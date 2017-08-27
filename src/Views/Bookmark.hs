{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Bookmark(
  renderIndex
  ,renderBookmark
)where

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.String (fromString)
import Data.Int
import Data.Maybe
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
  relativeTo (toURI $ "/bookmarks/" ++ (show $ M.bookmarkID ar)) (toURI host)

renderBreadcrumb :: [(String,String)] -> M.Bookmark -> Breadcrumb
renderBreadcrumb prevs ar = assemble
  where
    toLink (name,url) = Link name url
    assemble =
      let
        links = map toLink prevs
      in
        Breadcrumb links (M.bookmarkTitle ar)
renderContent :: [(String,String)]-> String -> M.Bookmark -> Response LT.Text
renderContent rcs url ar = do
    VL.renderWithTemplate "common/_content.html" assemble
  where
    ts = map (\tag -> T.pack $ M.tagName tag ) $ M.bookmarkTags ar
    assemble = ContentPage{
                            contentTitle = T.pack $ M.bookmarkTitle ar
                            ,contentTags = ts
                            ,contentPublish = T.pack $ time
                            ,contentSummary = Nothing
                            ,contentURL = Just $ T.pack $ url
                            ,contentBody = T.pack $ M.bookmarkSummary ar
                            ,contentRecommands = LT.toStrict $ renderHtml $ renderRecommand rcs
                          }
    time = formatTime defaultTimeLocale "%Y/%m/%d" $
      localTimeToUTC utc $ M.bookmarkUpdatedAt ar

renderBookmark :: [(String,String)] -> Bool
  -> [(String,String)]  -> M.Bookmark -> Response LT.Text
renderBookmark prevs canon rcs br = do
    name <- lift (asks siteName)
    host <- lift (asks siteHost)
    pageContent <- renderContent rcs (olink host name) br
    let p = page host name pageContent
    VL.renderPage p
  where
    page host name c = Page (T.pack $ title name)
      (Just $ renderBreadcrumb prevs br)
      (LT.toStrict $ renderHtml $ seo name host)
      (LT.toStrict c)
    olink host name =  showURI $
      updateUrlParams (utmParams host name) (toURI $ M.bookmarkUrl br)
    seo host name = do
      openGraph (title name) (show $ renderURL host br) (M.bookmarkTitle br)
      keywordsAndDescription (showTags $ M.bookmarkTags br) (M.bookmarkTitle br)
      when canon $ canonical (show $ renderURL host br)
    title name = (M.bookmarkTitle br) ++ "-" ++ name



renderIndex :: String -> String -> (Maybe T.Text) -> Int64 ->
  Pagination -> [M.Tag] -> Bool -> [M.Bookmark] -> LT.Text
renderIndex host name tag tid pn ts canon brs =
    VL.render 3 title [renderCanonical] [(sidebar base tid ts)] [render]
  where
    title = case tag of
      Nothing -> "书签-" ++ name
      Just t -> (T.unpack t) ++ "相关的书签-" ++ name
    base =
      case tag of
        Nothing -> toURI "/bookmarks"
        Just t -> updateUrlParam  "tag" (T.unpack t) $ toURI  "/bookmarks"
    fullURL =
      relativeTo (toURI "/articles") (toURI host)
    renderCanonical = when canon $ canonical (show fullURL)
    render =
      H.div $ do
        renderBookmarks
        Pagination.render base pn
    renderBookmarks =
      if length brs == 0
        then H.span ""
        else
          H.div ! A.class_ "ui segments" $
            mapM_ (segmentBookmark host name tag) brs
