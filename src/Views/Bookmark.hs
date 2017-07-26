{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Bookmark(
  renderIndex
  ,renderBookmark
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
import Utils.URI.Params

import qualified Models.DB.Schema as M

renderBookmark :: String -> String  -> [(String,String)] -> Bool -> M.Bookmark -> LT.Text
renderBookmark host name prevs canon br =
    VL.renderMain title [seo] [render]
  where
    olink = A.href (H.toValue $ showURI
      $ updateUrlParams (utmParams host name) (toURI $ M.bookmarkUrl br))
    seo = do
      openGraph title (show fullURL) (M.bookmarkTitle br)
      keywordsAndDescription (showTags $ M.bookmarkTags br) (M.bookmarkTitle br)
      when canon $ canonical (show fullURL)
    title = (M.bookmarkTitle br) ++ "-" ++ name
    fullURL =
      relativeTo (toURI $ "/bookmarks/" ++ (show $ M.bookmarkID br)) (toURI host)
    render =
      H.div $ do
        H.div ! A.class_ "ui main text container" $ do
          breadcrumb prevs (M.bookmarkTitle br)
          H.h1 ! A.class_ "ui header" $ do
            H.div ! A.class_ "ui small right floated primary basic button" $
              H.a ! (gaEvent "Read Bookmark" title) ! A.rel "nofollow" ! olink $ "原文"
            H.toHtml (M.bookmarkTitle br)
          H.div ! A.class_ "ui article text container" $
            H.div ! A.class_ "markdown-body" $ do
              H.preEscapedToHtml  (M.bookmarkSummary br)
              H.p $ ""
              H.div $
                H.h5 ! A.class_ "ui block header" $
                  H.toHtml $ "欢迎转载，著作权归" ++ name ++ "所有"

renderIndex :: String -> String -> (Maybe T.Text) -> Int64 ->
  Pagination -> [M.Tag] -> Bool -> [M.Bookmark] -> LT.Text
renderIndex host name tag tid pn ts canon brs =
    VL.render 3 title [renderCanonical] [(sidebar base tid ts)] [render]
  where
    title = "书签-" ++ name
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
