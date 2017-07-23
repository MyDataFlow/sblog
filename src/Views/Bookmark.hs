{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Bookmark(
  renderIndex
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
renderIndex :: String -> String -> (Maybe T.Text) -> Int64 ->
  Pagination -> [M.Tag] -> [M.Bookmark] -> LT.Text
renderIndex host name tag tid pn ts brs =
    VL.render 3 title [] [(sidebar base tid ts)] [render]
  where
    title = "书签-" ++ name
    base =
      case tag of
        Nothing -> toURI "/bookmarks"
        Just t -> updateUrlParam  "tag" (T.unpack t) $ toURI  "/bookmarks"
    render =
      H.div $ do
        renderBookmarks
        Pagination.render base pn
    renderBookmarks =
      if length brs == 0
        then H.span ""
        else
          H.div ! A.class_ "ui segments" $
            mapM_ (segmentBookmark host name) brs
