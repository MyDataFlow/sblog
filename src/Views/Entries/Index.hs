{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Views.Entries.Index(
  render
)where
import Control.Monad
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe
import Data.Int
import Text.Blaze.Html.Renderer.Text

import Utils.URI.Params
import Utils.URI.String
import qualified Utils.BlazeExtra.Pagination as P
import Utils.Scotty.MustacheRender

import App.Types
import Models.Schema
import Views.Types
import qualified Views.Layout as VL

toTagURI tag = updateUrlParam "tag" (T.unpack tag) $ toURI "/entries"
renderContent es pn tags tag tid = do
    VL.renderWithTemplate "common/_index.html" ctx
  where
    baseURL = if tid == 0 then toURI "/entries" else toTagURI tag
    toLink t =
      let
        theClass = if (tag == tagName t) then "active list-group-item" else "list-group-item"
      in
        Link {
            linkTitle = (tagName t)
            ,linkClass = theClass
            ,linkURL = T.pack $ show $ toTagURI $ (tagName t)
          }
    ctx = IndexContent {
        indexEntries = es
        ,indexTags = L.map toLink tags
        ,indexPagination = LT.toStrict $ renderHtml $ P.render baseURL pn
      }

render :: [Entry] -> P.Pagination -> [Tag] -> T.Text -> Int64 -> Response LT.Text
render es pn tags tag tid = do
    s <- lift $ (asks site)
    contet <- renderContent es pn tags tag tid
    VL.render 
  where
    ctx name contet = Page {
        pageTitle = T.intercalate "-" ["首页", name]
        ,pageBread = Nothing
        ,pageSEO = Nothing
        ,pageContent = LT.toStrict contet
      }
