{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Articles(
  render
  ,renderPagination
)where

import Control.Monad
import Data.Text.Lazy(Text)
import Data.String (fromString)

import Network.URI

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text
import Text.Markdown

import qualified Models.DB.Schema as M


tag :: M.Tag -> H.Html
tag t =
  H.a ! url ! A.class_ "ui tag label" $ H.toHtml $ M.name t
  where
    url = A.href $ fromString $ "/tags/" ++ (show $ M.tid t)

indexArticle :: M.Article -> H.Html
indexArticle ar =
  H.div ! A.class_ "item" $
    H.div ! A.class_ "content" $ do
      H.div ! A.class_ "header" $ H.toHtml title
      H.div ! A.class_ "description" $ markdown def $ fromString summary
      H.div ! A.class_ "extra" $ do
        H.div ! A.class_ "ui right floated primary basic button" $ 
          H.a ! (link aid) $ "阅读全文"
        H.div ! A.class_ "ui tag labels" $
          mapM_ tag tags

  where
    aid = M.aid ar
    title = M.title ar
    summary = M.summary ar
    tags = M.tags ar
    link aid = A.href $ fromString $ "/articles/" ++  (show aid)

render :: [M.Article] -> H.Html
render articles =
  if length articles == 0
    then H.span ""
    else
      H.div ! A.class_ "ui divided items" $ do
        mapM_ indexArticle articles

renderPagination :: URI -> Int -> Int -> Int -> H.Html
renderPagination uri page count total =
  let
    m = mod total count
    d = div total count
    pageCount = if m == 0 then d else d + 1
    w = 4
    start = max 1 (page - 2)
    end = min pageCount (start + w)
    paramName = "page"
  in
    if total == 1
      then H.div ""
      else
        H.div ! A.class_ "ui pagination menu" $ do
          when (page > 1) $
            H.div ! A.class_ "item" $ do
              H.a ! EA.hrefSet uri paramName (show (page -1)) $ "前一页"
              forM_ [start..end] $ \i ->
                let
                  theclass = if i == page then "active item" else "item"
                in
                  H.div ! A.class_ theclass $ do
                    H.a ! EA.hrefSet uri paramName (show i) $ H.toHtml (show i)
          when (end < pageCount) $
            H.div ! A.class_ "disabled item" $ "..."
          when (page < pageCount) $
            H.div ! A.class_ "item" $ do
              H.a !  EA.hrefSet uri paramName (show (page + 1)) $ "后一页"
