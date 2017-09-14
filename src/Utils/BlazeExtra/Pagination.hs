{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.BlazeExtra.Pagination(
  Pagination(..)
  ,render
  ,renderToText
) where

import Control.Monad
import qualified Data.Text.Lazy as LT
import Data.String (fromString)
import Data.Default
import Network.URI

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import qualified Text.Blaze.Html.Renderer.Text as Render

-- | A pagination object, holds information about the name, total, per
--   page, current page, etc.
data Pagination = Pagination
  { pnTotal       :: Integer
  , pnPerPage     :: Integer
  , pnName        :: String
  , pnCurrentPage :: Integer
  , pnWidth       :: Integer
  , pnPrev        :: String
  , pnNext        :: String
  , pnMenuClass   :: String
  , pnActiveClass :: String
  , pnDeActiveClass :: String
  , pnDisabledClass :: String
  } deriving (Show)

instance Default Pagination where
  def = Pagination
        { pnTotal       = 0
        , pnPerPage     = 10
        , pnName        = ""
        , pnCurrentPage = 1
        , pnWidth       = 4
        , pnPrev        = "前一页"
        , pnNext        = "后一页"
        , pnMenuClass   = "pagination middle"
        , pnActiveClass = "active"
        , pnDeActiveClass = ""
        , pnDisabledClass = "disabled"
        }

-- | Get the page count of the pagination results.
pnPageCount :: Pagination -> Integer
pnPageCount Pagination{..} = max 1 $
  if total/perpage > fromIntegral (round (total/perpage))
     then round (total/perpage) + 1
     else round (total/perpage)
  where total = fromIntegral pnTotal
        perpage = fromIntegral pnPerPage

renderToText :: URI -> Pagination -> LT.Text
renderToText uri pn = Render.renderHtml $ render uri pn 

render :: URI ->  Pagination -> H.Html
render uri pn@Pagination{..}   =
    if pnTotal <= pnPerPage
      then H.div ""
      else
          H.ul ! A.id "pagination" ! A.class_ (H.toValue pnMenuClass) $ do
            when (pnCurrentPage > 1) $ prevPart
            when (pnCurrentPage - pnWidth > 1) $ do
              items 1 2
              omittedPart
              items start pnCurrentPage
            when (pnCurrentPage - pnWidth <= 1) $ items 1 pnCurrentPage
            when (pageCount - pnCurrentPage <= pnWidth) $ items (pnCurrentPage + 1) pageCount
            when (pageCount - pnCurrentPage > pnWidth) $ do
              items (pnCurrentPage + 1) (end - 1)
              omittedPart
              items (pageCount - 1) pageCount
            when (pnCurrentPage < pageCount) $ nextPart
  where
    start = max 1 (pnCurrentPage - round((fromIntegral pnWidth)/2))
    end = min pageCount (start + pnWidth)
    pageCount = pnPageCount pn
    paramName = pnName ++ "_page"
    items s e =
      forM_ [s..e] $ \i ->
        let
          theclass = if i == pnCurrentPage then pnActiveClass else pnDeActiveClass
        in
          H.li ! A.class_ (H.toValue theclass) $
            H.a ! H.dataAttribute "page" (H.toValue i)
              ! H.dataAttribute "count" (H.toValue pnPerPage)
              ! EA.hrefSet uri paramName (show i)
              $ H.toHtml $ show i
    omittedPart = H.li ! A.class_ (H.toValue pnDisabledClass) $ H.span "..."
    prevPart = H.li ! A.class_ (H.toValue pnDeActiveClass) $
      H.a ! H.dataAttribute "page"  (H.toValue (pnCurrentPage -1))
        ! H.dataAttribute "count" (H.toValue pnPerPage)
        ! EA.hrefSet uri paramName (show $ pnCurrentPage -1)
        $ H.toHtml pnPrev
    nextPart = H.li ! A.class_ (H.toValue pnDeActiveClass) $
      H.a ! H.dataAttribute "page"  (H.toValue (pnCurrentPage + 1))
        ! H.dataAttribute "count" (H.toValue pnPerPage)
        ! EA.hrefSet uri paramName (show $ pnCurrentPage +1)
        $ H.toHtml pnNext

    --1 2 [3 4] (5 6 |7| 8 9) [10 11] 12 13
