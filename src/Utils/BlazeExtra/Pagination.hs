{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.BlazeExtra.Pagination(
  Pagination(..)
  ,render
) where

import Control.Monad
import Data.Text.Lazy(Text)
import Data.String (fromString)
import Data.Default
import Network.URI
import Network.URI.Params

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA

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
        }

-- | Get the page count of the pagination results.
pnPageCount :: Pagination -> Integer
pnPageCount Pagination{..} = max 1 $
  if total/perpage > fromIntegral (round (total/perpage))
     then round (total/perpage) + 1
     else round (total/perpage)
  where total = fromIntegral pnTotal
        perpage = fromIntegral pnPerPage

render :: URI ->  Pagination -> H.Html
render uri pn@Pagination{..}  =
    if pnTotal <= pnPerPage
      then H.div ""
      else
          H.div ! A.class_ "ui pagination menu" $ do
            when (pnCurrentPage > 1) $ prevPart
            when (end < pageCount) $ middlePart
            when (pnCurrentPage < pageCount) $ nextPart
  where
    w = pnWidth pn
    start = max 1 (page - 2)
    end = min pageCount (start + w)
    pageCount = pnPageCount pn
    paramName = pnName pn ++ "_page"
    items =
      forM_ [start..end] $ \i ->
        let
          theclass = if i == page then "active item" else "item"
        in
          H.div ! A.class_ theclass $
            H.a ! EA.hrefSet uri paramName (show i) $ H.toHtml (show i)
    prevPart =
      H.div ! A.class_ "item" $ do
        H.a ! EA.hrefSet uri paramName (show (pnCurrentPage -1)) $ H.toHtml pnPrev
        items
    middlePart = H.div ! A.class_ "disabled item" $ "..."
    nextPart =
      H.div ! A.class_ "item" $
        H.a ! EA.hrefSet uri paramName (show (pnCurrentPage + 1)) $ H.toHtml pnNext
