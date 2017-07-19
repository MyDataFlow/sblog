{-# LANGUAGE OverloadedStrings #-}

module Views.Common.Pagination(
  render
) where

import Control.Monad
import Data.Text.Lazy(Text)
import Data.String (fromString)

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA

render :: URI -> Int -> Int -> Int -> H.Html
render uri page count total =
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
              H.a ! EA.hrefSet uri paramName (show (page + 1)) $ "后一页"
