{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.BlazeExtra.Html(
  jsLink
  ,cssLink
)where

import Data.String (fromString)

import Text.Blaze.Html5            as H hiding (map)
import Text.Blaze.Html5.Attributes as A

cssLink :: String -> H.Html
cssLink ref =
  H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! l
  where
    l = A.href $ fromString ref

jsLink :: String -> H.Html
jsLink ref =
  H.script ! A.type_ "text/javascript" ! l $ ""
  where
    l = A.src $ fromString ref
