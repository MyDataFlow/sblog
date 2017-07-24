{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.BlazeExtra.Attributes(
  hrefSet
  ,hrefMultiSet
  ,hrefURI
)where

import Control.Monad
import Text.Blaze.Html5            as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Network.URI

import Utils.URI.Params
import Utils.URI.String


-- | Set a parameter of a URI, as an attribute.
hrefSet :: URI -> String -> String -> H.Attribute
hrefSet uri key value = hrefURI updated where
  updated = updateUrlParam key value uri

hrefMultiSet :: URI -> [(String,String)] -> H.Attribute
hrefMultiSet uri m = hrefURI updated where
  updated = updateUrlParams m uri

-- | Provide a URI as an attribute for href.
hrefURI :: URI -> H.Attribute
hrefURI uri = href (toValue (showRelativeURI uri))
