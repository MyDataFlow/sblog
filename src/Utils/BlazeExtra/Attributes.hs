
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Utils.BlazeExtra.Attributes(
  hrefSet
)where

import Control.Monad
import Text.Blaze.Html5            as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Network.URI

import Utils.URI.Params

-- | Set a parameter of a URI, as an attribute.
hrefSet :: URI -> String -> String -> H.Attribute
hrefSet uri key value = hrefURI updated where
  updated = updateUrlParam key value uri

-- | Provide a URI as an attribute for href.
hrefURI :: URI -> H.Attribute
hrefURI uri = href (toValue (showURI uri)) where
  showURI URI{..} = uriPath ++ uriQuery
