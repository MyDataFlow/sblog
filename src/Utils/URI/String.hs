{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.URI.String(
  showRelativeURI
  ,toURI
  ,toPath
)where


import Data.List
import Data.Maybe

import Network.URI

showRelativeURI :: URI -> String
showRelativeURI URI{..} = uriPath ++ uriQuery

toURI :: String -> URI
toURI u =
    case parseURI u of
      Nothing -> parseReference
      Just uri -> uri
  where
    parseReference =
      case parseRelativeReference u of
        Nothing -> fromJust $ parseURIReference u
        Just uri -> uri

toPath :: [String] -> String
toPath sl =
    "/" ++ (intercalate "/" $ map esc sl)
  where
    esc = escapeURIString isAllowedInURI
