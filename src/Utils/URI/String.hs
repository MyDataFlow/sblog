{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.URI.String(
  showRelativeURI
  ,showURI
  ,toURI
)where
import Network.URI
import Data.Maybe

showRelativeURI :: URI -> String
showRelativeURI URI{..} = uriPath ++ uriQuery

showURI :: URI -> String
showURI uri = show uri

toURI :: String -> URI
toURI u =
  case parseURI u of
    Nothing ->  fromJust $ parseRelativeReference u
    Just uri -> uri
