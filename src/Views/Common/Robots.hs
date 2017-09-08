{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Views.Common.Robots(
  render
) where


import qualified Data.Text as T

import Network.URI

import Utils.URI.String
import Utils.URI.Params

-- | A basic robots file which just lists the "Sitemap: " line.
render :: String -> String -> T.Text
render base smurl =
  let
    s = T.pack $ show $ relativeTo (toURI smurl) (toURI base)
  in
  T.unlines
        [ "Sitemap: " `T.append` s
        , "User-agent: *"
        , "Allow: /"
        ]