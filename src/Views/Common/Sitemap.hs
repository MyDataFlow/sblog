{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Views.Common.Sitemap(
  robots
  ,sitemap
  ,SitemapChangeFreq(..)
  ,SitemapUrl(..)
) where

import Data.Time (UTCTime,formatTime,defaultTimeLocale)
import Data.Maybe
import qualified Data.Text as T
import Text.XML.Light

import Network.URI

import Utils.URI.String
import Utils.URI.Params

data SitemapChangeFreq = Always
                       | Hourly
                       | Daily
                       | Weekly
                       | Monthly
                       | Yearly
                       | Never

data SitemapUrl = SitemapUrl
    { sitemapLoc :: String
    , sitemapLastMod :: Maybe UTCTime
    , sitemapChangeFreq :: Maybe SitemapChangeFreq
    , sitemapPriority :: Maybe Double
    }

showFreq :: SitemapChangeFreq -> String
showFreq Always  = "always"
showFreq Hourly  = "hourly"
showFreq Daily   = "daily"
showFreq Weekly  = "weekly"
showFreq Monthly = "monthly"
showFreq Yearly  = "yearly"
showFreq Never   = "never"

-- | A basic robots file which just lists the "Sitemap: " line.
robots :: String -> String -> T.Text
robots base smurl =
  let
    s = T.pack $ show $ relativeTo (toURI smurl) (toURI base)
  in
  T.unlines
        [ "Sitemap: " `T.append` s
        , "User-agent: *"
        , "Allow: /"
        ]

elementString :: String -> String -> Element
elementString name content =
  Element {
          elName  = unqual name
          , elAttribs = []
          , elContent = [Text (CData CDataText content Nothing)]
          , elLine    = Nothing
          }
element :: String -> [Element] -> Element
element name content =
  Element {
          elName    = unqual name
          , elAttribs = []
          , elContent = map Elem content
          , elLine    = Nothing
          }
formatW3 :: UTCTime -> String
formatW3 = formatTime defaultTimeLocale "%FT%X-00:00"

xmlUrl :: SitemapUrl  -> Element
xmlUrl url =
    let
      sub = [xmlLoc, xmlLastMod, xmlChangeFreq, xmlPriority]
      inner = [e | e <- sub,isJust e]
    in
      if length inner == 0
        then element "url" []
        else element "url" $ map fromJust inner
  where
    xmlLoc = return $ elementString "loc" $ sitemapLoc url
    xmlLastMod =
      case sitemapLastMod url of
        Nothing -> Nothing
        Just lm -> return $  elementString "lastmod" $ formatW3 lm
    xmlChangeFreq =
      case sitemapChangeFreq url of
        Nothing -> Nothing
        Just cf -> return $ elementString "changefreq" $ showFreq cf
    xmlPriority =
      case sitemapPriority url of
        Nothing -> Nothing
        Just p -> return $ elementString "priority" $ show p

xmlUrlSet :: [SitemapUrl] -> Element
xmlUrlSet urls = add_attr xmlns . element "urlset" $ map xmlUrl urls
  where xmlns = Attr (unqual "xmlns") "http://www.sitemaps.org/schemas/sitemap/0.9"

sitemap :: [SitemapUrl] -> String
sitemap urls =
  ppcTopElement prettyConfigPP $ xmlUrlSet urls
