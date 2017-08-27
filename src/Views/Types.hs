{-# LANGUAGE OverloadedStrings #-}

module Views.Types where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Int
import Data.Maybe
import Data.Default
import Text.Mustache

import Utils.URI.Params
import Utils.URI.String

import Models.Schema

data Link = Link {
  linkTitle :: T.Text
  ,linkClass :: T.Text
  ,linkURL :: T.Text
}
instance ToMustache Link where
  toMustache l = object
    [ "link" ~> linkURL l
    , "theClass" ~> linkClass l
    , "title" ~> linkTitle l
    ]
data Breadcrumb = Breadcrumb {
  breadLinks :: [Link]
  ,breadActive :: T.Text
}
instance ToMustache Breadcrumb where
  toMustache b = object
    [
      "links" ~> breadLinks b
    , "active" ~> breadActive b
    ]
data IndexContent = IndexContent {
  indexEntries :: [Entry]
  ,indexTags :: [Link]
  ,indexPagination :: T.Text
}
instance ToMustache IndexContent where
  toMustache index = object
    [ "entries" ~> indexEntries index
    , "tags" ~> indexTags index
    , "pagination" ~> indexPagination index
    ]
data Page = Page {
  pageTitle :: T.Text
  ,pageBread :: Maybe Breadcrumb
  ,pageSEO :: Maybe T.Text
  ,pageContent :: T.Text
}
instance ToMustache Page where
  toMustache p = object
    [ "title" ~> pageTitle p
    , "breadcrumb" ~> pageBread p
    , "seo" ~> pageSEO p
    , "content" ~> pageContent p
    ]
tagRelativeURL :: String -> T.Text
tagRelativeURL name = T.pack $ show $ updateUrlParam "tag" name $ toURI "/entries"
entryTagsToLink :: Tag -> Link
entryTagsToLink t = Link {
  linkTitle = tagName t
  ,linkClass = "label label-pink arrowed-in"
  ,linkURL = tagRelativeURL $ T.unpack $ tagName t
  }
instance ToMustache Entry where
  toMustache e = object
    [ "title" ~> entryTitle e
    , "tags" ~> (L.map entryTagsToLink $ entryTags e)
    , "publish" ~> (utcDateString $ entryUpdatedAt e)
    , "summary" ~> entrySummary e
    , "url" ~> entryURL e
    , "body" ~> entryBody e
    ]
