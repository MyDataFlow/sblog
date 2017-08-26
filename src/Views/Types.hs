{-# LANGUAGE OverloadedStrings #-}

module Views.Types where

import qualified Data.Text as T
import Data.Int
import Data.Maybe
import Data.Default
import Text.Mustache
data Link = Link {
  linkTitle :: String
  ,linkURL :: String
}
data Breadcrumb = Breadcrumb {
  breadLinks :: [Link]
  ,breadTitle :: String
}
data Page = Page {
  pageTitle :: T.Text
  ,pageBread :: Maybe Breadcrumb
  ,pageSeo :: T.Text
  ,pageContent :: T.Text
}
instance ToMustache Link where
  toMustache l = object
    [ "link" ~> linkURL l
    , "title" ~> linkTitle l
    ]
instance ToMustache Breadcrumb where
  toMustache b = object
    [
      "links" ~> breadLinks b
    , "title" ~> breadTitle b
    ]
instance ToMustache Page where
  toMustache p = object
    [
      "title" ~> pageTitle p
      ,"breadcrumb" ~> pageBread p
      ,"seo" ~> pageSeo p
      ,"content" ~> pageContent p
    ]
