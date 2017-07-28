{-# LANGUAGE OverloadedStrings #-}
module Views.Common.Recommand where

import Control.Monad
import qualified Data.Text as T
import Data.Text.Lazy(Text)
import Data.String (fromString)
import Data.Int

import Network.URI
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Utils.BlazeExtra.Attributes as EA
import Utils.URI.String
import Utils.URI.Params

import qualified Models.DB.Schema as M

renderRecommand :: [(String,String)]-> H.Html
renderRecommand items =
    if length items == 0
      then H.span ""
      else
        H.p $ do
          "相关推荐："
          sequence_ $ map render items
          H.br
  where
    render (url,title) = do
      H.br
      H.a ! EA.hrefSet (toURI url) "s" "recommand" $ H.toHtml title
