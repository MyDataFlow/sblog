{-# LANGUAGE OverloadedStrings #-}

module Views.Common.Widgets where

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

import Views.Common.SEO

import qualified Models.DB.Schema as M

renderMenu ::  Int -> [(Int,String,String)] ->H.Html
renderMenu active menus  =
  H.div ! A.class_ "ui menu" $
    H.div ! A.class_ "ui container" $
      toItems
  where
    toItems =
      forM_ menus $ \(ord,menu,url) ->
        let
          theclass = if ord == active then "active blue item" else "item"
        in
          H.a ! (gaEvent "Read Menu" menu) ! A.class_  theclass ! A.href (H.toValue url) $ H.toHtml menu

sidebar :: URI -> Int64 -> [M.Tag]  -> H.Html
sidebar base active ts =
  if length ts == 0
    then H.span ""
    else
      H.div ! A.class_ "ui vertical menu" $ do
        mapM_ tag ts
  where
    tag t =
      let
        l =  EA.hrefSet base "tag" $ M.tagName t
        c = if active == M.tagID t then "active teal item" else "item"
        cl = if active == M.tagID t then "ui teal label" else "ui label"
      in
        H.a ! (gaEvent "Read Tag" (M.tagName t)) ! A.class_ c ! l $ do
          H.toHtml $ M.tagName t
          H.div ! A.class_ cl $ H.toHtml $ show $ M.tagCount t

breadcrumb :: [(String,String)] -> String -> H.Html
breadcrumb prevs current =
    H.div ! A.class_ "ui breadcrumb" $ do
      sequence_ (map renderPrev prevs)
      H.div ! A.class_ "active section" $ H.toHtml current
  where
    renderPrev (name,url) = do
      H.a ! A.class_ "section" ! A.href (H.toValue url) $ H.toHtml name
      H.i ! A.class_ "right angle icon divider" $ ""


tags :: URI -> [M.Tag] -> H.Html
tags base ts =
    H.div ! A.class_ "ui tag labels" $
      mapM_ render ts
  where
    url t = EA.hrefSet base "tag" $ M.tagName t
    render t =
      H.a ! (gaEvent "Read Tag" (M.tagName t)) ! (url t) ! A.class_ "ui tag label" $ do
        H.toHtml $ M.tagName t
        H.div ! A.class_ "detail" $ H.toHtml $ (M.tagCount t)

segmentArticle :: Maybe T.Text -> M.Article -> H.Html
segmentArticle tag ar =
  H.div ! A.class_ "ui teal secondary segment" $
    H.div ! A.class_ "item" $
      H.div ! A.class_ "content" $ do
        H.div ! A.class_ "ui small right floated primary basic button" $
          H.a ! (gaEvent "Read Article" title) !  link $ "阅读原文"
        H.div ! A.class_ "header" $ H.p $ H.toHtml title
        H.div ! A.class_ "description" $ H.p $ H.toHtml summary
        H.div ! A.class_ "extra" $
          if length ts == 0
            then H.span ""
            else tags (toURI  "/articles") ts
  where
    aid = M.articleID ar
    title = M.articleTitle ar
    summary = M.articleSummary ar
    ts = M.articleTags ar
    l = "/articles/" ++  (show $ M.articleID ar)
    link =
      case tag of
        Nothing -> EA.hrefURI $ toURI l
        Just t ->  EA.hrefSet (toURI l) "tag" (T.unpack t)

utmParams :: String -> String -> [(String,String)]
utmParams host name =
  [("utm_source",host)
  ,("utm_campaign",name)
  ,("utm_medium","website")]

segmentBookmark :: String -> String -> Maybe T.Text -> M.Bookmark -> H.Html
segmentBookmark host name tag  br =
    H.div ! A.class_ "ui olive secondary segment" $
      H.div ! A.class_ "item" $
        H.div ! A.class_ "content" $ do
          H.div ! A.class_ "ui small right floated primary basic button" $
            H.a ! (gaEvent "Read Bookmark Comment" title) ! link $ "点评"
          H.div ! A.class_ "ui small right floated primary basic button" $
            H.a ! (gaEvent "Read Bookmark" title) ! A.rel "nofollow" ! olink $ "原文"
          H.div ! A.class_ "header" $ H.p $ H.toHtml title
          H.div ! A.class_ "extra" $ do
            if length ts == 0
              then H.span ""
              else tags (toURI  "/bookmarks") ts
  where
    bid = M.bookmarkID br
    title = M.bookmarkTitle br
    summary = M.bookmarkSummary br
    ts = M.bookmarkTags br
    l = "/bookmarks/" ++  (show $ M.bookmarkID br)
    link =
      case tag of
        Nothing -> EA.hrefURI $ toURI l
        Just t ->  EA.hrefSet (toURI l) "tag" (T.unpack t)

    olink = A.href (H.toValue $ showURI
      $ updateUrlParams (utmParams host name) (toURI $ M.bookmarkUrl br))
