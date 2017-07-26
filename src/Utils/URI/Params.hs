{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Utils.URI.Params(
  updateUrlParam
  ,updateUrlParams
  ,uriParams
  ,deleteQueryKey
  ,editQuery
  ,formEncodeUrl
)where

import Control.Arrow
import Network.URI
import Data.List
import Data.Function
import Network.CGI

updateUrlParam :: String -> String -> URI -> URI
updateUrlParam this value uri@(URI{uriQuery}) =
  uri { uriQuery = updated uriQuery } where
  updated = editQuery $ ((this,value):) . deleteBy ((==) `on` fst) (this,"")

deleteQueryKey :: String -> URI -> URI
deleteQueryKey key uri =
  uri { uriQuery = editQuery (filter ((/=key).fst)) (uriQuery uri) }

editQuery :: ([(String,String)] -> [(String,String)]) -> String -> String
editQuery f = ('?':) . formEncodeUrl . f . formDecode . dropWhile (=='?')

formEncodeUrl :: [(String, String)] -> String
formEncodeUrl = intercalate "&" . map keyval . map (esc *** esc)
  where keyval (key,val) = key ++ "=" ++ val
        esc = escapeURIString isAllowedInURI

updateUrlParams :: [(String,String)] -> URI -> URI
updateUrlParams = flip $ foldr $ uncurry updateUrlParam

uriParams :: URI -> [(String,String)]
uriParams = formDecode . dropWhile (=='?') . uriQuery
