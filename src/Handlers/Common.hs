{-# LANGUAGE OverloadedStrings #-}
module Handlers.Common where

import Data.Maybe
import Data.Text.Read
import Data.Text
import qualified Data.Map as M

import Network.URI
import qualified Text.Blaze.Html5 as H

import App.Types
import App.Context

import qualified Models.DB as DB


toUrl :: String -> URI
toUrl u = fromJust $ parseRelativeReference u

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right val) = Just val

textToInt :: Text -> Maybe Integer
textToInt t =
  let
    eitherPair = decimal t
    maybePair = eitherToMaybe eitherPair
  in
    fst <$> maybePair

lookupInt :: Text -> Integer -> (M.Map Text Text) -> Maybe Integer
lookupInt k v m =
  case M.lookup k m of
    Nothing -> Just v
    Just text -> textToInt text
