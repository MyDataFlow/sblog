{-# LANGUAGE OverloadedStrings #-}
module Handlers.Common(
  lookupIntWithDefault
  ,lookupTextWithDefault
  ,eitherToMaybe
  ,PagingParams(..)
  ,EntryParams(..)
)where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.Maybe
import Data.Text.Read
import Data.Default

import App.Types
import Handlers.Actions.Common

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right val) = Just val

textToInt :: T.Text -> Maybe Integer
textToInt t =
  let
    eitherPair = decimal t
    maybePair = eitherToMaybe eitherPair
  in
    fst <$> maybePair

lookupIntWithDefault :: T.Text -> Integer -> (M.Map T.Text  T.Text ) -> Maybe Integer
lookupIntWithDefault k v m =
  case M.lookup k m of
    Nothing -> Just v
    Just text -> textToInt text
lookupTextWithDefault :: T.Text -> T.Text -> (M.Map T.Text  T.Text ) -> Maybe T.Text
lookupTextWithDefault k v m =
  case M.lookup k m of
    Nothing -> Just v
    value -> value

data PagingParams = PagingParams {
  rPage :: Integer
  ,rCount :: Integer
  ,rTag :: T.Text
}

instance FromParams PagingParams where
  fromParams m = PagingParams <$>
    lookupIntWithDefault "_page" 1 m <*>
    lookupIntWithDefault "_count" 20 m <*>
    lookupTextWithDefault "tag" "" m

data EntryParams = EntryParams {
  eID :: Integer
  ,eTag :: T.Text
}
instance FromParams EntryParams where
  fromParams m = EntryParams <$>
    lookupIntWithDefault "id" 0 m <*>
    lookupTextWithDefault "tag" "" m
