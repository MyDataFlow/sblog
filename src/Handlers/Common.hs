{-# LANGUAGE OverloadedStrings #-}
module Handlers.Common(
  lookupIntWithDefault
  ,lookupTextWithDefault
  ,eitherToMaybe
  ,textToInt
  ,preloadUser
)where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Data.Maybe
import Data.Text.Read
import Data.Default

import App.Types
import Handlers.Actions.Common

import Models.Schemas
import qualified Models.DB as DB

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

preloadUser :: T.Text -> Response (Maybe User)
preloadUser u = do
    let userID = read $ T.unpack u
    users <- DB.runDBTry $ DB.retrieveUserByID userID
    setUser users
  where
    setUser [] = return Nothing
    setUser [user] = do
      setTplValue "user" user
      return $ Just user
