{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Bookmarks.Remove(
  removeR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Control.Monad.Except (catchError)

import Network.HTTP.Types.Status

import App.Types
import App.Context

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Layout as VL
import qualified Views.Admin.Bookmark as VAB

data BookmarkRemove = BookmarkRemove {
  bid :: Integer
}
instance FormParams BookmarkRemove where
    fromParams m = BookmarkRemove <$>
      lookupInt "id" 0 m


removeProcessor :: Processor BookmarkRemove (M.Map T.Text T.Text)
removeProcessor req =  do
  return $ (status200,M.empty )

authUser user req =
  removeProcessor req

removeR :: Response (M.Map T.Text T.Text)
removeR = do
  catchError
    (api $ withParams $ withAuthorization authUser)
    (\e -> api $ return (status401,M.empty ) )
