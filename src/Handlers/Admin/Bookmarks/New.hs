{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Bookmarks.New(
  newR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Except (catchError)

import Network.HTTP.Types.Status

import App.Types
import App.Context

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Admin.Bookmark as VAB

newProcessor :: Response (Status,LT.Text)
newProcessor  =  do
    bookmark <- liftIO $ DB.defBookmark
    return $ (status200,
      (VAB.renderWriter bookmark "/admin/bookmarks/create"))

authUser user req =
  if  user == "admin"
    then newProcessor
    else return (status302,"/admin/login")

newR :: Response LT.Text
newR = do
  view $ withAuthorization authUser ()
  
