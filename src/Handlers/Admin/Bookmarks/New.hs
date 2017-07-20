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

import qualified Views.Layout as VL
import qualified Views.Admin.Bookmark as VAB

newProcessor :: Response (Status,LT.Text)
newProcessor  =  do
    bookmark <- liftIO $ DB.defBookmark
    let writer = VAB.renderWriter bookmark "/admin/bookmarks/create"
    return $ (status200,
              VL.renderAdmin 1
                ["/bower_components/editor.md/css/editormd.min.css"]
                ["/bower_components/editor.md/editormd.min.js"
                ,"/bookmark/editor.js"]
                [writer])

authUser user req =
  if  user == "admin"
    then newProcessor
    else return (status302,"/admin/login")

newR :: Response LT.Text
newR = do
  view $ withAuthorization authUser ()
