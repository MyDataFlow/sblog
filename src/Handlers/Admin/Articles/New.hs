{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Articles.New(
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

import qualified Views.Admin.Article as VAA

newProcessor :: Response (Status,LT.Text)
newProcessor  =  do
    ar <- liftIO $ DB.defArticle
    return $ (status200,(VAA.renderWriter ar "/admin/articles/create"))

authUser user req =
  if  user == "admin"
    then newProcessor
    else return (status302,"/admin/login")

newR :: Response LT.Text
newR = do
 view $ withAuthorization authUser ()
 
