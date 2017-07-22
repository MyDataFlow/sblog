{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Articles.Remove(
  removeR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Control.Monad.Except (catchError)

import Network.HTTP.Types.Status
import qualified Web.Scotty.Trans as Web

import App.Types
import App.Context

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Admin.Article as VAA

data ArticleRemove = ArticleRemove {
  aid :: Integer
}
instance FormParams ArticleRemove where
  fromParams m = ArticleRemove <$>
    lookupInt "id" 0 m


removeProcessor :: Processor ArticleRemove (M.Map T.Text T.Text)
removeProcessor req =  do
  DB.runDBTry $ DB.removeBookmark $ fromInteger (aid req)
  return $ (status200,M.empty )

authUser user req =
  removeProcessor req

removeR :: Response (M.Map T.Text T.Text)
removeR = do
  Web.rescue
    (api $ withParams $ withAuthorization authUser)
    (\e ->
      if status e == unauthorized401
        then api $ return (status401,M.empty )
        else Web.raise e)
