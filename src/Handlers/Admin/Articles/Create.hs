{-# LANGUAGE OverloadedStrings #-}
module Handlers.Admin.Articles.Create(
  createR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

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
import qualified Views.Admin.Article as VAA

data ArticleForm = ArticleForm {
  aid :: Integer
  ,title :: T.Text
  ,summary :: T.Text
  ,markdown :: T.Text
  ,body :: T.Text
  ,published :: Integer
  ,tags :: T.Text
}
instance FormParams ArticleForm where
    fromParams m = ArticleForm <$>
      lookupInt "id" 0 m <*>
      M.lookup "title"  m <*>
      M.lookup "summary" m <*>
      M.lookup "editor-markdown-doc" m <*>
      M.lookup "editor-html-code" m <*>
      lookupInt "published" 0 m <*>
      M.lookup "tags" m


createProcessor :: Processor ArticleForm LT.Text
createProcessor req =  do
    action
  where
    t = T.unpack $ title req
    s = T.unpack $ summary req
    b = T.unpack $ body req
    m = T.unpack $ markdown req
    p = published req == 1
    upackTags =
      if T.null (tags req)
        then []
        else map T.unpack $ T.split (==',') $ tags req
    action = do
      liftIO $ putStrLn $ show $ published req
      c <-  DB.runDBTry $ DB.addArticle t s m b p upackTags
      return $ (status302,"/admin/articles")

authUser user req =
  if  user == "admin"
    then createProcessor req
    else return $ (status302,"/admin/login")

createR :: Response LT.Text
createR = do
  --  view $ withParams $ withAuthorization authUser
  view $ withParams createProcessor
