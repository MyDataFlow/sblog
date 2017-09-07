{-# LANGUAGE OverloadedStrings #-}
module Handlers.Entries.Index(
  indexR
)where

import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Except (catchError)
import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status

import App.Types

import qualified Utils.BlazeExtra.Pagination as P

import Handlers.Actions.Types
import Handlers.Common

import Models.Schema
import qualified Models.DB as DB
import Views.Entries.Index

indexProcessor :: Processor PagingParams LT.Text
indexProcessor req =  do
  (es,total,tid) <- fetchData
  tags <- DB.runDBTry $ DB.fetchAllTags
  let pn = def {
    P.pnCurrentPage = (rPage req)
    ,P.pnTotal = (toInteger total)
    ,P.pnPerPage = (rCount req)
  }
  r <- render es pn tags (rTag req) tid
  return $ (status200,r)
  where
    p = fromInteger $ rPage req
    c = fromInteger $ rCount req
    withoutTag = do
      es <- DB.runDBTry  $ DB.fetchEntries True p c
      total <- DB.runDBTry $ DB.fetchEntriesCount True
      return (es,total,0)
    withTag t = do
      tid <- DB.runDBTry  $ DB.fetchTagID t
      es <- DB.runDBTry $ DB.fetchTaggedEntries tid p c
      total <- DB.runDBTry $ DB.fetchTaggedEntriesCount tid
      return (es,total,tid)
    fetchData =
      if (T.length $ rTag req) == 0
        then withoutTag
        else withTag $ rTag req

indexR :: Response ()
indexR = do
  view $ withParams $ indexProcessor
