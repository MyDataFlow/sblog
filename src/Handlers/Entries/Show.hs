{-# LANGUAGE OverloadedStrings #-}
module Handlers.Entries.Show(
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

import Utils.URI.String
import Utils.URI.Params
import qualified Utils.BlazeExtra.Pagination as P

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import Models.Schema
import qualified Models.DB as DB
import Views.Entries.Show

showProcessor :: Processor EntryParams LT.Text
showProcessor req =  do
  er <- DB.runDBTry $ DB.fetchEntry intBid
  bs <- breadcrumbs
  r <- render bs er
  return $ (status200,r)
  where
    intBid = fromInteger (eID req)
    breadcrumbs =
      if (T.length $ eTag req)  == 0
        then return [("博文","/entries")]
        else ref $ T.unpack $ eTag req
    ref t = do
      r  <- Web.header "Referer"
      case r of
        Nothing -> return [("博文","/entries"),(t,tagURI t)]
        Just u -> return [("博文","/entries") ,(t,LT.unpack u)]
    tagURI t = show $ updateUrlParam "tag" t (toURI $ "/entries")


indexR :: Response LT.Text
indexR = do
  view $ withParams $ showProcessor
