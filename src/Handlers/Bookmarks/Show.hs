{-# LANGUAGE OverloadedStrings #-}
module Handlers.Bookmarks.Show(
  showR
)where


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)
import Control.Monad.Except (catchError)

import qualified Web.Scotty.Trans  as Web
import Network.HTTP.Types.Status

import App.Types
import App.Context

import Handlers.Actions.Types
import Handlers.Actions.Common
import Handlers.Common

import qualified Models.DB as DB

import qualified Views.Bookmark as VB

data BookmarkShow = BookmarkShow {
  bid :: Integer
  ,tag :: Maybe T.Text
}


instance FormParams BookmarkShow where
  fromParams m = BookmarkShow <$>
    lookupInt "id" 0 m <*> Just (M.lookup "tag" m)

showProcessor :: Processor BookmarkShow LT.Text
showProcessor req =  do
  br <- DB.runDBTry $ DB.fetchBookmark intBid
  bs <- breadcrumbs
  host <- lift (asks siteHost)
  name <- lift (asks siteName)
  let r = VB.renderBookmark host name bs br
  return $ (status200,r)
  where
    intBid = fromInteger (bid req)
    breadcrumbs =
      case tag req of
        Nothing -> return [("书签","/bookmarks")]
        Just t -> ref $ T.unpack t
    ref t = do
      r  <- Web.header "Referer"
      case r of
        Nothing -> return [("书签","/bookmarks")]
        Just u -> return [("书签","/bookmarks") ,(t,LT.unpack u)]

showR :: Response LT.Text
showR = do
  view $ withParams $ showProcessor
