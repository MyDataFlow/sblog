{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Views.Entries.Show(
  render
)where
import Control.Monad
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (MonadReader(..),asks)

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe

import qualified Web.Scotty.Trans  as Web


import Utils.Scotty.MustacheRender

import App.Types
import Models.Schema
import Views.Types
import qualified Views.Layout as VL

renderContent e = VL.renderWithTemplate "common/_content.html" e
renderBread bs title =
    return $ Breadcrumb{
      breadLinks = L.map toLink bs
      ,breadActive = title
      }
  where
    toLink (name,url) = Link (T.pack name) "" (T.pack url)
render :: [(String,String)] ->  Entry -> Response LT.Text
render bs e = do
    s <- lift $ (asks site)
    contet <- renderContent e
    bread <- renderBread bs (entryTitle e)
    VL.render
  where
    ctx name bread contet = Page {
        pageTitle = T.intercalate "-" [entryTitle e, name]
        ,pageBread = Just bread
        ,pageSEO = Nothing
        ,pageContent = LT.toStrict contet
      }
