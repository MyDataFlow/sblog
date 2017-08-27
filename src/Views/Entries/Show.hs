{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Views.Entries.Show(

)where
import Control.Monad
import Control.Monad.IO.Class(MonadIO,liftIO)

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Maybe

import qualified Web.Scotty.Trans  as Web


import Utils.Scotty.MustacheRender

import App.Types
import Models.Schema
import Views.Layout
