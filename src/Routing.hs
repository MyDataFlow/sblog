
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Antiblog.Routing where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (empty)
#endif
import Control.Monad.IO.Class(liftIO)
import Data.Aeson hiding (json,Number)
import Data.Aeson.Types hiding (Number)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List(isSuffixOf)
import qualified Data.Text.Lazy as T
import Data.String(IsString)
import Network.HTTP.Types.Status(forbidden403, notFound404)
import Web.Scotty hiding (body)