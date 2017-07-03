{-# LANGUAGE OverloadedStrings #-}

module  Models.JSON(
  encodeNewArticle
  ,encodeStoredArticle
) where
import Data.Aeson

import Models.Tables

encodeNewArticle
