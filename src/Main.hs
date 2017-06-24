{-# LANGUAGE OverloadedStrings #-}

module Main
(
    main
)
where

import Control.Monad
import System.IO (BufferMode (..),hSetBuffering,stderr,stdout,stdin)


import Web.Scotty as S

import Config
import qualified Models.DB as DB
import qualified Views.TagsView as TagsView
import qualified Views.ArticlesView as ArticlesView
import qualified Views.Layout as Layout

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin  LineBuffering
    hSetBuffering stderr NoBuffering
    conf <- readOptions
    db <- DB.createConnections conf
    tags <- DB.fetchTags db
    articles <- DB.fetchArticles db
    S.scotty (port conf) $ do
        get "/" $ do
            let tagsView = TagsView.render tags
            let articlesView = ArticlesView.render articles
            S.html (Layout.render [articlesView,tagsView])
    return ()
