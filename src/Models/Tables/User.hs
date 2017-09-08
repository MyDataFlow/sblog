{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Models.Tables.User where

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.Int
import Data.Default
import qualified Data.List as L
import qualified Data.Map as M
import Data.Time 

import Database.PostgreSQL.Simple

import App.Types
import Models.Schemas

createUser :: User -> Connection -> IO Int64
createUser user conn = do
    rs <- query conn q user
    return $ fromOnly $ head rs
  where 
    q = "INSERT INTO users (uid,name,email,avatar,created_at,updated_at) \
      \ VALUES (?,?,?,?,?,?) RETURNING id"

retrieveUserByUID :: Int64 -> Connection -> IO [User]
retrieveUserByUID uid conn = do 
    query conn q (Only uid)
  where
    q = "SELECT u.* FROM users AS u WHERE u.uid = ?"

updateUser :: User -> Connection -> IO Bool
updateUser user conn = do 
    rs <- execute conn q user 
    return $ rs == 1
  where
    q = "UPDATE users SET name = ?, SET email = ?, SET avatar = ?, \
      \ SET updated_at = ? WHERE id = ?" 
countUser :: Connection -> IO Int64
countUser conn = do
  rs <- query_ conn "SELECT count(id) FROM users"
  return $ fromOnly $ head rs