module Utils.Extra.TextMap(
  integerWithDefault
  ,textWithDefault
)where

import Data.Either
import qualified Data.Text as T
import Data.Text.Read
import qualified Data.Map as M

textToInt :: T.Text -> Integer  -> Integer
textToInt t v =
  case  decimal t of
    Left _ -> v
    Right value -> fst $ value

integerWithDefault :: T.Text -> Integer -> (M.Map T.Text  T.Text ) -> Integer
integerWithDefault k v m =
  case M.lookup k m of
    Nothing -> v
    Just text -> textToInt text v

textWithDefault :: T.Text -> T.Text -> (M.Map T.Text  T.Text ) -> T.Text
textWithDefault k v m =
  case M.lookup k m of
    Nothing ->  v
    Just value -> value