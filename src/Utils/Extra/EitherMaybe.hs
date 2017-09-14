module Utils.Extra.EitherMaybe(
  eitherToMaybe
)where
import Data.Maybe
import Data.Either

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right val) = Just val
