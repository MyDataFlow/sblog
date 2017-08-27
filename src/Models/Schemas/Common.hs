{-# LANGUAGE OverloadedStrings #-}
module Models.Schemas.Common(
  localTimeNow
  ,utcDateString
  ,stringToDate
  ,defaultDate
)where

import Data.Time

localTimeNow :: IO LocalTime
localTimeNow = do
  u <- getCurrentTime
  return $ utcToLocalTime utc u

utcDateString :: LocalTime -> String
utcDateString t =
    formatTime defaultTimeLocale "%Y-%m-%d" $ time t
  where
    time t = localTimeToUTC utc t

stringToDate :: String -> LocalTime
stringToDate t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" t

defaultDate :: LocalTime
defaultDate = stringToDate "1970-01-01"
