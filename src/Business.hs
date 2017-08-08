{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Business
    (
      Config (..)
    , loadConfig
    , isBusinessDay
    , datesBetween
    , rollForward
    , rollBackward
    , nextBusinessDay
    , previousBusinessDay
    , addBusinessDays
    , subtractBusinessDays
    , businessDaysBetween
    ) where

import GHC.Generics
import qualified Data.Text as T

import Data.Dates
  ( DateTime (DateTime)
  , WeekDay (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
  , DateInterval (Days)
  , addInterval
  , minusInterval
  , dateWeekDay
  , datesDifference
  , DateTime
  )

import Data.Dates.Formats (parseDateFormat)

import Data.Aeson
  ( FromJSON
  , parseJSON
  , withText
  , withObject
  , (.:)
  )

import Data.Yaml (decodeFileEither)

data Config = Config
  { workingDays :: [WeekDay]
  , holidays :: [DateTime]
  } deriving (Generic, Show)

instance FromJSON WeekDay where
  parseJSON = withText "WeekDay" $
    \v -> case T.unpack v of
            "monday" -> return Monday
            "tuesday" -> return Tuesday
            "wednesday" -> return Wednesday
            "thursday" -> return Thursday
            "friday" -> return Friday
            "saturday" -> return Saturday
            "sunday" -> return Sunday

instance FromJSON DateTime where
  parseJSON = withText "DateTime" $
    \v -> case parseDateFormat "YYYY-MM-DD" (T.unpack v) of
            Left err -> fail "parse error!"
            Right date -> return date

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    workingDays <- o .: "working_days"
    holidays <- o .: "holidays"
    return Config { workingDays = workingDays, holidays = holidays }

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig filePath = do
  config <- decodeFileEither filePath
  case config of
    Left err -> do
      putStrLn "Cannot parse file"
      return Nothing
    Right config -> return (Just config)

isBusinessDay :: Config -> DateTime -> Bool
isBusinessDay config date
  | dateWeekDay date `notElem` workingDays config = False
  | date `elem` holidays config = False
  | otherwise = True
  
-- Roll forward to the next business day. If the date given is a business
-- day, that day will be returned. If the day given is a holiday or
-- non-working day, the next non-holiday working day will be returned.
rollForward :: Config -> DateTime -> DateTime
rollForward config date
  | isBusinessDay config date = date
  | otherwise = rollForward config (addInterval date (Days 1))

-- Roll backward to the previous business day. If the date given is a
-- business day, that day will be returned. If the day given is a holiday or
-- non-working day, the previous non-holiday working day will be returned.
rollBackward :: Config -> DateTime -> DateTime
rollBackward config date
  | isBusinessDay config date = date
  | otherwise = rollBackward config (minusInterval date (Days 1))

-- Roll forward to the next business day regardless of whether the given
-- date is a business day or not.
nextBusinessDay :: Config -> DateTime -> DateTime
nextBusinessDay config date = rollForward config (addInterval date (Days 1))

previousBusinessDay :: Config -> DateTime -> DateTime
previousBusinessDay config date = rollBackward config (minusInterval date (Days 1))

-- Add a number of business days to a date. If a non-business day is given,
-- counting will start from the next business day. So,
--   monday + 1 = tuesday
--   friday + 1 = monday
--   sunday + 1 = tuesday
addBusinessDays :: Config -> DateTime -> Integer -> DateTime
addBusinessDays config date 0 = rollForward config date
addBusinessDays config date days
  | days < 0 = subtractBusinessDays config date (abs days)
  | otherwise = addBusinessDays config nextDate (days - 1)
  where nextDate = addInterval (rollForward config date) (Days 1)

-- Subtract a number of business days to a date. If a non-business day is
-- given, counting will start from the previous business day. So,
--   friday - 1 = thursday
--   monday - 1 = friday
--   sunday - 1 = thursday
subtractBusinessDays :: Config -> DateTime -> Integer -> DateTime
subtractBusinessDays config date 0 = rollBackward config date
subtractBusinessDays config date days
  | days < 0 = addBusinessDays config date (abs days)
  | otherwise = subtractBusinessDays config prevDate (days - 1)
  where prevDate = rollBackward config (minusInterval date (Days 1))

-- Count the number of business days between two dates.
-- This method counts from start of date1 to start of date2. So,
-- business_days_between(mon, weds) = 2 (assuming no holidays)
businessDaysBetween :: Config -> DateTime -> DateTime -> Int
businessDaysBetween config date1 date2 = length $ filter (isBusinessDay config) (datesBetween date1 date2)

-- Returns the list of dates between the two given dates (inclusive of start)
datesBetween :: DateTime -> DateTime -> [DateTime]
datesBetween start end = take (fromIntegral diff) $ iterate addOneDay start
  where addOneDay d = addInterval d (Days 1)
        diff = datesDifference start end
