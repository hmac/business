module Business
    (
      isBusinessDay
    , datesBetween
    ) where

import Data.Dates

isBusinessDay :: DateTime -> Bool
isBusinessDay = undefined
-- isBusinessDay date
--   | dateWeekDay date `notElem` workingDays = false
--   | date `elem` holidays = false
--   | otherwise = true
  
-- Roll forward to the next business day. If the date given is a business
-- day, that day will be returned. If the day given is a holiday or
-- non-working day, the next non-holiday working day will be returned.
rollForward :: DateTime -> DateTime
rollForward date
  | isBusinessDay date = date
  | otherwise = rollForward (addInterval date (Days 1))

-- Roll backward to the previous business day. If the date given is a
-- business day, that day will be returned. If the day given is a holiday or
-- non-working day, the previous non-holiday working day will be returned.
rollBackward :: DateTime -> DateTime
rollBackward date
  | isBusinessDay date = date
  | otherwise = rollForward (minusInterval date (Days 1))

-- Roll forward to the next business day regardless of whether the given
-- date is a business day or not.
nextBusinessDay :: DateTime -> DateTime
nextBusinessDay date = rollForward (addInterval date (Days 1))

previousBusinessDay :: DateTime -> DateTime
previousBusinessDay date = rollBackward (minusInterval date (Days 1))

-- Add a number of business days to a date. If a non-business day is given,
-- counting will start from the next business day. So,
--   monday + 1 = tuesday
--   friday + 1 = monday
--   sunday + 1 = tuesday
addBusinessDays :: DateTime -> DateInterval -> DateTime
addBusinessDays date = addInterval (rollForward date)

-- Subtract a number of business days to a date. If a non-business day is
-- given, counting will start from the previous business day. So,
--   friday - 1 = thursday
--   monday - 1 = friday
--   sunday - 1 = thursday
subtractBusinessDays :: DateTime -> DateInterval -> DateTime
subtractBusinessDays date = minusInterval (rollBackward date)

-- Count the number of business days between two dates.
-- This method counts from start of date1 to start of date2. So,
-- business_days_between(mon, weds) = 2 (assuming no holidays)
businessDaysBetween :: DateTime -> DateTime -> Int
businessDaysBetween date1 date2 = length $ filter isBusinessDay (datesBetween date1 date2)

-- Returns the list of dates between the two given dates (inclusive of start)
datesBetween :: DateTime -> DateTime -> [DateTime]
datesBetween start end = take (fromIntegral diff) $ iterate addOneDay start
  where addOneDay d = addInterval d (Days 1)
        diff = datesDifference start end
