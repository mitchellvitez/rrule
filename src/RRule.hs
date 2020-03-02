module RRule
  ( fromText
  , toText
  , defaultRRule
  , description
  , RRule(..)
  , Day(..)
  , Frequency(..)
  )
where

import Parse (parseRRule)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Text (Text, intercalate, pack, unpack)
import Types as Ty (defaultRRule, RRule(..), Day(..), Frequency(..), ToRRule(toRRule))
import Text.Megaparsec (parseMaybe)
import Data.Maybe (catMaybes, isJust)
import qualified Data.List.NonEmpty as NE (NonEmpty(..), toList)

fromText :: Text -> Maybe RRule
fromText = parseMaybe parseRRule

toText :: RRule -> Text
toText RRule{..} =
  (if prefix then "RRULE:" else "") <>
  (intercalate ";" $ catMaybes
    [ labelWith "WKST"       weekStart
    , labelWith "FREQ"       frequency
    , labelWith "COUNT"      count
    , labelWith "UNTIL"      until
    , labelWith "INTERVAL"   interval
    , labelWith "BYSECOND"   bySecond
    , labelWith "BYMINUTE"   byMinute
    , labelWith "BYHOUR"     byHour
    , labelWith "BYDAY"      byDay
    , labelWith "BYWEEKNO"   byWeekNo
    , labelWith "BYMONTH"    byMonth
    , labelWith "BYMONTHDAY" byMonthDay
    , labelWith "BYYEARDAY"  byYearDay
    , labelWith "BYSETPOS"   bySetPos
    ])

labelWith :: ToRRule a => Text -> Maybe a -> Maybe Text
labelWith _ Nothing = Nothing
labelWith label (Just x) = Just $ label <> "=" <> toRRule x

description :: RRule -> Text
description RRule{..} = intercalate " " $ catMaybes
  [ byDescription "the" ordinal "instance of" bySetPos
  , if isJust frequency then Just "every" else Nothing
  , intervalDescription =<< interval
  , frequencyDescription <$> frequency
  , byUsualDescription "second" bySecond
  , byUsualDescription "minute" byMinute
  , byUsualDescription "hour" byHour
  , byDescription "on" ordinalDay "" byDay
  , byUsualDescription "week of the year" byWeekNo
  , byDescription "in" monthDescription "" byMonth
  , byUsualDescription "day of the month" byMonthDay
  , byUsualDescription "day of the year" byYearDay
  , countDescription <$> count
  , untilDescription <$> until
  , weekStartDescription <$> weekStart
  ]

ordinal :: Int -> Text
ordinal n
  | n == -1 = "last"
  | n < 0 = ordinal (abs n) <> " from last"
  | lastDigits n == 11 = showText n <> "th"
  | lastDigits n == 12 = showText n <> "th"
  | lastDigits n == 13 = showText n <> "th"
  | lastDigit  n ==  1 = showText n <> "st"
  | lastDigit  n ==  2 = showText n <> "nd"
  | lastDigit  n ==  3 = showText n <> "rd"
  | otherwise = showText n <> "th"
  where lastDigit n = n `mod` 10
        lastDigits n = n `mod` 100

ordinalDay :: (Int, Day) -> Text
ordinalDay (0, d) = showText d
ordinalDay (n, d) = "the " <> ordinal n <> " " <> showText d

byUsualDescription :: Text -> Maybe (NE.NonEmpty Int) -> Maybe Text
byUsualDescription t = byDescription "on the" ordinal t

byDescription :: Text -> (a -> Text) -> Text -> Maybe (NE.NonEmpty a) -> Maybe Text
byDescription _ _ _ Nothing = Nothing
byDescription inOrOn toOrdinal t (Just ns) =
  Just $ inOrOn <> " " <> andedList <> timePeriod
  where andedList = intercalateAnd . map toOrdinal $ NE.toList ns
        timePeriod = if t == "" then "" else " " <> t

intercalateAnd :: [Text] -> Text
intercalateAnd [t1, t2, t3] = t1 <> ", " <> t2 <> ", and " <> t3
intercalateAnd [t1, t2] = t1 <> " and " <> t2
intercalateAnd [t] = t
intercalateAnd [] = ""
intercalateAnd (t:ts) = t <> ", " <> intercalateAnd ts

monthDescription :: Int -> Text
monthDescription = \case
  1  -> "January"
  2  -> "February"
  3  -> "March"
  4  -> "April"
  5  -> "May"
  6  -> "June"
  7  -> "July"
  8  -> "August"
  9  -> "September"
  10 -> "October"
  11 -> "November"
  12 -> "December"

showText :: Show a => a -> Text
showText = pack . show

intervalDescription :: Int -> Maybe Text
intervalDescription n = case n of
  0 -> Nothing
  1 -> Nothing
  2 -> Just "other"
  n -> Just $ ordinal n

frequencyDescription :: Frequency -> Text
frequencyDescription freq = case freq of
  Secondly -> "second"
  Minutely -> "minute"
  Hourly   -> "hour"
  Daily    -> "day"
  Weekly   -> "week"
  Monthly  -> "month"
  Yearly   -> "year"

countDescription :: Int -> Text
countDescription n = "for " <> showText n <> " occurrences"

untilDescription :: UTCTime -> Text
untilDescription t = "until " <> (pack $ formatTime defaultTimeLocale "%B %d, %Y at %H:%M:%S" t)

weekStartDescription :: Day -> Text
weekStartDescription d = "with weeks starting on " <> showText d
