module RRule
  ( fromText
  , toText
  , defaultRRule
  , description
  , RRule(..)
  , Day(..)
  , Frequency(..)
  , ToText
  )
where

import Parse (parseRRule)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Text (Text, intercalate, pack, unpack)
import Types (defaultRRule, RRule(..), Day(..), Frequency(..), ToText(toText))
import Text.Megaparsec (parseMaybe)
import Data.Maybe (catMaybes, isJust)
import qualified Data.List.NonEmpty as NE (NonEmpty(..), toList)

fromText :: Text -> Maybe RRule
fromText = parseMaybe parseRRule

instance ToText RRule where
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

labelWith :: ToText a => Text -> Maybe a -> Maybe Text
labelWith _ Nothing = Nothing
labelWith label (Just x) = Just $ label <> "=" <> toText x

description :: RRule -> Text
description RRule{..} = intercalate " " $ catMaybes
  [ posDescription <$> bySetPos
  , if isJust frequency then Just "every" else Nothing
  , intervalDescription interval
  , frequencyDescription <$> frequency
  , byUsualDescription "second" bySecond
  , byUsualDescription "minute" byMinute
  , byUsualDescription "hour" byHour
  , byDescription "on" ordinalDay "" byDay
  , byUsualDescription "week of the year" byWeekNo
  , byDescription "in" monthName "" byMonth
  , byUsualDescription "day of the month" byMonthDay
  , byUsualDescription "day of the year" byYearDay
  , countDescription <$> count
  , untilDescription <$> until
  , weekStartDescription <$> weekStart
  ]

posDescription :: NE.NonEmpty Int -> Text
posDescription ns = "the " <> (intercalate " and " $ map ordinal $ NE.toList ns) <> " instance of"

weekStartDescription :: Day -> Text
weekStartDescription d = "with weeks starting on " <> pack (show d)

ordinal :: Int -> Text
ordinal n
  | n == -1 = "last"
  | n < 0 = ordinal (abs n) <> " from last"
  | lastDigits n == 11 = toText n <> "th"
  | lastDigits n == 12 = toText n <> "th"
  | lastDigits n == 13 = toText n <> "th"
  | lastDigit  n ==  1 = toText n <> "st"
  | lastDigit  n ==  2 = toText n <> "nd"
  | lastDigit  n ==  3 = toText n <> "rd"
  | otherwise = toText n <> "th"
  where toText n = pack (show n)
        lastDigit n = n `mod` 10
        lastDigits n = n `mod` 100

ordinalDay :: (Int, Day) -> Text
ordinalDay (0, d) = pack (show d)
ordinalDay (-1, d) = "the last " <> pack (show d)
ordinalDay (n, d) =
  "the " <> ordinal n <> " " <> pack (show d)

byUsualDescription :: Text -> Maybe (NE.NonEmpty Int) -> Maybe Text
byUsualDescription t = byDescription "on the" ordinal t

byDescription :: Text -> (a -> Text) -> Text -> Maybe (NE.NonEmpty a) -> Maybe Text
byDescription _ _ _ Nothing = Nothing
byDescription inOrOn toOrdinal t (Just ns) =
  Just $ inOrOn <> " " <> (intercalateAnd . map toOrdinal $ NE.toList ns) <> (if t == "" then "" else (" " <> t))

intercalateAnd :: [Text] -> Text
intercalateAnd [] = ""
intercalateAnd [t] = t
intercalateAnd [t1, t2, t3] = t1 <> ", " <> t2 <> ", and " <> t3
intercalateAnd [t1, t2] = t1 <> " and " <> t2
intercalateAnd (t:ts) = t <> ", " <> intercalateAnd ts

monthName :: Int -> Text
monthName = \case
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

intervalDescription :: Maybe Int -> Maybe Text
intervalDescription Nothing = Nothing
intervalDescription (Just n) = case n of
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
countDescription n = "for " <> pack (show n) <> " occurrences"

untilDescription :: UTCTime -> Text
untilDescription t = "until " <> (pack $ formatTime defaultTimeLocale "%B %d, %Y at %H:%M:%S" t)
