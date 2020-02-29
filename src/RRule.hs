module RRule
  ( fromText
  , toText
  )
where

import Parse (parseRRule)
import Data.Text (Text, intercalate, pack, unpack)
import Types (defaultRRule, RRule(..), Day(..), Frequency(..), ToText(toText))
import Text.Megaparsec (parseMaybe)
import Data.Maybe (catMaybes)

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
