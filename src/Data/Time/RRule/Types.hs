module Data.Time.RRule.Types
  ( defaultRRule
  , RRule(..)
  , Day(..)
  , Frequency(..)
  , ToRRule(toRRule)
  )
where

import Prelude hiding (until)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text, intercalate, pack, unpack)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

class Show a => ToRRule a where
  toRRule :: a -> Text
  toRRule = pack . show

instance ToRRule Int

instance ToRRule a => ToRRule (NonEmpty a) where
  toRRule (x :| xs) = intercalate "," $ toRRule x : map toRRule xs

instance (Show a, Integral a, ToRRule b) => ToRRule (a, b) where
  toRRule (a, b) = (if a == 0 then "" else pack $ show a) <> toRRule b

instance ToRRule UTCTime where
  toRRule = pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

instance ToRRule a => ToRRule (Maybe a) where
  toRRule Nothing = ""
  toRRule (Just a) = toRRule a

data Frequency
  = Secondly
  | Minutely
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly
  deriving (Eq, Show)

instance ToRRule Frequency where
  toRRule = \case
    Secondly -> "SECONDLY"
    Minutely -> "MINUTELY"
    Hourly   -> "HOURLY"
    Daily    -> "DAILY"
    Weekly   -> "WEEKLY"
    Monthly  -> "MONTHLY"
    Yearly   -> "YEARLY"

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Eq, Show)

instance ToRRule Day where
  toRRule = \case
    Sunday    -> "SU"
    Monday    -> "MO"
    Tuesday   -> "TU"
    Wednesday -> "WE"
    Thursday  -> "TH"
    Friday    -> "FR"
    Saturday  -> "SA"

data RRule = RRule
  { prefix     :: Bool                        -- ^ whether this rule has the "RRULE:" prefix
  , weekStart  :: Maybe Day                   -- ^ starting day of the week
  , frequency  :: Maybe Frequency             -- ^ how often to recur
  , count      :: Maybe Int                   -- ^ how many times to recur
  , until      :: Maybe UTCTime               -- ^ what UTCTime to stop recurring after
  , interval   :: Maybe Int                   -- ^ number of units to wait before recurring
  , bySecond   :: Maybe (NonEmpty Int)        -- ^ which second(s) to recur on
  , byMinute   :: Maybe (NonEmpty Int)        -- ^ which minute(s) to recur on
  , byHour     :: Maybe (NonEmpty Int)        -- ^ which hour(s) to recur on
  , byDay      :: Maybe (NonEmpty (Int, Day)) -- ^ which days(s) to recur on
  , byWeekNo   :: Maybe (NonEmpty Int)        -- ^ which week number(s) to recur on
  , byMonth    :: Maybe (NonEmpty Int)        -- ^ which month(s) to recur on
  , byMonthDay :: Maybe (NonEmpty Int)        -- ^ which day(s) of the month to recur on
  , byYearDay  :: Maybe (NonEmpty Int)        -- ^ which day(s) of the year to recur on
  , bySetPos   :: Maybe (NonEmpty Int)        -- ^ which occurrence of the rule inside the frequency period
  }
  deriving (Eq, Show)

defaultRRule :: RRule
defaultRRule = RRule
  { prefix     = False
  , weekStart  = Nothing
  , frequency  = Nothing
  , count      = Nothing
  , until      = Nothing
  , interval   = Nothing
  , bySecond   = Nothing
  , byMinute   = Nothing
  , byHour     = Nothing
  , byDay      = Nothing
  , byWeekNo   = Nothing
  , byMonth    = Nothing
  , byMonthDay = Nothing
  , byYearDay  = Nothing
  , bySetPos   = Nothing
  }
