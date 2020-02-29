module Types
  ( defaultRRule
  , RRule(..)
  , Day(..)
  , Frequency(..)
  , ToText(toText)
  )
where

import Prelude hiding (until)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text, intercalate, pack, unpack)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

class Show a => ToText a where
  toText :: a -> Text
  toText = pack . show

instance ToText Int where
  toText = pack . show

instance ToText a => ToText (NonEmpty a) where
  toText (x :| xs) = intercalate "," $ toText x : map toText xs

instance (Show a, Integral a, ToText b) => ToText (a, b) where
  toText (a, b) = (if a == 0 then "" else pack $ show a) <> toText b

instance ToText UTCTime where
  toText = pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

data Frequency
  = Secondly
  | Minutely
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly
  deriving (Eq, Show)

instance ToText Frequency where
  toText = \case
    Secondly -> "SECONDLY"
    Minutely -> "MINUTELY"
    Hourly   -> "HOURLY"
    Daily    -> "DAILY"
    Weekly   -> "WEEKLY"
    Monthly  -> "MONTHLY"
    Yearly   -> "YEARLY"

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Eq, Show)

instance ToText Day where
  toText = \case
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
