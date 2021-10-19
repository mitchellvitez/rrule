module Data.Time.RRule.Parse
  ( parseRRule
  )
where
import Debug.Trace (traceShow)
import Prelude hiding (until)
import Control.Monad (msum)
import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Text (Text, intercalate, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.RRule.Types (defaultRRule, RRule(..), Day(..), Frequency(..), ToRRule, TimeOrDate(..))
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char.Lexer
import qualified Data.Time.Calendar as Cal (Day, toGregorian)

type Parser = Parsec () Text

parseRRule :: Parser RRule
parseRRule = do
  prefixText <- try . optional $ chunk "RRULE:"
  rules <- parseVariable `sepBy` single ';'
  let allRules = foldr combineRules defaultRRule rules
  return allRules { prefix = isJust prefixText }

combineRules :: RRule -> RRule -> RRule
combineRules r s = RRule
  { prefix     = prefix r || prefix s
  , weekStart  = combine weekStart
  , frequency  = combine frequency
  , count      = combine count
  , until      = combine until
  , interval   = combine interval
  , bySecond   = combine bySecond
  , byMinute   = combine byMinute
  , byHour     = combine byHour
  , byDay      = combine byDay
  , byWeekNo   = combine byWeekNo
  , byMonth    = combine byMonth
  , byMonthDay = combine byMonthDay
  , byYearDay  = combine byYearDay
  , bySetPos   = combine bySetPos
  }
  where combine f = msum [f r, f s]

parseVariable :: Parser RRule
parseVariable = do
  let prefix = False
  weekStart  <- parseVar "WKST"       parseDay
  frequency  <- parseVar "FREQ"       parseFrequency
  count      <- parseVar "COUNT"      decimal
  until      <- parseVar "UNTIL"      parseTimeOrDate
  interval   <- parseVar "INTERVAL"   decimal
  bySecond   <- parseVar "BYSECOND"   parseSomeInt
  byMinute   <- parseVar "BYMINUTE"   parseSomeInt
  byHour     <- parseVar "BYHOUR"     parseSomeInt
  byDay      <- parseVar "BYDAY"      parseSomeDay
  byWeekNo   <- parseVar "BYWEEKNO"   parseSomeInt
  byMonthDay <- parseVar "BYMONTHDAY" parseSomeInt
  byMonth    <- parseVar "BYMONTH"    parseSomeInt
  byYearDay  <- parseVar "BYYEARDAY"  parseSomeInt
  bySetPos   <- parseVar "BYSETPOS"   parseSomeInt
  return RRule{..}

parseVar :: Text -> Parser a -> Parser (Maybe a)
parseVar label parse = try . optional $ chunk label >> single '=' >> parse

parseSomeInt :: Parser (NonEmpty Int)
parseSomeInt = parseInt `NE.sepBy1` single ','

parseSomeDay :: Parser (NonEmpty (Int, Day))
parseSomeDay = parseIntDay `NE.sepBy1` single ','

parseInt :: Parser Int
parseInt = do
  sign <- try . optional $ single '-'
  d <- decimal
  return $ if isJust sign then (negate d) else d

parseIntDay :: Parser (Int, Day)
parseIntDay = do
  n <- try . optional $ parseInt
  d <- parseDay
  return (fromMaybe 0 n, d)

parseTimeOrDate :: Parser TimeOrDate
parseTimeOrDate = fmap Time (try parseUtcTime) <|> fmap Date parseDate

parseDate :: Parser Cal.Day
parseDate = do
  d <- takeP Nothing 8
  parseTimeM False defaultTimeLocale "%Y%m%d" (unpack d)

parseUtcTime :: Parser UTCTime
parseUtcTime = do
  d <- manyTill anySingle (single 'Z')
  parseTimeM False defaultTimeLocale "%Y%m%dT%H%M%S" d

parseFrequency :: Parser Frequency
parseFrequency =
  Secondly <$ chunk "SECONDLY" <|>
  Minutely <$ chunk "MINUTELY" <|>
  Hourly   <$ chunk "HOURLY"   <|>
  Daily    <$ chunk "DAILY"    <|>
  Weekly   <$ chunk "WEEKLY"   <|>
  Monthly  <$ chunk "MONTHLY"  <|>
  Yearly   <$ chunk "YEARLY"

parseDay :: Parser Day
parseDay =
  Sunday    <$ chunk "SU" <|>
  Monday    <$ chunk "MO" <|>
  Tuesday   <$ chunk "TU" <|>
  Wednesday <$ chunk "WE" <|>
  Thursday  <$ chunk "TH" <|>
  Friday    <$ chunk "FR" <|>
  Saturday  <$ chunk "SA"
