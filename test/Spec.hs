import Test.Hspec
import Data.Text
import RRule

-- many of these test cases were taken from
-- https://icalendar.org/iCalendar-RFC-5545/3-8-5-3-recurrence-rule.html

main :: IO ()
main = hspec $ do
  describe "roundtrips" $ do
    it "empty" $ do
      roundTrip ""

    it "empty with prefix" $ do
      roundTrip "RRULE:"

    it "daily" $ do
      roundTrip "FREQ=DAILY"

    it "daily with prefix" $ do
      roundTrip "RRULE:FREQ=DAILY"

    it "10 times daily" $ do
      roundTrip "FREQ=DAILY;COUNT=10"

    it "daily until 2050" $ do
      roundTrip "FREQ=DAILY;UNTIL=20500101T000000Z"

    it "first of every month" $ do
      roundTrip "INTERVAL=1;FREQ=MONTHLY;BYMONTHDAY=1"

    it "every other day" $ do
      roundTrip "RRULE:FREQ=DAILY;INTERVAL=2"

    it "every 10 days for 5 times" $ do
      roundTrip "FREQ=DAILY;INTERVAL=10;COUNT=5"

    it "every day in january for 3 years by days" $ do
      roundTrip "FREQ=YEARLY;COUNT=3;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA"

    it "every day in january for 3 years" $ do
      roundTrip "FREQ=DAILY;COUNT=3;BYMONTH=1"

    it "weekly for 10 occurences" $ do
      roundTrip "RRULE:FREQ=WEEKLY;COUNT=10"

    it "weekly until December 24 1997" $ do
      roundTrip "FREQ=WEEKLY;UNTIL=19971224T000000Z"

    it "every other week starting sunday" $ do
      roundTrip "RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU"

    it "every other week starting friday" $ do
      roundTrip "RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=FR"

    it "weekly on tuesday and thursday for five weeks" $ do
      roundTrip "RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH"

    it "every other week on MWF" $ do
      roundTrip "FREQ=WEEKLY;INTERVAL=2;UNTIL=20251224T000000Z;WKST=SU;BYDAY=MO,WE,FR"

    it "every other week on Tuesday and Thursday, for 8 occurrences" $ do
      roundTrip "FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH"

    it "monthly on the first Friday for 10 occurrences" $ do
      roundTrip "FREQ=MONTHLY;COUNT=10;BYDAY=1FR"

    it "monthly on the first Friday until December 24, 1997" $ do
      roundTrip "FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR"

    it "every other month on the first and last Sunday of the month for 10 occurrences" $ do
      roundTrip "FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU"

    it "monthly on the second-to-last Monday of the month for 6 months" $ do
      roundTrip "FREQ=MONTHLY;COUNT=6;BYDAY=-2MO"

    it "monthly on the third-to-the-last day of the month, forever" $ do
      roundTrip "FREQ=MONTHLY;BYMONTHDAY=-3"

    it "monthly on the 2nd and 15th of the month for 10 occurrences" $ do
      roundTrip "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15"

    it "monthly on the first and last day of the month for 10 occurrences" $ do
      roundTrip "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1"

    it "every 18 months on the 10th thru 15th of the month for 10 occurrences" $ do
      roundTrip "FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15"

    it "every Tuesday, every other month" $ do
      roundTrip "FREQ=MONTHLY;INTERVAL=2;BYDAY=TU"

    it "yearly in June and July for 10 occurrences" $ do
      roundTrip "FREQ=YEARLY;COUNT=10;BYMONTH=6,7"

    it "every other year on January, February, and March for 10 occurrences" $ do
      roundTrip  "FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3"

    it "every third year on the 1st, 100th, and 200th day for 10 occurrences" $ do
      roundTrip "FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200"

    it "every 20th Monday of the year, forever" $ do
      roundTrip "FREQ=YEARLY;BYDAY=20MO"

    it "Monday of week number 20 (where the default start of the week is Monday), forever" $ do
      roundTrip "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO"

    it "every Thursday in March, forever" $ do
      roundTrip "FREQ=YEARLY;BYMONTH=3;BYDAY=TH"

    it "every Thursday, but only during June, July, and August, forever" $ do
      roundTrip "FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8"

    it "every Friday the 13th" $ do
      roundTrip "FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13"

    it "first Saturday that follows the first Sunday of the month" $ do
      roundTrip "FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13"

    it "Every 4 years, the first Tuesday after a Monday in November, forever (U.S. Presidential Election day)" $ do
      roundTrip "FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8"

    it "third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months" $ do
      roundTrip "FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3"

    it "second-to-last weekday of the month" $ do
      roundTrip "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2"

    it "every 3 hours from 9:00 AM to 5:00 PM on a specific day" $ do
      roundTrip "FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z"

    it "every 15 minutes for 6 occurrences" $ do
      roundTrip "FREQ=MINUTELY;INTERVAL=15;COUNT=6"

    it "every hour and a half for 4 occurrences" $ do
      roundTrip "FREQ=MINUTELY;INTERVAL=90;COUNT=4"

    it "every 20 minutes from 9:00 AM to 4:40 PM every day" $ do
      roundTrip "FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40"

    it "weekstart Monday" $ do
      roundTrip "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO"

    it "weekstart Sunday" $ do
      roundTrip "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU"

    it "seconds" $ do
      roundTrip "FREQ=SECONDLY;BYSECOND=-10,90"

roundTrip :: Text -> Expectation
roundTrip rruleText = do
  fromText <$> toText <$> fromText rruleText `shouldBe` fromText <$> Just rruleText
