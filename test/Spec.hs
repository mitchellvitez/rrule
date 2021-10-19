import Test.Hspec
import Data.Text
import Data.Time.RRule

-- many of these test cases were taken from
-- https://icalendar.org/iCalendar-RFC-5545/3-8-5-3-recurrence-rule.html

roundTrip :: Text -> Expectation
roundTrip rruleText = do
  fromText <$> toText <$> fromText rruleText `shouldBe` fromText <$> Just rruleText

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

    it "every month on Tuesday until November 30, 2021" $ do
      roundTrip "RRULE:BYDAY=TU;INTERVAL=1;FREQ=MONTHLY;UNTIL=20211130"

    it "every other week on Friday until November 19, 1997" $ do
      roundTrip "RRULE:BYDAY=FR;INTERVAL=2;FREQ=WEEKLY;UNTIL=19971119"

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

    it "every 3 hours until 5:00 PM on a specific day" $ do
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

  describe "describes" $ do
    it "empty" $ do
      description <$> fromText "" `shouldBe`
        Just ""

    it "empty with prefix" $ do
      description <$> fromText "RRULE:" `shouldBe`
        Just ""

    it "daily" $ do
      description <$> fromText "FREQ=DAILY" `shouldBe`
        Just "every day"

    it "daily with prefix" $ do
      description <$> fromText "RRULE:FREQ=DAILY" `shouldBe`
        Just "every day"

    it "10 times daily" $ do
      description <$> fromText "FREQ=DAILY;COUNT=10" `shouldBe`
        Just "every day for 10 occurrences"

    it "daily until 2050" $ do
      description <$> fromText "FREQ=DAILY;UNTIL=20500101T000000Z" `shouldBe`
        Just "every day until January 01, 2050 at 00:00:00"

    it "first of every month" $ do
      description <$> fromText "INTERVAL=1;FREQ=MONTHLY;BYMONTHDAY=1" `shouldBe`
        Just "every month on the 1st day of the month"

    it "every other day" $ do
      description <$> fromText "RRULE:FREQ=DAILY;INTERVAL=2" `shouldBe`
        Just "every other day"

    it "every 10 days for 5 times" $ do
      description <$> fromText "FREQ=DAILY;INTERVAL=10;COUNT=5" `shouldBe`
        Just "every 10th day for 5 occurrences"

    it "every day in january for 3 years by days" $ do
      description <$> fromText "FREQ=YEARLY;COUNT=3;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA" `shouldBe`
        Just "every year on Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, and Saturday in January for 3 occurrences"

    it "every day in january for 3 years" $ do
      description <$> fromText "FREQ=DAILY;COUNT=3;BYMONTH=1" `shouldBe`
        Just "every day in January for 3 occurrences"

    it "weekly for 10 occurences" $ do
      description <$> fromText "RRULE:FREQ=WEEKLY;COUNT=10" `shouldBe`
        Just "every week for 10 occurrences"

    it "weekly until December 24 1997" $ do
      description <$> fromText "FREQ=WEEKLY;UNTIL=19971224T000000Z" `shouldBe`
        Just "every week until December 24, 1997 at 00:00:00"

    it "every other week starting sunday" $ do
      description <$> fromText "RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU" `shouldBe`
        Just "every other week with weeks starting on Sunday"

    it "every other week starting friday" $ do
      description <$> fromText "RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=FR" `shouldBe`
        Just "every other week with weeks starting on Friday"

    it "weekly on tuesday and thursday for five weeks" $ do
      description <$> fromText "RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH" `shouldBe`
        Just "every week on Tuesday and Thursday for 10 occurrences with weeks starting on Sunday"

    it "every other week on MWF" $ do
      description <$> fromText "FREQ=WEEKLY;INTERVAL=2;UNTIL=20251224T000000Z;WKST=SU;BYDAY=MO,WE,FR" `shouldBe`
        Just "every other week on Monday, Wednesday, and Friday until December 24, 2025 at 00:00:00 with weeks starting on Sunday"

    it "every other week on Tuesday and Thursday, for 8 occurrences" $ do
      description <$> fromText "FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH" `shouldBe`
        Just "every other week on Tuesday and Thursday for 8 occurrences with weeks starting on Sunday"

    it "monthly on the first Friday for 10 occurrences" $ do
      description <$> fromText "FREQ=MONTHLY;COUNT=10;BYDAY=1FR" `shouldBe`
        Just "every month on the 1st Friday for 10 occurrences"

    it "monthly on the first Friday until December 24, 1997" $ do
      description <$> fromText "FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR" `shouldBe`
        Just "every month on the 1st Friday until December 24, 1997 at 00:00:00"

    it "every other month on the first and last Sunday of the month for 10 occurrences" $ do
      description <$> fromText "FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU" `shouldBe`
        Just "every other month on the 1st Sunday and the last Sunday for 10 occurrences"

    it "monthly on the second-to-last Monday of the month for 6 months" $ do
      description <$> fromText "FREQ=MONTHLY;COUNT=6;BYDAY=-2MO" `shouldBe`
        Just "every month on the 2nd from last Monday for 6 occurrences"

    it "monthly on the third-to-the-last day of the month, forever" $ do
      description <$> fromText "FREQ=MONTHLY;BYMONTHDAY=-3" `shouldBe`
        Just "every month on the 3rd from last day of the month"

    it "monthly on the 2nd and 15th of the month for 10 occurrences" $ do
      description <$> fromText "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15" `shouldBe`
        Just "every month on the 2nd and 15th day of the month for 10 occurrences"

    it "monthly on the first and last day of the month for 10 occurrences" $ do
      description <$> fromText "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1" `shouldBe`
        Just "every month on the 1st and last day of the month for 10 occurrences"

    it "every 18 months on the 10th thru 15th of the month for 10 occurrences" $ do
      description <$> fromText "FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15" `shouldBe`
        Just "every 18th month on the 10th, 11th, 12th, 13th, 14th, and 15th day of the month for 10 occurrences"

    it "every Tuesday, every other month" $ do
      description <$> fromText "FREQ=MONTHLY;INTERVAL=2;BYDAY=TU" `shouldBe`
        Just "every other month on Tuesday"

    it "yearly in June and July for 10 occurrences" $ do
      description <$> fromText "FREQ=YEARLY;COUNT=10;BYMONTH=6,7" `shouldBe`
        Just "every year in June and July for 10 occurrences"

    it "every other year on January, February, and March for 10 occurrences" $ do
      description <$> fromText  "FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3" `shouldBe`
        Just "every other year in January, February, and March for 10 occurrences"

    it "every third year on the 1st, 100th, and 200th day for 10 occurrences" $ do
      description <$> fromText "FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200" `shouldBe`
        Just "every 3rd year on the 1st, 100th, and 200th day of the year for 10 occurrences"

    it "every 20th Monday of the year, forever" $ do
      description <$> fromText "FREQ=YEARLY;BYDAY=20MO" `shouldBe`
        Just "every year on the 20th Monday"

    it "Monday of week number 20 where the default start of the week is Monday, forever" $ do
      description <$> fromText "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO" `shouldBe`
        Just "every year on Monday on the 20th week of the year"

    it "every Thursday in March, forever" $ do
      description <$> fromText "FREQ=YEARLY;BYMONTH=3;BYDAY=TH" `shouldBe`
        Just "every year on Thursday in March"

    it "every Thursday, but only during June, July, and August, forever" $ do
      description <$> fromText "FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8" `shouldBe`
        Just "every year on Thursday in June, July, and August"

    it "every Friday the 13th" $ do
      description <$> fromText "FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13" `shouldBe`
        Just "every month on Friday on the 13th day of the month"

    it "first Saturday that follows the first Sunday of the month" $ do
      description <$> fromText "FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13" `shouldBe`
        Just "every month on Saturday on the 7th, 8th, 9th, 10th, 11th, 12th, and 13th day of the month"

    it "Every 4 years, the first Tuesday after a Monday in November, forever U.S. Presidential Election day" $ do
      description <$> fromText "FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8" `shouldBe`
        Just "every 4th year on Tuesday in November on the 2nd, 3rd, 4th, 5th, 6th, 7th, and 8th day of the month"

    it "third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months" $ do
      description <$> fromText "FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3" `shouldBe`
        Just "the 3rd instance of every month on Tuesday, Wednesday, and Thursday for 3 occurrences"

    it "second-to-last weekday of the month" $ do
      description <$> fromText "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2" `shouldBe`
        Just "the 2nd from last instance of every month on Monday, Tuesday, Wednesday, Thursday, and Friday"

    it "every 3 hours until 5:00 PM on a specific day" $ do
      description <$> fromText "FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z" `shouldBe`
        Just "every 3rd hour until September 02, 1997 at 17:00:00"

    it "every month on Tuesday until November 30, 2021" $ do
      description <$> fromText "RRULE:BYDAY=TU;INTERVAL=1;FREQ=MONTHLY;UNTIL=20211130" `shouldBe`
        Just "every month on Tuesday until November 30, 2021"

    it "every other week on Friday until November 19, 1997" $ do
      description <$> fromText "RRULE:BYDAY=FR;INTERVAL=2;FREQ=WEEKLY;UNTIL=19971119" `shouldBe`
        Just "every other week on Friday until November 19, 1997"

    it "every 15 minutes for 6 occurrences" $ do
      description <$> fromText "FREQ=MINUTELY;INTERVAL=15;COUNT=6" `shouldBe`
        Just "every 15th minute for 6 occurrences"

    it "every hour and a half for 4 occurrences" $ do
      description <$> fromText "FREQ=MINUTELY;INTERVAL=90;COUNT=4" `shouldBe`
        Just "every 90th minute for 4 occurrences"

    it "every 20 minutes from 9:00 AM to 4:40 PM every day" $ do
      description <$> fromText "FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40" `shouldBe`
        Just "every day on the 0th, 20th, and 40th minute on the 9th, 10th, 11th, 12th, 13th, 14th, 15th, and 16th hour"

    it "weekstart Monday" $ do
      description <$> fromText "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO" `shouldBe`
        Just "every other week on Tuesday and Sunday for 4 occurrences with weeks starting on Monday"

    it "weekstart Sunday" $ do
      description <$> fromText "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU" `shouldBe`
        Just "every other week on Tuesday and Sunday for 4 occurrences with weeks starting on Sunday"

    it "seconds" $ do
      description <$> fromText "FREQ=SECONDLY;BYSECOND=-10,90" `shouldBe`
        Just "every second on the 10th from last and 90th second"
