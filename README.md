# rrule

## Recurrence rule parser and formatter

`fromText` parses from a recurrence rule Text to the RRule type

```haskell
> let rule = fromJust $ fromText "RRULE:FREQ=MONTHLY;INTERVAL=1;BYMONTHDAY=1"
> rule
RRule {prefix = True, frequency = Just Monthly, byMonthDay = Just (1 :| []), ... }
```

`toText` formats an RRule to a recurrence rule Text

```haskell
> toText rule
"RRULE:FREQ=MONTHLY;INTERVAL=1;BYMONTHDAY=1"
```

`description` gives a description of what an RRule means, in English

```haskell
> description rule
"every month on the 1st day of the month"
```

## Possible future additions 

- `byEaster` as seen in the Python `dateutil` rrule implementation
- `toList :: RRule -> [UTCTime]` - a possibly-infinite list of all the `UTCTime`s described by an `RRule`
- Parse the numerals 0-6 as valid `Day`s
- Cover more of the iCalendar spec, for example handling the `DTSTART` flag, or `EXDATE` exceptions
- `before :: UTCTime -> RRule -> UTCTime` - get the last UTCTime before the given UTCTime satisfying this rule
- `after :: UTCTime -> RRule -> UTCTime` - get the first UTCTime after the given UTCTime satisfying this rule
- `between` to get the UTCTime(s) satisfying a rule and between two other UTCTimes
