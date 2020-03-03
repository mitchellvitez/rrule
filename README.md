# rrule

## Recurrence rule parser and formatter

`fromText` parses from a recurrence rule `Text` to the `RRule` type

```haskell
> let rule = fromJust $ fromText "RRULE:FREQ=MONTHLY;INTERVAL=1;BYMONTHDAY=1"
> rule
RRule {prefix = True, frequency = Just Monthly, byMonthDay = Just (1 :| []), ... }
```

`toText` formats an `RRule` to a recurrence rule `Text`

```haskell
> toText rule
"RRULE:FREQ=MONTHLY;INTERVAL=1;BYMONTHDAY=1"
```

`description` gives a description of what an `RRule` means, in English

```haskell
> description rule
"every month on the 1st day of the month"
```
