# Make a cqtc object

Create a cqtc object from a data frame, or an empty cqtc object if the
'obj' argument is NULL.

## Usage

``` r
new_cqtc(obj = NULL, silent = NULL, rr_threshold = 0.1)
```

## Arguments

- obj:

  A data frame.

- silent:

  Suppress warnings, as logical.

- rr_threshold:

  The allowed fractional deviation between the recorded RR interval and
  the RR interval back-calculated from the recorded HR.

## Value

A cqtc object from the input data set.

## Details

The minimally required fields are: \* ID, the subject ID as numeric \*
NTIME, the nominal time in hours as numeric \* CONC, the pharmacokinetic
concentration as numeric \* QTCF, the QTcF interval in ms, as numeric

Further expected fields are: \* ACTIVE, active/control treatment flag,
as logical \* QT, the QT interval in ms, as numeric \* DQTCF, the delta
QTcF to baseline in ms, as numeric \* HR, the heart rate in 1/min, as
numeric \* RR, the RR interval in ms, as numeric

If only one of HR or RR is included, the other will be derived.
