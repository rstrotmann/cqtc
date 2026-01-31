# Make a cqtc object

Create a cqtc object from a data frame, or an empty cqtc object if the
'obj' argument is NULL.

## Usage

``` r
cqtc(
  obj = NULL,
  conc_field = NULL,
  silent = NULL,
  rr_inconsistency = 0.1,
  baseline_filter = NULL,
  summary_function = mean
)
```

## Arguments

- obj:

  A data frame.

- conc_field:

  The field in the input data set that represents the independent
  concentration variable, if 'CONC' is not provided.

- silent:

  Suppress warnings, as logical.

- rr_inconsistency:

  allowed relative difference between RR and the RR as back-calculated
  from HR, defaults to 0.1 (10%).

- baseline_filter:

  A filter term to identify baseline QTcF. If NULL, the 'BL_QTCF' field
  will not be added.

- summary_function:

  Summarizing function to consolidate multiple individual baseline
  values.

## Value

A cqtc object from the input data set.

## Details

If not input data is provided in the 'obj argument, an empty cqtc object
is returned. If 'obj' is a data frame, the minimally required fields
are:

\* ID, the subject ID as numeric \* NTIME, the nominal time in hours as
numeric \* CONC, the pharmacokinetic concentration as numeric \* QTCF,
the QTcF interval in ms, as numeric \* HR, the heart rate in 1/min, as
numeric \* RR, the RR interval in ms, as numeric

The following additional fields will be recognized: \* ACTIVE,
active/control treatment flag, as logical \* QT, the QT interval in ms,
as numeric \* DQTCF, the delta QTcF to baseline in ms, as numeric

If only one of HR or RR is included, the other will be derived.

If a baseline filter is provided, the following fields will be
automatically derived and added to the cqtc object:

\* BL_QTCF, the baseline QTcF value. If applying the baseline filter
results in multiple individual baseline values, they will be summarized
using the function provided by the 'summrary_function' argument. \*
DQTCF, the difference between QTCF and BL_QTCF. \* PM_BL_QTCF, the
population mean baseline QTcF value for the populations defined by
'ACTIVE' field. \* DPM_BL_QTCF, the difference between BL_QTCF and
PM_BL_QTCF.
