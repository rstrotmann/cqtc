# Create cqtc object

Create cqtc object

## Usage

``` r
cqtc(obj = NULL, conc_field = NULL, silent = NULL, rr_threshold = 0.1)
```

## Arguments

- obj:

  A data frame.

- conc_field:

  The field in the input data set that represents the independent
  concentration variable, if 'CONC' is not provided.

- silent:

  Suppress warnings, as logical.

- rr_threshold:

  The allowed fractional deviation between the recorded RR interval and
  the RR interval back-calculated from the recorded HR.

## Value

A cqtc object.
