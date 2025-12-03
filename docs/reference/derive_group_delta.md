# Add delta to reference group by NTIME

Add delta to reference group by NTIME

## Usage

``` r
derive_group_delta(cqtc, parameter = "DQTCF", reference_filter = "ACTIVE == 0")
```

## Arguments

- cqtc:

  A cqtc data set.

- parameter:

  The parameter to calculate the delta over. Defaults to DQTCF.

- reference_filter:

  The filter term to identify the control group.

## Value

A cqtc object.
