# Identify entries with inconsistent RR and HR fields.

Identify entries with inconsistent RR and HR fields.

## Usage

``` r
find_inconsistent_rr_hr(obj, threshold = 0.1)
```

## Arguments

- obj:

  A cqtc object.

- threshold:

  The allowed fractional deviation.

## Value

A data frame with entries in which the reported RR and the RR
re-calculated from the reported HR (i.e., rr_recalc) are different by
the specified threshold.

## Examples

``` r
find_inconsistent_rr_hr(dofetilide_cqtc, 0.01)
#>   ID ACTIVE NTIME CONC       QT     QTCF   DQTCF   RR HR RR_recalc RR_delta
#> 1 38   TRUE     2 2.41 484.6667 451.6164 61.4337 1237 49   1224.49 -12.5102
```
