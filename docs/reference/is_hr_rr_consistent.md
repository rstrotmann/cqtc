# Test whether the HR and RR fields are consistent with each other

Test whether the HR and RR fields are consistent with each other

## Usage

``` r
is_hr_rr_consistent(obj, threshold = 0.05, silent = NULL)
```

## Arguments

- obj:

  A cqtc object.

- threshold:

  The allowed relative difference between the original RR and the RR as
  recalculated from HR, defaults to 0.05.

- silent:

  Suppress messages.

## Value

A logical value.

## Examples

``` r
is_hr_rr_consistent(dofetilide_cqtc)
#> [1] TRUE
```
