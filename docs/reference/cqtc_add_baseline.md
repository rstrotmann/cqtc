# Add baseline field for parameter across treatment groups

Add baseline field for parameter across treatment groups

## Usage

``` r
cqtc_add_baseline(
  obj,
  param = "QTCF",
  baseline_filter = "NTIME < 0",
  summary_function = mean,
  silent = NULL
)
```

## Arguments

- obj:

  A cqtc object.

- param:

  The parameter to find the baseline for.

- baseline_filter:

  A filter term to identify the baseline condition, as character.

- summary_function:

  Function to resolve multiple individual baseline values.

- silent:

  Suppress messages.

## Value

A cqtc object with the baseline columns added.
