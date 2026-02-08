# Prediction data set

\`r lifecycle::badge("experimental")\`

This approach follows Parkinson et al.

## Usage

``` r
cqtc_prediction_dataset(fit, cqtc, level = 0.9, method = "boot")
```

## Arguments

- fit:

  A model fit object.

- cqtc:

  The qtcf object.

- level:

  The confidence level.

- method:

  The CI calculation method ("profile" or "boot").

## Value

A data frame.
