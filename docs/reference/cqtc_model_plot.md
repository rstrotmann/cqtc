# Plot predicted dQTcF over concentration

Plot predicted dQTcF over concentration

## Usage

``` r
cqtc_model_plot(
  obj,
  mod,
  title = NULL,
  level = 0.9,
  size = 2,
  alpha = 0.1,
  lwd = 0.6,
  loess = FALSE
)
```

## Arguments

- obj:

  A cqtc object.

- mod:

  A linear model.

- title:

  The plot title.

- level:

  The prediction interval.

- size:

  The point size.

- alpha:

  The point alpha value.

- lwd:

  The line width.

- loess:

  Show LOESS fit.

## Value

A ggplot2 object.
