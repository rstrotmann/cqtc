# Plot predicted dQTcF over concentration

Plot predicted dQTcF over concentration

## Usage

``` r
cqtc_model_plot(
  obj,
  mod,
  x_label = "concentration (ng/ml)",
  y_label = "dQTcF",
  title = NULL,
  level = 0.9,
  size = 2,
  alpha = 0.1,
  lwd = 0.6,
  loess = FALSE,
  refline = NULL
)
```

## Arguments

- obj:

  A cqtc object.

- mod:

  A linear model.

- x_label:

  The x axis label.

- y_label:

  The y axis label.

- title:

  The plot title.

- level:

  The prediction interval.

- size:

  Point size.

- alpha:

  Alpha for points.

- lwd:

  Line width for point range.

- loess:

  Show LOESS, as logical.

- refline:

  Plot horizontal dashed reference lines at these y axis values,
  defaults to NULL (no lines).

## Value

A ggplot2 object.
