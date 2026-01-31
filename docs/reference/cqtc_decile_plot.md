# Exploratory decile plot.

\#' @description \`r lifecycle::badge("deprecated")\`

## Usage

``` r
cqtc_decile_plot(
  obj,
  param = "DQTCF",
  n = 10,
  x_label = "concentration (ng/ml)",
  y_label = NULL,
  title = "",
  size = 2,
  alpha = 0.1,
  lwd = 0.6,
  ...
)
```

## Arguments

- obj:

  A cqtc object.

- param:

  The parameter to plot.

- n:

  The number of quantiles, defaults to 10.

- x_label:

  The x axis label.

- y_label:

  The y axis label.

- title:

  The plot title.

- size:

  Point size.

- alpha:

  Alpha for points.

- lwd:

  Line width for point range.

- ...:

  Further parameters to geom_point.

## Value

A ggplot object.

## Details

Please use cqtc_ntile_plot instead.
