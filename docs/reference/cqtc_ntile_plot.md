# Exploratory decile plot.

Exploratory decile plot.

## Usage

``` r
cqtc_ntile_plot(
  obj,
  param = "DQTCF",
  n = 10,
  x_label = "concentration (ng/ml)",
  y_label = NULL,
  title = "",
  size = 2,
  alpha = 0.1,
  lwd = 0.6,
  loess = FALSE,
  lm = FALSE,
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

- loess:

  Show LOESS, as logical.

- lm:

  Show linear regression, as logical.

- ...:

  Further parameters to geom_point.

## Value

A ggplot object.

## Examples

``` r
cqtc_ntile_plot(dofetilide_cqtc)
```
