# Hysteresis plot

Hysteresis plot

## Usage

``` r
cqtc_hysteresis_plot(obj, param = "QTCF", ...)
```

## Arguments

- obj:

  A cqtc object.

- param:

  The parameter to plot on the y axis.

- ...:

  Further parameters to geom_point.

## Value

A ggplot object.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(magrittr)

dofetilide_cqtc |>
  filter(ACTIVE == 1) |>
  cqtc_hysteresis_plot()

```
