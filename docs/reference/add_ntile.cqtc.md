# Add ntiles (quantiles) for a specific column across all subjects

Add ntiles (quantiles) for a specific column across all subjects

## Usage

``` r
# S3 method for class 'cqtc'
add_ntile(obj, input_col = "CONC", n = 10, ntile_name = NULL)
```

## Arguments

- obj:

  A cqtc object.

- input_col:

  The column to calculate quantiles over.

- n:

  The number of quantiles.

- ntile_name:

  The name of the quantile column.

## Value

A cqtc object with the ntile_name colunn added.

## Examples

``` r
head(add_ntile(dofetilide_cqtc, "CONC", 10))
#>   ID ACTIVE NTIME CONC       QT     QTCF      DQTCF       RR HR CONC_NTILE
#> 1  1  FALSE  -0.5    0 371.0000 391.5099  0.0000000 851.0000 71          1
#> 2  1  FALSE   0.5    0 381.3333 385.5113 -5.9985922 968.0000 62          1
#> 3  1  FALSE   1.0    0 368.6667 391.3640 -0.1459669 836.0000 72          1
#> 4  1  FALSE   1.5    0 368.0000 389.4623 -2.0476373 844.0000 71          1
#> 5  1  FALSE   2.0    0 377.3333 394.0007  2.4907356 878.6667 68          1
#> 6  1  FALSE   2.5    0 364.0000 390.2416 -1.2683179 811.6667 74          1
```
