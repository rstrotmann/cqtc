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
