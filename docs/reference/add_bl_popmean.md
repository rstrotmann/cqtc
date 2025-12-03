# Add population mean column

Add population mean column

## Usage

``` r
add_bl_popmean(cqtc, param = "BL_QTCF", silent = NULL)
```

## Arguments

- cqtc:

  A cqtc object.

- param:

  The column name for the baseline value.

- silent:

  Suppress messages.

## Value

a cqtc object with the population mean column(n) added for the specified
parameters. The name(s) follow the naming "PM_xx", where "xx" is the
parameter name.
