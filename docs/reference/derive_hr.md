# Derive HR from RR

Derive HR from RR

## Usage

``` r
derive_hr(obj, silent = NULL)
```

## Arguments

- obj:

  A cqtc object.

- silent:

  Suppress messages.

## Value

A cqtc object with the HR field added.

## Examples

``` r
derive_hr(dofetilide_cqtc)
#> ! HR field was be replaced!
#> ----- c-QTc data set summary -----
#> Data from 44 subjects
#> 
#> QTcF observations per time point:
#>   NTIME   CONTROL   ACTIVE   NA   
#>   -0.5    22        22       NA   
#>   0.5     22        22       NA   
#>   1       22        22       NA   
#>   1.5     22        22       NA   
#>   2       22        22       NA   
#>   2.5     22        22       NA   
#>   3       22        22       NA   
#>   3.5     22        22       NA   
#>   4       22        22       NA   
#>   5       22        22       NA    
#>   (7 more rows)
#> 
#> Columns:
#>   ID, ACTIVE, NTIME, CONC, QT, QTCF, DQTCF, RR, HR 
#> 
#> Data (selected columns):
#>   ID   ACTIVE   NTIME   CONC   QT      QTCF    DQTCF   RR      HR   
#>   1    FALSE    -0.5    0      371     391.5   0       851     71   
#>   1    FALSE    0.5     0      381.3   385.5   -6      968     62   
#>   1    FALSE    1       0      368.7   391.4   -0.1    836     72   
#>   1    FALSE    1.5     0      368     389.5   -2      844     71   
#>   1    FALSE    2       0      377.3   394     2.5     878.7   68   
#>   1    FALSE    2.5     0      364     390.2   -1.3    811.7   74   
#>   1    FALSE    3       0      355     384.4   -7.1    788     76   
#>   1    FALSE    3.5     0      360.3   384.7   -6.8    823.3   73   
#>   1    FALSE    4       0      355.3   386.2   -5.3    779     77   
#>   1    FALSE    5       0      361     391.5   0       784     77    
#> 694 more rows
#> 
#> Hash: 8311ace9e980a52614a1af4e93ac890c
dplyr::select(dofetilide_cqtc, -"HR") |>
  derive_hr()
#> ℹ HR was derived from RR!
#> ----- c-QTc data set summary -----
#> Data from 44 subjects
#> 
#> QTcF observations per time point:
#>   NTIME   CONTROL   ACTIVE   NA   
#>   -0.5    22        22       NA   
#>   0.5     22        22       NA   
#>   1       22        22       NA   
#>   1.5     22        22       NA   
#>   2       22        22       NA   
#>   2.5     22        22       NA   
#>   3       22        22       NA   
#>   3.5     22        22       NA   
#>   4       22        22       NA   
#>   5       22        22       NA    
#>   (7 more rows)
#> 
#> Columns:
#>   ID, ACTIVE, NTIME, CONC, QT, QTCF, DQTCF, RR, HR 
#> 
#> Data (selected columns):
#>   ID   ACTIVE   NTIME   CONC   QT      QTCF    DQTCF   RR      HR   
#>   1    FALSE    -0.5    0      371     391.5   0       851     71   
#>   1    FALSE    0.5     0      381.3   385.5   -6      968     62   
#>   1    FALSE    1       0      368.7   391.4   -0.1    836     72   
#>   1    FALSE    1.5     0      368     389.5   -2      844     71   
#>   1    FALSE    2       0      377.3   394     2.5     878.7   68   
#>   1    FALSE    2.5     0      364     390.2   -1.3    811.7   74   
#>   1    FALSE    3       0      355     384.4   -7.1    788     76   
#>   1    FALSE    3.5     0      360.3   384.7   -6.8    823.3   73   
#>   1    FALSE    4       0      355.3   386.2   -5.3    779     77   
#>   1    FALSE    5       0      361     391.5   0       784     77    
#> 694 more rows
#> 
#> Hash: 8311ace9e980a52614a1af4e93ac890c
```
