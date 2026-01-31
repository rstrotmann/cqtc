# Derive RR from HR

Derive RR from HR

## Usage

``` r
derive_rr(obj, silent = NULL)
```

## Arguments

- obj:

  A cqtc object.

- silent:

  Suppress messages.

## Value

A cqtc object with the RR field added.

## Examples

``` r
derive_rr(dofetilide_cqtc)
#> ! RR field was replaced!
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
#>   ID   ACTIVE   NTIME   CONC   QT      QTCF    DQTCF   RR    HR   
#>   1    FALSE    -0.5    0      371     391.5   0       845   71   
#>   1    FALSE    0.5     0      381.3   385.5   -6      968   62   
#>   1    FALSE    1       0      368.7   391.4   -0.1    833   72   
#>   1    FALSE    1.5     0      368     389.5   -2      845   71   
#>   1    FALSE    2       0      377.3   394     2.5     882   68   
#>   1    FALSE    2.5     0      364     390.2   -1.3    811   74   
#>   1    FALSE    3       0      355     384.4   -7.1    789   76   
#>   1    FALSE    3.5     0      360.3   384.7   -6.8    822   73   
#>   1    FALSE    4       0      355.3   386.2   -5.3    779   77   
#>   1    FALSE    5       0      361     391.5   0       779   77    
#> 694 more rows
#> 
#> Hash: 029d3201f15ac6da1ee07915e94e31b6
dplyr::select(dofetilide_cqtc, -"RR") |>
  derive_rr()
#> ℹ RR was derived from HR!
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
#>   ID, ACTIVE, NTIME, CONC, QT, QTCF, DQTCF, HR, RR 
#> 
#> Data (selected columns):
#>   ID   ACTIVE   NTIME   CONC   QT      QTCF    DQTCF   RR    HR   
#>   1    FALSE    -0.5    0      371     391.5   0       845   71   
#>   1    FALSE    0.5     0      381.3   385.5   -6      968   62   
#>   1    FALSE    1       0      368.7   391.4   -0.1    833   72   
#>   1    FALSE    1.5     0      368     389.5   -2      845   71   
#>   1    FALSE    2       0      377.3   394     2.5     882   68   
#>   1    FALSE    2.5     0      364     390.2   -1.3    811   74   
#>   1    FALSE    3       0      355     384.4   -7.1    789   76   
#>   1    FALSE    3.5     0      360.3   384.7   -6.8    822   73   
#>   1    FALSE    4       0      355.3   386.2   -5.3    779   77   
#>   1    FALSE    5       0      361     391.5   0       779   77    
#> 694 more rows
#> 
#> Hash: 5701310375d0b5df16c7e8c3c276c25a
```
