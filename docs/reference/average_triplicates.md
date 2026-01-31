# Average triplicate observations

Average triplicate observations

## Usage

``` r
average_triplicates(obj)
```

## Arguments

- obj:

  A cqtc object.

## Value

A cqtc object with the HR, RR, QT, QTCF, DQTCF fields averaged over ID,
NTIME and CONC.

## Examples

``` r
average_triplicates(quinidine_cqtc)
#> ----- c-QTc data set summary -----
#> Data from 21 subjects
#> 
#> QTcF observations per time point:
#>   NTIME   ACTIVE   NA   
#>   -0.5    21       NA   
#>   0.5     21       NA   
#>   1       21       NA   
#>   1.5     21       NA   
#>   2       21       NA   
#>   2.5     20       NA   
#>   3       20       NA   
#>   3.5     20       NA   
#>   4       21       NA   
#>   5       21       NA    
#>   (7 more rows)
#> 
#> Columns:
#>   ID, NTIME, CONC, HR, RR, QT, QTCF, ACTIVE 
#> 
#> Data (selected columns):
#>   ID     ACTIVE   NTIME   CONC   QT      QTCF    RR      HR     
#>   1001   TRUE     -0.5    NA     378.3   396.8   867.3   69     
#>   1001   TRUE     0.5     1110   400.7   414.8   901.3   66.7   
#>   1001   TRUE     1       1760   431.7   479.7   728.7   82.3   
#>   1001   TRUE     1.5     1980   435     475.2   767.3   78.3   
#>   1001   TRUE     2       2070   429.3   481.1   711     84.7   
#>   1001   TRUE     2.5     1940   421.3   473.3   705.3   85.3   
#>   1001   TRUE     3       1850   422     454.8   800.7   75.3   
#>   1001   TRUE     3.5     1980   404.7   459.6   682.7   88     
#>   1001   TRUE     4       1850   403     443.7   749.7   80     
#>   1001   TRUE     5       1480   400.7   442.6   741.7   81      
#> 326 more rows
#> 
#> Hash: 55a7fe892eb668907b5830c862191f4e
```
