# Verapamil c-QTc data set

A concentration-QTc analysis set for verapamil

## Usage

``` r
verapamil_cqtc
```

## Format

A cqtc object, i.e., a class wrapper around a data frame with 704 rows
and 7 columns:

- ID:

  Subject ID

- NTIME:

  Nominal time in hours

- Verapamil:

  Verapamil plasma concentration in ng/ml

- QT:

  The QT interval in ms

- QTCF:

  The QTcF interval in ms

- DQTCF:

  The change from baseline in QTCF in ms

- RR:

  The RR interval in ms

- HR:

  Heart rate, derived from RR in the original data set

- BL_QTCF:

  Baseline QTCF, derived from QTCF at NTIME -0.5

## Source

\<https://github.com/joannaparkinson/C-QTc-hands-on-tutorial\>

## References

Parkinson, J., Dota, C. & Rekić, D. Practical guide to concentration-QTc
modeling: a hands-on tutorial. J Pharmacokinet Pharmacodyn 52, 43
(2025). https://doi.org/10.1007/s10928-025-09981-8
