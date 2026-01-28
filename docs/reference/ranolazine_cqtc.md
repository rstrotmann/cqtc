# Ranolazine c-QTc data set

A concentration-QTc analysis set for ranolazine with triplicate
observations, derived from 'SCR_002_Clinical_Data'.

## Usage

``` r
ranolazine_cqtc
```

## Format

A cqtc object, i.e., a class wrapper around a data frame with 1056 rows
and 38 columns:

- ID:

  Subject ID

- NTIME:

  Nominal time in hours

- Dofetilide:

  Dofetilide plasma concentration in ng/ml

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

Derived from
\<https://physionet.org/content/ecgrdvq/1.0.0/#files-panel\>

## References

Johannesen L, Vicente J, Mason JW, Sanabria C, Waite-Labott K, Hong M,
Guo P, Lin J, Sørensen JS, Galeotti L, Florian J, Ugander M, Stockbridge
N, Strauss DG. Differentiating Drug-Induced Multichannel Block on the
Electrocardiogram: Randomized Study of Dofetilide, Quinidine,
Ranolazine, and Verapamil. Clin Pharmacol Ther. 2014 Jul 23. doi:
10.1038/clpt.2014.155.
