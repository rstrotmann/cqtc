# Title

Title

## Usage

``` r
auto_cqtc(
  sdtm,
  treatment = NULL,
  pc_analyte = NULL,
  eg_analyte = c(QTCF = "QTCF", HR = "HR", RR = "RR", QT = "QTGDUR"),
  egcat = "CENTRAL ECG",
  ntime_method = "TPT",
  NTIME_lookup = NULL,
  silent = NULL,
  verbose = TRUE
)
```

## Arguments

- sdtm:

  A sdtm object.

- treatment:

  The EXTRT.

- pc_analyte:

  The PCTESTCD.

- eg_analyte:

  The EGTESTCD.

- egcat:

  The EGCAT to filter observations for.

- ntime_method:

  The field to derive the nominal time from. Allowed values are 'TPT',
  'TPTNUM' and 'ELTM'. Defaults to xxTPT where xx is the domain name.

- NTIME_lookup:

  A data frame with two columns, a column that defines the custom
  nominal time information in the target domain (e.g., 'PCELTM'), and
  'NTIME'. This data frame is left_joined into the observation data
  frame to provide the NTIME field.

- silent:

  Suppress messages.

- verbose:

  Verbose messages.

## Value

A cqtc object.
