# source: https://physionet.org/content/ecgrdvq/1.0.0/#files-panel

#' Johannesen L, Vicente J, Mason JW, Sanabria C, Waite-Labott K, Hong M, Guo P,
#' Lin J, Sørensen JS, Galeotti L, Florian J, Ugander M, Stockbridge N, Strauss
#' DG. Differentiating Drug-Induced Multichannel Block on the Electrocardiogram:
#' Randomized Study of Dofetilide, Quinidine, Ranolazine, and Verapamil. Clin
#' Pharmacol Ther. 2014 Jul 23. doi: 10.1038/clpt.2014.155.

SCR_002_Clinical_Data <- data.table::fread(
  "https://physionet.org/files/ecgrdvq/1.0.0/SCR-002.Clinical.Data.csv?download"
)

usethis::use_data(SCR_002_Clinical_Data, overwrite = TRUE)


ranolazine_cqtc <- SCR_002_Clinical_Data |>
  filter(EXTRT == "Ranolazine") |>
  mutate(ID = RANDID) |>
  mutate(NTIME = TPT) |>
  mutate(CONC = PCSTRESN) |>
  mutate(QTCF = qtcf(QT, RR)) |>
  cqtc()

usethis::use_data(ranolazine_cqtc, overwrite = TRUE)

quinidine_cqtc <- SCR_002_Clinical_Data |>
  filter(EXTRT == "Quinidine Sulph") |>
  mutate(ID = RANDID) |>
  mutate(NTIME = TPT) |>
  mutate(CONC = PCSTRESN) |>
  mutate(QTCF = qtcf(QT, RR)) |>
  cqtc()

usethis::use_data(quinidine_cqtc, overwrite = TRUE)
