#' Title
#'
#' @param sdtm A sdtm object.
#' @param treatment The EXTRT.
#' @param pc_analyte The PCTESTCD.
#' @param silent Suppress messages.
#' @param eg_analyte The EGTESTCD.
#' @param egcat The EGCAT to filter observations for.
#' @param ntime_method
#' @param NTIME_lookup
#' @param verbose Verbose messages.
#'
#' @returns A cqtc object.
#' @export
#' @import nif
auto_cqtc <- function(
    sdtm,
    treatment = NULL,
    pc_analyte = NULL,
    # eg_test = c("QTCF", "HR", "RR", "DQTC"),
    eg_analyte = c(QTCF = "QTCF", HR = "HR", RR = "RR", QT = "QTGDUR"),
    egcat = "CENTRAL ECG",
    ntime_method = "TPT",
    NTIME_lookup = NULL,
    silent = NULL,
    verbose = TRUE) {
  # validations
  nif:::validate_sdtm(sdtm, c("dm", "vs", "ex", "pc", "eg"))
  nif:::validate_char_param(treatment, "treatment", allow_null = TRUE)
  nif:::validate_char_param(pc_analyte, "pc_analyte", allow_null = TRUE)

  nif:::validate_char_param(eg_analyte, "eg_analyte", allow_multiple = TRUE)

  nif:::conditional_message(
    "ECG analytes available in domain EG:\n",
    nif:::df_to_string(
      distinct(domain(sdtm, "eg"), EGTEST, EGTESTCD),
      indent = 2),
    silent = !verbose
  )

  available_eg_analytes <- distinct(domain(sdtm, "eg"), .data$EGTEST, .data$EGTESTCD)
  missing_eg_analyte <- setdiff(eg_analyte, available_eg_analytes$EGTESTCD)
  if(length(missing_eg_analyte) > 0)
    stop(paste0(
      "ECG ", plural("parameter", length(missing_egtest) > 1),
      " not found in EG domain: ",
      nif::nice_enumeration(missing_eg_analyte)
    ))

  # print_verbose <- (!nif:::nif_option_value("silent") & !isTRUE(silent)) && verbose

  nif:::conditional_message(
    "Autogenerating c-QTc data set for study ",
    summary(sdtm)$study,
    silent = silent
  )

  # treatments and analytes
  actual_treatment <- treatment
  if(is.null(treatment)) {
    actual_treatment <- treatments(sdtm)[1]
    nif:::conditional_message(
      "Treatment not specified, using ", actual_treatment, silent = silent)
  }

  # PC observations
  actual_pc_analyte <- pc_analyte
  if(is.null(pc_analyte)) {
    available_pctestcd <- unique(domain(sdtm, "pc")$PCTESTCD)
    if(any(available_pctestcd == actual_treatment)) {
      actual_pc_analyte <- actual_treatment
    } else {
      stop(paste0(
        "PC analyte cannot be automatically assigned. ",
        "Please define pc_analyte as one of ",
        nif::nice_enumeration(available_pctestcd, conjunction = "or"),
        "!"
      ))
    }
  }

  nif <- nif() %>%
    add_administration(sdtm, actual_treatment, silent = T) %>%
    add_observation(
      sdtm, "pc", actual_pc_analyte, silent = T, duplicates = "resolve")

  for(i in 1:length(eg_analyte)) {
    testcd = eg_analyte[i]
    analyte = names(eg_analyte)[i]
    nif:::conditional_message(
      "Adding ECG observations for ", analyte, " (", testcd, ")",
      silent = !verbose)
    nif <- nif %>%
      add_observation(
        sdtm, "eg", testcd, analyte = analyte, parent = actual_pc_analyte,
        duplicates = "resolve", cat = egcat,
        ntime_method = ntime_method, silent = silent)
  }


  out <- nif::correlate_obs(nif, actual_pc_analyte, names(eg_analyte)) %>%
    mutate(CONC = .data[[actual_pc_analyte]]) %>%
    new_cqtc()

  return(out)
}
