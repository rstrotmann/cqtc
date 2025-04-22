in_range <- function(val, ref, window_low, window_high) {
  val >= ref - window_low & val < ref + window_high
}

#' Make EG nif object from SDTM data
#'
#' @param sdtm A sdtm object.
#' @param silent Suppress messages, as logical.
#'
#' @returns A data frame.
#' @import nif
#' @import dplyr
eg_nif_from_sdtm <- function(
    sdtm,
    extrt,
    pc_ntl = NULL,
    eg_ntl = NULL,
    pctestcd = NULL,
    silent = NULL
  ) {
  # Validate inputs
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  required_domains <- c("eg", "dm", "vs", "pc", "ex")
  missing_domains <- setdiff(required_domains, names(sdtm$domains))
  if(length(missing_domains) > 0)
    stop(paste0(
      "Missing ", nif::plural("domain", length(missing_domains) > 1), ": ",
      nif::nice_enumeration(missing_domains)))

  # Validate EG fields
  eg <- nif::domain(sdtm, "eg")

  # required_fields <- c("HR", "RR", "QTCF")
  required_fields <- c("HR", "QTCF")

  missing_fields <- setdiff(required_fields, unique(eg$EGTESTCD))
  if(length(missing_fields) > 0)
    stop("Missing EGTESTCD in domain EG: ", nif::nice_enumeration(missing_fields))

  if(all(c("EGTPTNUM", "EGDY") %in% names(eg))) {
    eg <- eg %>%
      mutate(NTIME = nif:::trialday_to_day(EGDY) * 24 +
               ifelse(is.na(EGTPTNUM), 0, EGTPTNUM))
  } else {
    stop(paste0("EG domain does not provide both of EGTPTNUM and EGDY. ",
    "NTIME cannot derived. Please provide a lookup table in ntl_eg!"))
  }

  sdtm$domains$eg <- eg %>%
    mutate(EGELTM = EGTPT)

  # Validate PC fields
  if(is.null(pctestcd)) {
    pctestcd <- extrt
  }

  # Create nif object
  nif <- nif::new_nif() %>%
    nif::add_administration(sdtm, extrt, silent = silent) %>%
    nif::add_observation(sdtm, "pc", pctestcd, cmt = 2,
                         NTIME_lookup = pc_ntl,
                         include_day_in_ntime = T,
                         duplicates = "resolve",
                         silent = silent) %>%
    nif::add_observation(sdtm, "eg", "HR", cmt = 3,
                         # include_day_in_ntime = TRUE,
                         NTIME_lookup = eg_ntl,
                         duplicates = "resolve",
                         silent = silent) %>%
    nif::add_observation(sdtm, "eg", "RR", cmt = 4,
                         # include_day_in_ntime = TRUE,
                         NTIME_lookup = eg_ntl,
                         duplicates = "resolve",
                         silent = silent) %>%
    nif::add_observation(sdtm, "eg", "QTCF", cmt = 5,
                         # include_day_in_ntime = TRUE,
                         NTIME_lookup = eg_ntl,
                         duplicates = "resolve",
                         silent = silent)

  return(nif)
}


#' Find matching ECG and concentration measurements
#'
#' @description
#' Finds time-matched pairs between ECG (QTCF) measurements and concentration
#' (PK) measurements based on proximity in time.
#'
#' @param nif A nif object with observations for the conc_analyte and the
#' ecg_analyte.
#' @param conc_analyte The concentration analyte.
#' @param ecg_analyte The ECG parameter analyte.
#' @param time_window The allowed time window.
#'
#' @returns
#' @export
find_time_matched <- function(
    nif,
    conc_analyte = NULL,
    ecg_analyte = "QTCF",
    time_window = 10/60
    ) {
  # Input validation
  if (!inherits(nif, "nif") && !is.data.frame(nif)) {
    stop("Input must be a nif object or data frame")
  }

  if(is.null(conc_analyte))
    conc_analyte <- nif:::guess_analyte(nif)

  # analyte validation
  if(!conc_analyte %in% analytes(nif))
    stop(paste0(conc_analyte , " not found in analytes!"))

  if(!ecg_analyte %in% analytes(nif))
    stop(paste0(ecg_analyte , " not found in analytes!"))


  eg <- nif %>%
    filter(ANALYTE == ecg_analyte,
           EVID == 0)

  pc <- nif %>%
    filter(ANALYTE == conc_analyte,
           EVID == 0)

  # Check if we have data to work with
  if (nrow(pc) == 0 || nrow(eg) == 0) {
    warning("No data found for specified analyte or ECG parameter")
    return(data.frame(eg_REF = numeric(0), pc_REF = numeric(0)))
  }

  out = data.frame()

  for(i in 1:length(eg)){
    e <- eg[i, ]
    temp <- pc %>%
      filter(USUBJID == e$USUBJID) %>%
      mutate(delta_time = as.numeric(difftime(DTC, e$DTC, unit = "hours"))) %>%
      filter(delta_time == min(abs(delta_time))) %>%
      filter(delta_time < time_window) %>%
      select(pc_DTC = DTC, pc_REF = REF, pc_DV = DV)
    if(nrow(temp) != 0){
      out <- rbind(
        out,
        cbind(e, temp)
      )
    }
  }
  return(out)
}
