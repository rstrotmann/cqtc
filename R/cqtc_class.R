#' Make a cqtc object
#'
#' Create a cqtc object from a data frame, or an empty cqtc object if the 'obj'
#' argument is NULL.
#'
#' @details
#' If not input data is provided in the 'obj argument, an empty cqtc object is
#' returned. If 'obj' is a data frame, the minimally required fields are:
#'
#' * ID, the subject ID as numeric
#' * NTIME, the nominal time in hours as numeric
#' * CONC, the pharmacokinetic concentration as numeric
#' * QTCF, the QTcF interval in ms, as numeric
#' * HR, the heart rate in 1/min, as numeric
#' * RR, the RR interval in ms, as numeric
#'
#' The following additional fields will be recognized:
#' * ACTIVE, active/control treatment flag, as logical
#' * QT, the QT interval in ms, as numeric
#' * DQTCF, the delta QTcF to baseline in ms, as numeric
#'
#' If only one of HR or RR is included, the other will be derived.
#'
#' If a baseline filter is provided, the following fields will be automatically
#' derived and added to the cqtc object:
#'
#' * BL_QTCF, the baseline QTcF value. If applying the baseline filter results
#' in multiple individual baseline values, they will be summarized using the
#' function provided by the 'summrary_function' argument.
#' * DQTCF, the difference between QTCF and BL_QTCF.
#' * PM_BL_QTCF, the population mean baseline QTcF value for the populations
#' defined by 'ACTIVE' field.
#' * DPM_BL_QTCF, the difference between BL_QTCF and PM_BL_QTCF.
#'
#' @param obj A data frame.
#' @param silent Suppress warnings, as logical.
#' @param conc_field The field in the input data set that represents the
#' independent concentration variable, if 'CONC' is not provided.
#' @param rr_inconsistency allowed relative difference between RR and the RR as
#' back-calculated from HR, defaults to 0.1 (10\%).
#' @param baseline_filter A filter term to identify baseline QTcF. If NULL,
#' the 'BL_QTCF' field will not be added.
#' @param summary_function Summarizing function to consolidate multiple
#'   individual baseline values.
#'
#' @import dplyr
#' @import cli
#' @return A cqtc object from the input data set.
#' @export
cqtc <- function(
  obj = NULL,
  conc_field = NULL,
  silent = NULL,
  rr_inconsistency = 0.1,
  baseline_filter = NULL,
  summary_function = mean
) {
  # input validation
  validate_param("character", conc_field, allow_null = TRUE)
  validate_param("logical", silent, allow_null = TRUE)
  validate_param("numeric", rr_inconsistency, allow_null = TRUE)
  if (rr_inconsistency < 0 | rr_inconsistency > 1) {
    stop("rr_inconsistency threshold must be between 0 and 1!")
  }
  validate_param("character", baseline_filter, allow_null = TRUE)

  if (!is.null(baseline_filter)) {
    valid_filter <- nif:::is_valid_filter(obj, baseline_filter, silent = TRUE)
    if (!valid_filter)
      stop("Invalid baseline filter!")
  }

  # Empty cqtc object
  if (is.null(obj)) {
    out <- data.frame(
      ID = numeric(0),
      ACTIVE = logical(0),
      NTIME = numeric(0),
      CONC = numeric(0),
      QT = numeric(0),
      QTCF = numeric(0),
      HR = numeric(0),
      RR = numeric(0)
    )
    class(out) <- c("cqtc", "data.frame")
    return(out)
  }

  # cqtc object based on input data
  # input validation
  if (!inherits(obj, "data.frame")) {
    stop("Input must be a data frame!")
  }

  if (!is.null(conc_field)) {
    if (!conc_field %in% names(obj)) {
      stop("Concentration field (", conc_field, ") not found in input!")
    }
    obj <- mutate(obj, CONC = .data[[conc_field]])
  }

  if (!any(c("QT", "QTCF") %in% names(obj)))
    stop("At least one of QT and QTCF must be in the input!")

  minimal_fields <- c("ID", "NTIME", "CONC")
  missing_minimal <- setdiff(minimal_fields, names(obj))
  if (length(missing_minimal) > 0) {
    stop(paste0(
      "Missing expected ",
      nif::plural("field", length(missing_minimal) > 1), ": ",
      nif::nice_enumeration(missing_minimal), "!"
    ))
  }

  out <- obj
  fields <- names(out)

  if (!"HR" %in% fields && !"RR" %in% fields) {
    stop("Neither RR nor HR fields found, at least one must be present!")
  }

  if ("RR" %in% fields && !"HR" %in% fields)
    out <- derive_hr(out, silent = silent)

  if ("HR" %in% fields && !"RR" %in% fields)
    out <- derive_rr(out, silent = silent)

  if (!"ACTIVE" %in% fields) {
    out <- out |>
      mutate(ACTIVE = TRUE)
  } else {
    out <- out |>
      mutate(ACTIVE = as.logical(.data$ACTIVE))
  }

  # calculate QTcF, if needed
  if (!"QTCF" %in% fields && "QT" %in% fields) {
    out <- mutate(out, QTCF = qtcf(.data$QT, .data$RR))

    nif:::conditional_cli(
      cli_alert_info("QTCF was derived from QT and RR!"),
      silent = silent
    )
  }

  out <- out |>
    arrange("ID", "ACTIVE", "NTIME")

  class(out) <- c("cqtc", "data.frame")
  dummy = is_hr_rr_consistent(out, rr_inconsistency, silent = silent)

  # add QTcF baseline, if possible
  if (!is.null(baseline_filter)) {
    out <- cqtc_add_baseline(
        out, "QTCF", baseline_filter = baseline_filter,
        summary_function = summary_function, silent = silent) |>
      mutate(DQTCF = .data$QTCF - .data$BL_QTCF) |>
      add_bl_popmean("BL_QTCF") |>
      mutate(DPM_BL_QTCF = .data$BL_QTCF - .data$PM_BL_QTCF)
  }

  out
}


#' Generic print method for cqtc objects
#'
#' @param x A cqtc object.
#' @param ... Further parameters.
#'
#' @returns Nothing.
#' @noRd
#' @export
print.cqtc <- function(x, ...) {
  print(summary(x), ...)
}


#' Summary function for cqtc objects
#'
#' @param obj A cqtc object.
#' @param ... Further parameters
#'
#' @returns A summary_cqtc object.
#' @noRd
#' @export
#' @import tidyr
#' @importFrom rlang hash
summary.cqtc <- function(object, ...) {
  validate_cqtc(object)

  temp <- as.data.frame(object)

  # add mock row to facilitate the disposition calculation
  temp[nrow(temp) +1,] <- rep(NA, ncol(temp))

  out <- list(
    cqtc = as.data.frame(object),
    subjects = distinct(as.data.frame(object), across(any_of(c("ID", "USUBJID", "SUBJID")))),
    ntime = sort(unique(object$NTIME)),
    disposition = temp |>
      pivot_longer(
        cols = any_of(c("QT", "QTCF", "DQTCF", "RR", "HR")),
        names_to = "param", values_to = "value"
      ) |>
      reframe(n = sum(!is.na(.data$value)),
              .by = c("NTIME", "ACTIVE", "param")) |>
      pivot_wider(names_from = "param", values_from = "n") |>
      arrange(.data$ACTIVE, .data$NTIME),
    hash = rlang::hash(object)
  )
  class(out) <- "summary_cqtc"
  out
}


#' Print function for cqtc object
#'
#' @param x A cqtc object.
#' @param ... Further parameters.
#'
#' @returns A ggplot object.
#' @export
plot.cqtc <- function(x, ...) {
  validate_cqtc(x)
  cqtc_plot(x, ...)
}


#' Generic print function for cqtc_summary objects
#'
#' @param x A summary_cqtc object.
#' @param ... Further parameters
#'
#' @returns Nothing.
#' @noRd
#' @export
#' @importFrom stringr str_wrap
print.summary_cqtc <- function(x, ...) {
  indent <- 2
  # spacer <- paste(replicate(indent, " "), collapse = "")
  hline <- "-----"
  cat(paste0(hline, " c-QTc data set summary ", hline, "\n"))

  cat("Data from", nrow(x$subjects), "subjects\n\n")

  cat(paste0(
    "QTcF observations per time point:\n",
    x$disposition |>
      select(all_of(c("NTIME", "ACTIVE", "QTCF"))) |>
      mutate(GROUP = case_match(.data$ACTIVE, TRUE ~ "ACTIVE",
                                FALSE ~ "CONTROL")) |>
      select(-c("ACTIVE")) |>
      pivot_wider(names_from = "GROUP", values_from = "QTCF") |>
      nif:::df_to_string(indent = 2),
    "\n\n"
  ))


  cat("Columns:\n")
  cat(stringr::str_wrap(paste(names(x$cqtc), collapse = ", "),
                        width = 80, indent = 2, exdent = 2
  ), "\n")

  temp <- x$cqtc |>
    as.data.frame() |>
    select(any_of(
      c("ID", "ACTIVE", "NTIME", "CONC", "QT", "QTCF", "DQTCF", "RR", "HR")
    )) |>
    utils::head(10) |>
    mutate(across(where(is.numeric), ~ round(., 1))) |>
    nif:::df_to_string(indent = 2)
  cat(paste0("\nData (selected columns):\n", temp, "\n"))
  cat(paste0(nif::positive_or_zero(nrow(x$cqtc) - 10), " more rows"))

  cat(paste0("\n\nHash: ", x$hash))
}


#' Return the first lines of a cqtc object
#'
#' @param x A cqtc object.
#' @param ... Further arguments.
#'
#' @import dplyr
#' @return A data frame.
#' @import utils
#' @export
#' @noRd
head.cqtc <- function(x, ...) {
  x <- x |>
    as.data.frame()
  NextMethod("head")
}


#' Generic subjects function
#'
#' @param obj A cqtc object.
#'
#' @returns A data frame.
#' @export
subjects <- function(obj) {
  UseMethod("subjects")
}


#' Subjects in a cqtc object
#'
#' @param obj A cqtc object.
#'
#' @returns A data frame.
#' @export
#' @examples
#' subjects(dofetilide_cqtc)
#'
subjects.cqtc <- function(obj) {
  # input validation
  validate_cqtc(obj)

  obj |>
    as.data.frame() |>
    distinct(.data$ID, .data$ACTIVE)
}


#' Average triplicate observations
#'
#' @param obj A cqtc object.
#'
#' @returns A cqtc object with the HR, RR, QT, QTCF, DQTCF fields
#' averaged over ID, NTIME and CONC.
#' @export
#'
#' @examples
#' average_triplicates(quinidine_cqtc)
average_triplicates <- function(obj) {
  # validate input
  validate_cqtc(obj)

  obj |>
    reframe(
      across(any_of(c("HR", "RR", "QT", "QTCF", "DQTCF")), mean),
      .by = c("ID", "NTIME", "CONC")
    ) |>
    cqtc()
}
