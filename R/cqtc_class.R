#' cqtc class constructor
#'
#' @param obj A data frame.
#' @param silent Suppress warnings, as logical.
#'
#' @import dplyr
#' @return A cqtc object from the input data set.
#' @export
new_cqtc <- function(obj = NULL, silent = NULL) {
  if (is.null(obj)) {
    out <- data.frame(
      ID = numeric(0),
      NTIME  = numeric(0),
      CONC= numeric(0),
      QTCF = numeric(0)
    )
  } else {
    # input validation
    if(!inherits(obj, "data.frame")) {
      stop("Input must be a data frame!")
    }
    minimal_fields <- c("ID", "NTIME", "CONC", "QTCF")
    missing_minimal <- setdiff(minimal_fields, names(obj))
    if(length(missing_minimal) > 0) {
      stop(paste0(
        "Missing expected ",
        nif::plural("field", length(missing_minimal) > 1), ": ",
        nif::nice_enumeration(missing_minimal), "!"))
    }

    out <- obj
    fields <- names(obj)
    if(!"HR" %in% fields & !"RR" %in% fields)
      warning("Neither RR nor HR fields found!")

    if("RR" %in% fields & !"HR" %in% fields) {
      out <- out %>%
        mutate(HR = round(1000/.data$RR*60, 0))
    }

    if("HR" %in% fields & !"RR" %in% fields) {
      out <- out %>%
        mutate(RR = 60/.data$HR * 1000)
    }

  }
  class(out) <- c("cqtc", "data.frame")

  return(out)
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
summary.cqtc <- function(object, ...) {
  out <- list(
    cqtc = as.data.frame(object),
    subjects = distinct(object, across(any_of(c("ID", "USUBJID", "SUBJID")))),
    ntime = arrange(distinct(object, .data$NTIME), .data$NTIME),
    disposition = object %>%
      pivot_longer(
        cols = any_of(c("QT", "QTCF", "DQTCF", "RR", "HR")),
        names_to = "param", values_to = "value"
      ) %>%
      reframe(n = sum(!is.na(.data$value)), .by = c("NTIME", "param")) %>%
      pivot_wider(names_from = "param", values_from = "n")
  )
  class(out) <- "summary_cqtc"
  return(out)
}


#' Generic print function for cqtc_summary objects
#'
#' @param x A summary_cqtc object.
#' @param ... Further parameters
#'
#' @returns Nothing.
#' @noRd
#' @export
print.summary_cqtc <- function(x, ...) {
  indent = 2
  spacer = paste(replicate(indent, " "), collapse = "")
  hline <- "-----"
  cat(paste0(hline, " c-QTc data set summary ", hline, "\n"))

  cat("Data from", nrow(x$subjects), "subjects\n\n")

  cat(paste0(
    "Observations per time point:\n",
    nif:::df_to_string(x$disposition, indent = 2),
    "\n\n"
  ))

  # cat(paste0(
  #   "Time points:\n",
  #   paste(x$ntime$NTIME, collapse = ", "),
  #   "\n"
  # ))

  temp <- x$cqtc %>%
    as.data.frame() %>%
    select(any_of(c("ID", "NTIME", "QT", "QTCF", "DQTCF", "RR", "HR"))) %>%
    utils::head(10) %>%
    mutate(across(where(is.numeric), ~ round(., 1))) %>%
    nif:::df_to_string(indent = 2)
  cat(paste0("\nData (selected columns):\n", temp, "\n"))
  cat(paste0(nif::positive_or_zero(nrow(x$cqtc) - 10), " more rows"))
}




