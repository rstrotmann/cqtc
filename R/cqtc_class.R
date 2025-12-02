#' Make a cqtc object
#'
#' Create a cqtc object from a data frame, or an empty cqtc object if the 'obj'
#' argument is NULL.
#'
#' The minimally required fields are:
#' * ID, the subject ID as numeric
#' * NTIME, the nominal time in hours as numeric
#' * CONC, the pharmacokinetic concentration as numeric
#' * QTCF, the QTcF interval in ms, as numeric
#'
#' Further expected fields are:
#' * ACTIVE, active/control treatment flag, as logical
#' * QT, the QT interval in ms, as numeric
#' * DQTCF, the delta QTcF to baseline in ms, as numeric
#' * HR, the heart rate in 1/min, as numeric
#' * RR, the RR interval in ms, as numeric
#'
#' If only one of HR or RR is included, the other will be derived.
#'
#' @param obj A data frame.
#' @param silent Suppress warnings, as logical.
#'
#' @import dplyr
#' @return A cqtc object from the input data set.
#' @export
new_cqtc <- function(obj = NULL, silent = NULL) {
  # input validation
  nif:::validate_logical_param(silent, allow_null = TRUE)

  if (is.null(obj)) {
    out <- data.frame(
      ID = numeric(0),
      ACTIVE = logical(0),
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

    if("RR" %in% fields & !"HR" %in% fields)
      out <- out %>%
        mutate(HR = round(1000/.data$RR*60, 0))

    if("HR" %in% fields & !"RR" %in% fields)
      out <- out %>%
        mutate(RR = 60/.data$HR * 1000)

    if(!"ACTIVE" %in% fields) {
      out <- out %>%
        mutate(ACTIVE = TRUE)
    } else {
      out <- out %>%
        mutate(ACTIVE = as.logical(ACTIVE))
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
  validate_cqtc(object)

  temp <- as.data.frame(object)
  out <- list(
    cqtc = as.data.frame(temp),
    subjects = distinct(temp, across(any_of(c("ID", "USUBJID", "SUBJID")))),
    ntime = sort(unique(temp$NTIME)),
    disposition = temp %>%
      pivot_longer(
        cols = any_of(c("QT", "QTCF", "DQTCF", "RR", "HR")),
        names_to = "param", values_to = "value"
      ) %>%
      reframe(n = sum(!is.na(.data$value)), .by = c("NTIME", "ACTIVE", param)) %>%
      pivot_wider(names_from = "param", values_from = "n") %>%
      arrange(.data$ACTIVE, .data$NTIME),
    hash = hash(object)
  )
  class(out) <- "summary_cqtc"
  return(out)
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
  indent = 2
  spacer = paste(replicate(indent, " "), collapse = "")
  hline <- "-----"
  cat(paste0(hline, " c-QTc data set summary ", hline, "\n"))

  cat("Data from", nrow(x$subjects), "subjects\n\n")

  cat(paste0(
    "QTcF observations per time point:\n",
    x$disposition %>%
      select(all_of(c("NTIME", "ACTIVE", "QTCF"))) %>%
      mutate(GROUP = case_match(ACTIVE, TRUE ~ "ACTIVE", FALSE ~ "CONTROL")) %>%
      select(-c("ACTIVE")) %>%
      pivot_wider(names_from = "GROUP", values_from = "QTCF") %>%
      nif:::df_to_string(indent = 2),
    "\n\n"
  ))


  cat("Columns:\n")
  cat(stringr::str_wrap(paste(names(x$cqtc), collapse = ", "),
               width = 80, indent = 2, exdent = 2), "\n")

  # cat(paste0(
  #   "Time points:\n",
  #   paste(x$ntime$NTIME, collapse = ", "),
  #   "\n"
  # ))

  temp <- x$cqtc %>%
    as.data.frame() %>%
    select(any_of(
      c("ID", "ACTIVE", "NTIME", "CONC", "QT", "QTCF", "DQTCF", "RR", "HR"))) %>%
    utils::head(10) %>%
    mutate(across(where(is.numeric), ~ round(., 1))) %>%
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
  x <- x %>%
    as.data.frame()
  NextMethod("head")
}


#' Generic hash function
#'
#' @param obj A cqtc object.
#'
#' @return The XXH128 hash of the nif object as character.
#' @export
hash <- function(obj) {
  UseMethod("hash")
}


#' Generate the XXH128 hash of a cqtc object
#'
#' @param obj A catc object.
#'
#' @returns The XXH128 hash of the catc object as character.
#' @export
#' @importFrom rlang hash
#'
#' @examples
#' hash(dofetilide_cqtc)
hash.cqtc <- function(obj) {
  validate_cqtc(obj)
  rlang::hash(obj)
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

  obj %>%
    as.data.frame() %>%
    distinct(ID, ACTIVE)
}
