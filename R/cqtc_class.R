#' cqtc class constructor
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
      reframe(n = sum(!is.na(.data$value)), .by = c("NTIME", "param")) %>%
      pivot_wider(names_from = "param", values_from = "n") %>%
      arrange(.data$NTIME),
    hash = hash(object)
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
#' @importFrom stringr str_wrap
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
    select(any_of(c("ID", "NTIME", "CONC", "QT", "QTCF", "DQTCF", "RR", "HR"))) %>%
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


#' Add ntiles (quantiles) for a specific column across all subjects
#'
#' @param obj A cqtc object.
#' @param n The number of quantiles.
#' @param input_col The column to calculate quantiles over.
#' @param ntile_name The name of the quantile column.
#'
#' @returns A cqtc object with the ntile_name colunn added.
#' @export
add_ntile <- function(obj, input_col, n, ntile_name = NULL) {
  UseMethod("add_ntile")
}


#' Add ntiles (quantiles) for a specific column across all subjects
#'
#' @param obj A cqtc object.
#' @param n The number of quantiles.
#' @param input_col The column to calculate quantiles over.
#' @param ntile_name The name of the quantile column.
#'
#' @returns A cqtc object with the ntile_name colunn added.
#' @export
#' @importFrom rlang :=
#'
#' @examples
#' head(add_ntile(dofetilide_cqtc, "CONC", 10))
add_ntile.cqtc <- function(obj, input_col = "CONC", n = 10, ntile_name = NULL) {
  # Validate that input is a nif object
  if (!inherits(obj, "cqtc")) {
    stop("Input must be a cqtc object")
  }

  nif:::validate_char_param(input_col, "input_col")
  nif:::validate_numeric_param(n, "n")
  if(n > 10 || n < 2)
    stop("n must be between 1 and 10!")
  nif:::validate_char_param(input_col, "input_col", allow_null = TRUE)
  nif:::validate_char_param(ntile_name, "ntile_name", allow_null = TRUE)
  if(is.null(ntile_name))
    ntile_name <- paste(input_col, "NTILE", sep = "_")

  # Check that required columns exist: ID, input_col
  required_cols <- c(input_col)
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0)
    stop("Missing required columns: ", nif::nice_enumeration(missing_cols))

  # Validate data types (input_col should be numeric)
  if (!is.numeric(obj[[input_col]])) {
    stop("Column '", input_col, "' must contain numeric values")
  }

  out <- obj %>%
    mutate(!!ntile_name := ntile(.data[[input_col]], n = n))

  return(out)
}
