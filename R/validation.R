validate_cqtc <- function(obj) {
  if (!inherits(obj, "cqtc"))
    stop("Input must be a cqtc object")

  expected_columns <- c("ID", "NTIME", "CONC", "QTCF")
  missing_columns <- setdiff(expected_columns, names(obj))
  if(length(missing_columns) > 0)
    stop("Missing expected columns: ", nif::nice_enumeration(missing_columns))
}


#' Validate character parameter
#'
#' @param value The parameter value.
#' @param df A data frame the column names of which must contain the value(s),
#' if not NULL.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_multiple Allow character vectors, as logical.
#'
#' @returns Nothing.
#' @export
validate_col_param <- function(
    value,
    df = NULL,
    allow_null = FALSE,
    allow_multiple = FALSE) {
  if(!isTRUE(allow_null) && is.null(value))
    stop("Parameter must not be NULL!")

  # If NULL is allowed and value is NULL, return early
  if(isTRUE(allow_null) && is.null(value))
    return(invisible(NULL))

  if(!is.character(value))
    stop("Parameter type must be character!")
  if(!isTRUE(allow_multiple) && length(value) > 1)
    stop("Parameter must be a single value!")
  if(!is.null(df)) {
    if(!is.data.frame(df))
      stop("df must be a data frame!")
    missing_col = setdiff(value, names(df))
    if(length(missing_col) > 0) {
      stop(paste0(
        nif::plural("Column", length(missing_col) > 1),
        " not found in input: ", nif::nice_enumeration(missing_col)
      ))
    }
  }
}


#' validate function parameter
#'
#' @param type The expected parameter type (one of 'character', 'logical' or
#' 'numeric').
#' @param param The parameter.
#' @param allow_null Allow NULL values.
#' @param allow_empty Allow empty values.
#' @param allow_multiple Allow multiple values.
#' @param allow_na Allow NA values.
#'
#' @returns Nothing.
#' @noRd
validate_param <- function(
    type = c("character", "logical", "numeric"),
    param,
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE,
    allow_na = FALSE
) {
  # Validate type parameter
  type <- match.arg(type)

  param_name <- deparse(substitute(param))

  # Check for NULL first
  if (is.null(param)) {
    if (allow_null) {
      return(invisible(NULL))
    } else {
      stop(paste0(param_name, " must not be NULL"))
    }
  }

  # Check for NA values
  if (!allow_na && any(is.na(param))) {
    stop(paste0(param_name, " must not contain NA"))
  }

  # Type checking
  if ((type == "character" && !is.character(param)) ||
      (type == "logical" && !is.logical(param)) ||
      (type == "numeric" && !is.numeric(param))) {
    stop(paste0(param_name, " must be a ", type, " value"))
  }

  # Length checking
  if (length(param) != 1 && !allow_multiple) {
    stop(paste0(param_name, " must be a single value"))
  }

  # Empty string check (only for character types)
  if (
    type == "character" &&
    !allow_empty &&
    length(param) > 0 &&
    any(nchar(param) == 0)
  ) {
    stop(paste0(param_name, " must be a non-empty string"))
  }

  invisible(NULL)
}
