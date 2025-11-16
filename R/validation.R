validate_cqtc <- function(obj) {
  if (!inherits(obj, "cqtc"))
    stop("Input must be a cqtc object")
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
