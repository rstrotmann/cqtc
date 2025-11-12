validate_cqtc <- function(obj) {
  if (!inherits(obj, "cqtc"))
    stop("Input must be a cqtc object")
}
