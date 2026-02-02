qtcf <- function(qt, rr) {
  qt / (rr/1000)^(1/3)
}


#' Lower limit of confidence interval
#'
#' @param mean Mean as numeric.
#' @param sd Standard deviation as numeric.
#' @param n Sample size as numeric.
#' @param conf_level Confidence level, defaults to 0.9
#'
#' @returns Numeric.
#' @noRd
lower_ci <- function(mean, sd, n, conf_level = 0.9){
  se <- sd / sqrt(n)
  mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}


#' Upper limit of confidence interval
#'
#' @param mean Mean as numeric.
#' @param sd Standard deviation as numeric.
#' @param n Sample size as numeric.
#' @param conf_level Confidence level, defaults to 0.9
#'
#' @returns Numeric.
#' @noRd
upper_ci <- function(mean, sd, n, conf_level = 0.9){
  se <- sd / sqrt(n)
  mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
