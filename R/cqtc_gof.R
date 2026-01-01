#' Goodness of fit plots for linear mixed effects model
#'
#' @param mod Model result.
#'
#' @returns A list of ggplot objects.
#' @importFrom broom.mixed augment
#' @importFrom stats residuals
#' @importFrom rlang flatten
#' @importFrom stringr str_extract_all
#' @importFrom stats as.formula
#' @export
cqtc_gof_plot <- function(mod) {

  result <- augment(mod) |>
    mutate(scaled_res = stats::residuals(mod, scaled = TRUE))

  mod_call <- mod@call[2] |>
    stats::as.formula() |>
    rlang::f_rhs()
  mod_formula <- deparse(mod_call)
  mod_elements <- unique(unlist(rlang::flatten(
    stringr::str_extract_all(mod_formula, pattern = "[A-Za-z_]+"))))

  out <- list(
    # IPRED plot as in publication:
    ipred_plot = result |>
      ggplot(aes(x = .data$.fitted, y = .data$DQTCF)) +
      geom_point(alpha = 0.2) +
      geom_smooth(method="loess", se = TRUE, formula = y ~ x) +
      geom_abline(intercept = 0, slope = 1) +
      labs(x = "predicted (ms)", y = "observed (ms)",
           title = "IPRED vs OBS") +
      theme_bw(),

    # QQ plot as in publication
    qqplot = result |>
      ggplot(aes(sample = .data$scaled_res)) +
      stat_qq(alpha = 0.2) +
      geom_abline(intercept = 0, slope = 1) +
      labs(x = "theoretical quantiles", y = "standardized residuals",
           title = "QQ plot") +
      theme_bw()
    )

  res_plot <- function(element) {
    result |>
      ggplot(aes(x = .data[[element]], y = .data$scaled_res)) +
      geom_point(alpha = 0.2) +
      geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed") +
      labs(
        x = element,
        y = "standardized residuals",
        title = paste0("Residuals by ", element)) +
      theme_bw()
  }

  for(i in mod_elements) {
    out[[paste0("residuals_by_", tolower(i))]] = res_plot(i)
  }

  return(out)
}
