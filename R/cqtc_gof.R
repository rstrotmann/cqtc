#' Goodness of fit plots for linear mixed effects model
#'
#' @param mod Model result.
#'
#' @returns A list of ggplot objects.
#' @export
cqtc_gof_plot <- function(mod) {

  result <- augment(mod) %>%
    mutate(scaled_res = residuals(mod, scaled = TRUE))

  out <- list(
    # IPRED plot as in publication:
    result %>%
      ggplot(aes(x = .fitted, y = DQTCF)) +
      geom_point(alpha = 0.2) +
      geom_smooth(method="loess", se = TRUE) +
      geom_abline(intercept = 0, slope = 1) +
      labs(x = "predicted (ms)", y = "observed (ms)",
           title = "IPRED vs OBS") +
      theme_bw(),

    # QQ plot as in publication
    result %>%
      ggplot(aes(sample = scaled_res)) +
      stat_qq(alpha=0.2) +
      geom_abline(intercept = 0, slope = 1) +
      labs(x = "theoretical quantiles", y = "standardized residuals",
           title = "QQ plot") +
      theme_bw(),

    result %>%
      ggplot(aes(x = CONC, y = scaled_res)) +
      geom_point(alpha = 0.2) +
      geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed") +
      geom_smooth(method = "loess", se = TRUE) +
      labs(x = "concentration (ng/ml)", y = "standardized residuals",
           title = "Residuals by concentration") +
      theme_bw(),

    result %>%
      ggplot(aes(x = DPM_BL_QTCF, y = scaled_res)) +
      geom_point(alpha = 0.2) +
      geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed") +
      labs(x = "centered baseline QTcF", y = "standardized residuals",
           title = "Residuals by baseline QTcF") +
      theme_bw(),

    result %>%
      ggplot(aes(x = NTIME, y = scaled_res)) +
      geom_point(alpha = 0.2) +
      # geom_boxplot(notch = TRUE, outlier.color="white") +
      geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed") +
      labs(x = "NTIME (h)", y = "standardized residuals",
           title = "Residuals by NTIME") +
      theme_bw()
  )

  return(out)
}
