
#' Plot ECG parameter by HR
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot.
#' @param fit Show regression fit.
#' @param method The method for geom_smooth.
#' @param ... Further parameters to geom_point()
#'
#' @returns A ggplot object.
#' @import ggplot2
#' @export
hr_plot <- function(obj, param = "QTCF", fit = TRUE, method = "lm", ...) {
  # input validation
  validate_cqtc(obj)
  nif:::validate_char_param(param, "param")
  if(!param %in% names(obj))
    stop(paste0(
      param, " field not found in input."
    ))
  nif:::validate_logical_param(fit, "fit")
  nif:::validate_char_param(method, "method")
  allowed_methods <- c("loess", "lm")
  if(!method %in% allowed_methods)
    stop(paste0(
      "method must be one of: ",
      nif::nice_enumeration(allowed_methods, conjunction = "or")
    ))
  # suppressMessages(
    obj %>%
      as.data.frame() %>%
      ggplot(aes(x = .data$HR, y = .data[[param]])) +
      geom_point(...) +
      {if(fit == TRUE) invisible(geom_smooth(method = method))} +
      theme_bw()
  # )
}


#' Title
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot on the y axis.
#' @param fit Show regression fit.
#' @param method The method for geom_smooth.
#' @param ... Further parameters to geom_point()
#'
#' @returns A ggplot object.
#' @export
cqtc_plot <- function(obj, param = "QTCF", fit = TRUE, method = "lm", ...) {
  # input validation
  validate_cqtc(obj)
  nif:::validate_logical_param(fit, "fit")
  nif:::validate_char_param(method, "method")
  allowed_methods <- c("loess", "lm")
  if(!method %in% allowed_methods)
    stop(paste0(
      "method must be one of: ",
      nif::nice_enumeration(allowed_methods, conjunction = "or")
    ))

  obj %>%
    as.data.frame() %>%
    ggplot(aes(x = .data$CONC, y = .data[[param]])) +
    geom_point(...) +
    {if(fit == TRUE) invisible(geom_smooth(method = method))} +
    theme_bw()

}


#' Title
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot on the y axis.
#' @param ... Further parameters to geom_point.
#'
#' @returns A ggplot object.
#' @importFrom ggrepel geom_text_repel
#' @export
hysteresis_plot <- function(
    obj, param = "QTCF", ...) {
  # input validation
  validate_cqtc(obj)

  temp <- obj %>%
    filter(!is.na(CONC), !is.na(.data[[param]])) %>%
    reframe(
      c_mean = mean(CONC),
      dqtcf_mean = mean(.data[[param]], na.rm = T),
      dqtcf_sd = sd(.data[[param]], na.rm = T),
      n = n(),
      .by = NTIME) %>%
    mutate(
      dqtcf_lcl = dqtcf_mean + qnorm(0.05) * dqtcf_sd/sqrt(n),
      dqtcf_ucl = dqtcf_mean + qnorm(0.95) * dqtcf_sd/sqrt(n)
    ) %>%
    arrange(NTIME)

  temp %>%
    ggplot(aes(x = c_mean, y = dqtcf_mean)) +
    geom_point(...) +
    geom_pointrange(aes(ymin = dqtcf_lcl, ymax = dqtcf_ucl)) +
    geom_path() +
    geom_text_repel(size = 3, aes(label = NTIME)) +
    theme_bw()
}


# cqtc <- dofetilide_cqtc
# drug <- "Dofetilide"
#
# cqtc <- verapamil_cqtc
# drug <- "Verapamil"
#
# temp <- cqtc %>%
#   filter(!is.na(.data[[drug]]), !is.na(DQTCF)) %>%
#   reframe(
#     c_mean = mean(.data[[drug]]),
#     dqtcf_mean = mean(DQTCF, na.rm = T),
#     dqtcf_sd = sd(DQTCF, na.rm = T),
#     n = n(),
#     .by = NTIME) %>%
#   mutate(
#     dqtcf_lcl = dqtcf_mean + qnorm(0.05) * dqtcf_sd/sqrt(n),
#     dqtcf_ucl = dqtcf_mean + qnorm(0.95) * dqtcf_sd/sqrt(n)
#   ) %>%
#   arrange(NTIME)
#
# temp %>%
#   ggplot(aes(x = c_mean, y = dqtcf_mean)) +
#   geom_point() +
#   geom_pointrange(aes(ymin = dqtcf_lcl, ymax = dqtcf_ucl)) +
#   geom_path() +
#   geom_text_repel(size = 3, aes(label = NTIME)) +
#   theme_bw()
#
#
# cqtc %>%
#   ggplot(aes(x = RR, y = QT)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_bw()
#
# cqtc %>%
#   ggplot(aes(x = RR, y = QTCF)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_bw()
#
# cqtc %>%
#   ggplot(aes(x = .data[[drug]], y = QTCF)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_bw()
