
#' Plot ECG parameter by HR
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot.
#' @param fit Show regression fit.
#' @param method The method for geom_smooth.
#' @param ... Further parameters to geom_point().
#' @param color The column to be used for coloring.
#'
#' @returns A ggplot object.
#' @import ggplot2
#' @export
#' @examples
#' library(dplyr)
#' dofetilide_cqtc %>%
#'   hr_plot(param = "QT", color = "ACTIVE")
#'
#' verapamil_cqtc %>%
#'   hr_plot(param = "QTCF", color = "ACTIVE")
hr_plot <- function(
    obj,
    param = "QTCF",
    color = NULL,
    fit = TRUE,
    method = "lm",
    ...) {
  # input validation
  validate_cqtc(obj)
  validate_col_param(param, obj)
  validate_col_param(color, obj, allow_null = TRUE)
  nif:::validate_logical_param(fit, "fit")
  nif:::validate_char_param(method, "method")
  allowed_methods <- c("loess", "lm")
  if(!method %in% allowed_methods)
    stop(paste0(
      "method must be one of: ",
      nif::nice_enumeration(allowed_methods, conjunction = "or")
    ))

  # Business logic
  obj %>%
    as.data.frame() %>%
    ggplot(aes(
      x = .data$HR, y = .data[[param]])) +
    {if(is.null(color))
        geom_point(...) else
          geom_point(aes(color = as.factor(.data[[color]])), ...)} +
    {if(fit == TRUE) invisible(geom_smooth(method = method))} +
    {if(!is.null(color)) labs(color = color)} +
    theme_bw() +
    theme(legend.position = "bottom")
}


#' Exploratory c-QTc plot
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot on the y axis.
#' @param fit Show regression fit.
#' @param method The method for geom_smooth.
#' @param ... Further parameters to geom_point()
#' @param color The column to be used for coloring.
#'
#' @returns A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#'
#' verapamil_cqtc %>%
#'   cqtc_plot(color = "ACTIVE")
#'
#' dofetilide_cqtc %>%
#' filter(ACTIVE == 1) %>%
#'   cqtc_plot()
#'
cqtc_plot <- function(
    obj,
    param = "QTCF",
    color = NULL,
    fit = TRUE,
    method = "lm",
    ...) {
  # input validation
  validate_cqtc(obj)
  validate_col_param(param, obj)
  validate_col_param(color, obj, allow_null = TRUE)
  nif:::validate_logical_param(fit, "fit")
  nif:::validate_char_param(method, "method")
  allowed_methods <- c("loess", "lm")
  if(!method %in% allowed_methods)
    stop(paste0(
      "method must be one of: ",
      nif::nice_enumeration(allowed_methods, conjunction = "or")
    ))

  # business logic
  obj %>%
    as.data.frame() %>%
    filter(!is.na(.data$CONC)) %>%
    filter(!is.na(.data[[param]])) %>%
    ggplot(aes(x = .data$CONC, y = .data[[param]])) +
    {if(is.null(color))
      geom_point(...) else
        geom_point(aes(color = as.factor(.data[[color]])), ...)} +
    {if(fit == TRUE) invisible(geom_smooth(method = method))} +
    {if(!is.null(color)) labs(color = color)} +
    theme_bw() +
    theme(legend.position = "bottom")
}


#' Hysteresis plot
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot on the y axis.
#' @param ... Further parameters to geom_point.
#'
#' @returns A ggplot object.
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats qnorm
#' @importFrom stats sd
#' @export
#' @examples
#' dofetilide_cqtc %>%
#'   filter(ACTIVE == 1) %>%
#'   hysteresis_plot()
#'
hysteresis_plot <- function(
    obj,
    param = "QTCF",
    ...) {
  # input validation
  validate_cqtc(obj)
  validate_col_param(param, obj)

  temp <- obj %>%
    filter(!is.na(.data$CONC), !is.na(.data[[param]])) %>%
    reframe(
      c_mean = mean(.data$CONC),
      dqtcf_mean = mean(.data[[param]], na.rm = T),
      dqtcf_sd = sd(.data[[param]], na.rm = T),
      n = n(),
      .by = .data$NTIME) %>%
    mutate(
      dqtcf_lcl = .data$dqtcf_mean + stats::qnorm(0.05) * .data$dqtcf_sd/sqrt(.data$n),
      dqtcf_ucl = .data$dqtcf_mean + stats::qnorm(0.95) * .data$dqtcf_sd/sqrt(.data$n)
    ) %>%
    arrange(.data$NTIME)

  temp %>%
    ggplot(aes(x = .data$c_mean, y = .data$dqtcf_mean)) +
    geom_point(...) +
    geom_pointrange(aes(ymin = .data$dqtcf_lcl, ymax = .data$dqtcf_ucl)) +
    geom_path() +
    geom_text_repel(size = 3, aes(label = .data$NTIME)) +
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
