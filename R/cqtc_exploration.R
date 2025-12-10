#' Identify entries with inconsistent RR and HR fields.
#'
#' @param obj A cqtc object.
#' @param threshold The allowed fractional deviation.
#'
#' @returns A data frame with entries in which the reported RR and the RR
#' re-calculated from the reported HR (i.e., rr_recalc) are different by the
#' specified threshold.
#'
#' @export
#'
#' @examples
#' find_inconsistent_rr_hr(dofetilide_cqtc, 0.01)
find_inconsistent_rr_hr <- function(obj, threshold = 0.1) {
  # input validation
  nif:::validate_numeric_param(threshold, "threshold")
  if(threshold < 0 | threshold > 1)
    stop("Threshold must be between 0 and 1!")

  out <- obj %>%
    as.data.frame() %>%
    mutate(RR_recalc = 60/.data$HR * 1000) %>%
    mutate(RR_delta = .data$RR_recalc - .data$RR) %>%
    filter(abs(.data$RR_delta / .data$RR_recalc) > threshold)

  return(out)
}


#' Plot ECG parameter by HR
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot.
#' @param fit Show regression fit.
#' @param method The method for geom_smooth.
#' @param ... Further parameters to geom_point().
#' @param group The column to be used for grouping
#'
#' @returns A ggplot object.
#' @import ggplot2
#' @export
#' @examples
#' library(dplyr)
#' library(magrittr)
#'
#' dofetilide_cqtc %>%
#'   hr_plot(param = "QT", group = "ACTIVE")
#'
#' verapamil_cqtc %>%
#'   hr_plot(param = "QTCF", group = "ACTIVE")
hr_plot <- function(
    obj,
    param = "QTCF",
    group = NULL,
    fit = TRUE,
    method = "lm",
    ...) {
  # input validation
  validate_cqtc(obj)
  if(!"HR" %in% names(obj))
    stop("HR field not found in input!")
  validate_col_param(param, obj)
  validate_col_param(group, obj, allow_null = TRUE)
  nif:::validate_logical_param(fit, "fit")
  nif:::validate_char_param(method, "method")
  allowed_methods <- c("loess", "lm")
  if(!method %in% allowed_methods)
    stop(paste0(
      "method must be one of: ",
      nif::nice_enumeration(allowed_methods, conjunction = "or")
    ))

  # plotting
  {if(!is.null(group))
    ggplot(obj, aes(
      x = .data$HR, y = .data[[param]],
      color = as.factor(.data[[group]]))) else
    ggplot(obj, aes(x = .data$HR, y = .data[[param]]))} +

    geom_point() +
    {if(fit == TRUE) {
      if(!is.null(group))
        geom_smooth(
          aes(fill = as.factor(.data[[group]])),
          method = method,
          formula = y ~ x)
      else
        geom_smooth(
          method = method,
          formula = y ~ x)}} +

    {if(!is.null(group))
      labs(color = group, fill = NULL)} +
    theme_bw() +
    guides(fill = "none") +
    theme(legend.position = "bottom")
}


#' Plot ECG parameter by RR interval
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot.
#' @param fit Show regression fit.
#' @param method The method for geom_smooth.
#' @param ... Further parameters to geom_point().
#' @param group The column to be used for grouping
#'
#' @returns A ggplot object.
#' @import ggplot2
#' @export
#' @examples
#' library(dplyr)
#' library(magrittr)
#'
#' dofetilide_cqtc %>%
#'   rr_plot(param = "QT", group = "ACTIVE")
#'
#' verapamil_cqtc %>%
#'   rr_plot(param = "QTCF", group = "ACTIVE")
rr_plot <- function(
    obj,
    param = "QTCF",
    group = NULL,
    fit = TRUE,
    method = "lm",
    ...) {
  # input validation
  validate_cqtc(obj)
  if(!"RR" %in% names(obj))
    stop("RR field not found in input!")
  validate_col_param(param, obj)
  validate_col_param(group, obj, allow_null = TRUE)
  nif:::validate_logical_param(fit, "fit")
  nif:::validate_char_param(method, "method")
  allowed_methods <- c("loess", "lm")
  if(!method %in% allowed_methods)
    stop(paste0(
      "method must be one of: ",
      nif::nice_enumeration(allowed_methods, conjunction = "or")
    ))

  # plotting
  {if(!is.null(group))
    ggplot(obj, aes(
      x = .data$RR, y = .data[[param]],
      color = as.factor(.data[[group]]))) else
        ggplot(obj, aes(x = .data$RR, y = .data[[param]]))} +

    geom_point() +
    {if(fit == TRUE) {
      if(!is.null(group))
        geom_smooth(
          aes(fill = as.factor(.data[[group]])),
          method = method,
          formula = y ~ x)
      else
        geom_smooth(
          method = method,
          formula = y ~ x)}} +

    {if(!is.null(group))
      labs(color = group, fill = NULL)} +
    theme_bw() +
    guides(fill = "none") +
    theme(legend.position = "bottom")
}


#' Exploratory c-QTc plot
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot on the y axis.
#' @param fit Show regression fit.
#' @param method The method for geom_smooth.
#' @param x_label The x axis label.
#' @param y_label The y axis label.
#' @param title The plot title.
#' @param ... Further parameters to geom_point()
#' @param color The column to be used for coloring.
#'
#' @returns A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' library(magrittr)
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
    method = "loess",
    x_label = "concentration (ng/ml)",
    y_label = NULL,
    title = "",
    # model = NULL,
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

  # plot labels
  y_label = ifelse(is.null(y_label), param, y_label)
  # if(y_label == "DQTCF")
  #   y_label = "\u0394QTcF (ms)"
  # if(y_label == "QTCF")
  #   y_label = "QTcF (ms)"

  # model paramters
  # if(!is.null(model)) {
  #   coef = coef(summary(model))
  # }

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
    labs(x = x_label, y = y_label, title = title) +
    theme_bw() +
    theme(legend.position = "bottom")
}


#' Exploratory decile plot.
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot.
#' @param n The number of quantiles, defaults to 10.
#' @param ... Further parameters to geom_point.
#' @param x_label The x axis label.
#' @param y_label The y axis label.
#' @param title The plot title.
#' @param size Point size.
#' @param alpha Alpha for points.
#' @param lwd Line width for point range.
#' @param loess Show LOESS, as logical.
#' @param lm Show linear regression, as logical.
#'
#' @returns A ggplot object.
#' @export
#' @importFrom stats median
#'
#' @examples
#' cqtc_ntile_plot(dofetilide_cqtc)
cqtc_ntile_plot <- function(
    obj,
    param = "DQTCF",
    n = 10,
    x_label = "concentration (ng/ml)",
    y_label = NULL,
    title = "",
    size = 2,
    alpha = 0.1,
    lwd = 0.6,
    loess = FALSE,
    lm = FALSE,
    ...) {
  # input validation
  validate_cqtc(obj)
  validate_col_param(param, obj)
  nif:::validate_numeric_param(n, "n")

  individual <- obj %>%
    filter(.data$CONC != 0)

  baseline <- obj %>%
    filter(.data$CONC == 0)

  deciles <- obj %>%
    filter(!is.na(.data[[param]])) %>%
    filter(!is.na(.data[["CONC"]])) %>%
    filter(.data$CONC != 0) %>%
    add_ntile("CONC", n = n) %>%
    reframe(
      n = n(),
      mean_conc = stats::median(.data[["CONC"]]),
      mean = mean(.data[[param]], na.rm = TRUE),
      sd = sd(.data[[param]]), na.rm = TRUE,
      UCL = .data$mean + qnorm(0.95)  * .data$sd/sqrt(.data$n),
      LCL = .data$mean + qnorm(0.05)  * .data$sd/sqrt(.data$n),
      .by = "CONC_NTILE")

  y_label = ifelse(is.null(y_label), param, y_label)
  # if(y_label == "DQTCF")
  #   y_label = "\u0394QTcF (ms)"
  # if(y_label == "QTCF")
  #   y_label = "QTcF (ms)"

  out <- ggplot() +
    geom_point(
      aes(x = .data$CONC, y = .data[[param]]),
      data = individual,
      size = size,
      alpha = alpha,
      color = "blue",
      # ...
      ) +
    geom_point(
      aes(x = .data$CONC, y = .data[[param]]),
      data = baseline,
      size = size,
      alpha = 0.1,
      color = "red",
      # ...
    ) +

    {if(loess == TRUE)
      geom_smooth(
        aes(x = .data$mean_conc, y = .data$mean),
        method = "loess",
        formula = y ~ x,
        data = deciles,
        color = "red",
        se = FALSE,
        lwd = lwd)} +

    {if(lm == TRUE)
      geom_smooth(
        aes(x = .data$mean_conc, y = .data$mean),
        method = "lm",
        formula = y ~ x,
        data = deciles,
        color = "black",
        se = FALSE,
        lwd = lwd,
        linetype = "dashed")} +

    geom_point(aes(x = .data$mean_conc, y = .data$mean), data = deciles) +
    geom_pointrange(
      aes(x = .data$mean_conc, y = .data$mean, ymin = .data$LCL, ymax = .data$UCL),
      data = deciles,
      lwd = lwd) +

    labs(
      x = x_label,
      y = y_label,
      title = title,
      caption = "Quantiles shown at the median bin concentration as mean and 90% CI") +
    theme_bw()

  return(out)
}


#' Exploratory decile plot.
#'
#' #' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Please use cqtc_ntile_plot instead.
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot.
#' @param n The number of quantiles, defaults to 10.
#' @param ... Further parameters to geom_point.
#' @param x_label The x axis label.
#' @param y_label The y axis label.
#' @param title The plot title.
#' @param size Point size.
#' @param alpha Alpha for points.
#' @param lwd Line width for point range.
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' cqtc_decile_plot(dofetilide_cqtc)
cqtc_decile_plot <- function(
    obj,
    param = "DQTCF",
    n = 10,
    x_label = "concentration (ng/ml)",
    y_label = NULL,
    title = "",
    size = 2,
    alpha = 0.1,
    lwd = 0.6,
    ...) {
  lifecycle::deprecate_warn("0.3.1", "cqtc_decile_plot()", "cqtc_ntile_plot()")
  cqtc_ntile_plot(obj, param, n, x_label, y_label, title, size, alpha, lwd, ...)
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
#' library(dplyr)
#' library(magrittr)
#'
#' dofetilide_cqtc %>%
#'   filter(ACTIVE == 1) %>%
#'   cqtc_hysteresis_plot()
#'
cqtc_hysteresis_plot <- function(
    obj,
    param = "QTCF",
    ...) {
  # input validation
  validate_cqtc(obj)
  validate_col_param(param, obj)

  # business logic
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
    arrange("NTIME")

  temp %>%
    ggplot(aes(x = .data$c_mean, y = .data$dqtcf_mean)) +
    geom_point() +
    geom_pointrange(aes(ymin = .data$dqtcf_lcl, ymax = .data$dqtcf_ucl)) +
    geom_path() +
    geom_text_repel(aes(label = .data$NTIME), point.padding = 2) +#
    labs(
      caption = "mean and 90% CI",
      x = "concentration",
      y = param) +
    theme_bw()
}


#' Time course plot for QT parameter and drug concentration
#'
#' @param obj A cqtc object.
#' @param param The parameter to be plotted, as character.
#' @param title The plot title.
#'
#' @returns A ggplot object.
#' @export
cqtc_time_course_plot <- function(
    obj,
    param = "QTCF",
    title = "") {
  # input validation
  validate_cqtc(obj)
  validate_col_param(param, obj)

  temp <- obj %>%
    as.data.frame() %>%
    mutate(NTIME = as.numeric(as.character(.data$NTIME))) %>%
    filter(.data$ACTIVE == TRUE) %>%
    mutate(PAR = .data[[param]]) %>%
    select(all_of(c("ID", "NTIME", "ACTIVE", "CONC", "PAR"))) %>%
    pivot_longer(cols = c("CONC", "PAR"), names_to = "PARAM", values_to = "VAL")

  temp %>%
    reframe(
      mean = mean(.data$VAL, na.rm = TRUE),
      sd = sd(.data$VAL, na.rm = TRUE),
      n = n(),
      .by = c("NTIME", "ACTIVE", "PARAM")) %>%
    filter(!is.na(mean)) %>%
    ggplot(aes(x = .data$NTIME, y = mean)) +
    geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd)) +
    geom_point() +
    geom_line() +
    facet_grid(
      PARAM ~ .,
      scales = "free_y",
      labeller = labeller(PARAM = c(CONC = "concentration", PAR = param))) +
    labs(y = "", title = title, caption = "mean and SD") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))
}





