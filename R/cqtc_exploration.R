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

  out <- obj |>
    as.data.frame() |>
    mutate(RR_recalc = 60/.data$HR * 1000) |>
    mutate(RR_delta = .data$RR_recalc - .data$RR) |>
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
#' @noRd
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
#' dofetilide_cqtc |>
#'   rr_plot(param = "QT", group = "ACTIVE")
#'
#' verapamil_cqtc |>
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
#' verapamil_cqtc |>
#'   cqtc_plot(color = "ACTIVE")
#'
#' dofetilide_cqtc |>
#' filter(ACTIVE == 1) |>
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
  obj |>
    as.data.frame() |>
    filter(!is.na(.data$CONC)) |>
    filter(!is.na(.data[[param]])) |>
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
#' @param refline Plot horizontal dashed reference lines at these y axis values,
#' defaults to NULL (no lines).
#' @param errorbar The type of error bar, can be one of "CI" and "SD".
#' @param level Confidence level.
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
    refline = NULL,
    errorbar = "sd",
    level = 0.9,
    ...) {
  # input validation
  validate_cqtc(obj)
  validate_col_param(param, obj)
  validate_param("numeric", n)
  validate_param("character", x_label)
  validate_param("character", y_label, allow_null = TRUE)
  validate_param("numeric", size)
  validate_param("numeric", alpha)
  validate_param("numeric", lwd)
  validate_param("logical", loess)
  validate_param("logical", lm)
  validate_param("numeric", refline, allow_multiple = TRUE, allow_null = TRUE)
  validate_param("character", errorbar)

  errorbar <- tolower(errorbar)
  if (!errorbar %in% c("sd", "ci"))
    stop("errorbar must be one of 'SD' or 'CI'")

  individual <- obj |>
    filter(.data$CONC != 0) |>
    filter(!is.na(.data$CONC), !is.na(.data[[param]]))

  baseline <- obj |>
    filter(.data$CONC == 0)

  # level = 0.9

  deciles <- obj |>
    filter(!is.na(.data[[param]])) |>
    filter(!is.na(.data[["CONC"]])) |>
    # filter(.data$CONC != 0) |>
    filter(.data$ACTIVE == TRUE) |>
    add_ntile("CONC", n = n) |>
    reframe(
      n = n(),
      mean_conc = stats::median(.data[["CONC"]]),
      mean = mean(.data[[param]], na.rm = TRUE),
      sd = sd(.data[[param]]), na.rm = TRUE,
      UCL = upper_ci(.data$mean, .data$sd, .data$n, conf_level = level),
      LCL = lower_ci(.data$mean, .data$sd, .data$n, conf_level = level),
      # UCL = .data$mean + qnorm(0.95)  * .data$sd/sqrt(.data$n),
      # LCL = .data$mean + qnorm(0.05)  * .data$sd/sqrt(.data$n),
      .by = "CONC_NTILE")

  y_label = ifelse(is.null(y_label), param, y_label)

  out <- ggplot() +
    geom_point(
      aes(x = .data$CONC, y = .data[[param]]),
      data = individual,
      size = size,
      alpha = alpha,
      color = "blue",
      ) +
    geom_point(
      aes(x = .data$CONC, y = .data[[param]]),
      data = baseline,
      size = size,
      alpha = alpha,
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
        fullrange = TRUE,
        color = "black",
        se = FALSE,
        lwd = lwd,
        linetype = "dashed")} +

    {if (!is.null(refline))
      geom_hline(yintercept = refline, color = "red", linetype = "dashed",
                 lwd = lwd)
    } +

    geom_point(
      aes(x = .data$mean_conc, y = .data$mean),
      data = deciles,
      size = size
    ) +

    {if (errorbar == "ci")
      geom_pointrange(
        aes(x = .data$mean_conc, y = .data$mean, ymin = .data$LCL, ymax = .data$UCL),
        data = deciles,
        lwd = lwd)} +

    {if (errorbar == "sd")
      geom_pointrange(
        aes(x = .data$mean_conc,
            y = .data$mean,
            ymin = .data$mean - .data$sd,
            ymax = .data$mean + .data$sd),
        data = deciles,
        lwd = lwd)} +

    labs(
      x = x_label,
      y = y_label,
      title = title
    ) +

    {if (errorbar == "sd")
      labs(caption = "Quantiles shown at the median bin concentration as mean and SD")
    } +

    {if (errorbar == "ci")
      labs(caption = "Quantiles shown at the median bin concentration as mean with 90% CI")
    } +

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
#' dofetilide_cqtc |>
#'   filter(ACTIVE == 1) |>
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
  temp <- obj |>
    filter(!is.na(.data$CONC), !is.na(.data[[param]])) |>
    reframe(
      c_mean = mean(.data$CONC),
      dqtcf_mean = mean(.data[[param]], na.rm = T),
      dqtcf_sd = sd(.data[[param]], na.rm = T),
      n = n(),
      .by = "NTIME") |>
    mutate(
      dqtcf_lcl = lower_ci(.data$dqtcf_mean, .data$dqtcf_sd, .data$n),
      dqtcf_ucl = upper_ci(.data$dqtcf_mean, .data$dqtcf_sd, .data$n)
    ) |>
    arrange("NTIME")

  temp |>
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

  temp <- obj |>
    as.data.frame() |>
    mutate(NTIME = as.numeric(as.character(.data$NTIME))) |>
    filter(.data$ACTIVE == TRUE) |>
    mutate(PAR = .data[[param]]) |>
    select(all_of(c("ID", "NTIME", "ACTIVE", "CONC", "PAR"))) |>
    pivot_longer(cols = c("CONC", "PAR"), names_to = "PARAM", values_to = "VAL")

  temp |>
    reframe(
      mean = mean(.data$VAL, na.rm = TRUE),
      sd = sd(.data$VAL, na.rm = TRUE),
      n = n(),
      .by = c("NTIME", "ACTIVE", "PARAM")) |>
    filter(!is.na(mean)) |>
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



#' Plot predicted dQTcF over concentration
#'
#' @param obj A cqtc object.
#' @param mod A linear model.
#' @param level The prediction interval.
#' @inheritParams cqtc_ntile_plot
#'
#' @returns A ggplot2 object.
#' @importFrom emmeans ref_grid emmeans
#' @export
cqtc_model_plot <- function(
    obj,
    mod,
    x_label = "concentration (ng/ml)",
    y_label = "dQTcF",
    title = NULL,
    level = 0.9,
    size = 2,
    alpha = 0.1,
    lwd = 0.6,
    loess = FALSE,
    refline = NULL
    ) {
  # input validateion
  validate_cqtc(obj)
  validate_param("character", title, allow_null = TRUE)
  validate_param("character", x_label)
  validate_param("character", y_label)
  validate_param("numeric", level)
  validate_param("numeric", size)
  validate_param("numeric", alpha)
  validate_param("numeric", lwd)
  validate_param("logical", loess)
  validate_param("numeric", refline, allow_multiple = TRUE, allow_null = TRUE)

  # make reference grid
  length_out <- 20
  max_conc <- max(obj$CONC, na.rm = TRUE)

  temp = list(
    CONC = seq(0, max_conc, length.out = length_out)
  )
  if ("DPM_BL_QTCF" %in% names(obj))
    temp <- c(temp, DPM_BL_QTCF = 0)

  rg <- emmeans::ref_grid(mod, at=temp)

  emm <- summary(emmeans(rg, specs = "CONC", level = level))

  p <- obj %>%
    cqtc_ntile_plot(
      param = "DQTCF", n = 10, size = size, alpha = alpha,
      lwd = lwd, loess = loess, refline = refline,
      errorbar = "CI") +
    geom_line(
      data = emm,
      aes(x = .data$CONC, y = .data$emmean),
      lwd = lwd
    ) +

    geom_ribbon(
      data = emm,
      aes(x = .data$CONC, ymin = .data$lower.CL, ymax = .data$upper.CL,
          y = .data$emmean),
      alpha = 0.2,
      lwd = lwd) +

    labs(
      x = x_label,
      y = y_label,
      caption = paste0("Grey: Model prediction (mean and ", level*100, "% PI)"))

  if (!is.null(title))
    p <- p + ggtitle(title)

  p
}


#' Fixed effects of model
#'
#' CI are likelihood profile CI
#'
#' @param mod The linear mixed-effects model
#' @param level The confidence interval level.
#'
#' @returns A data frame.
#' @export
#' @importFrom stats coef
#' @importFrom stats qt
#' @importFrom broom.mixed tidy
#' @import lmerTest
#'
cqtc_model_fixed_effects <- function(
    mod,
    method = "profile",
    level = 0.9
) {
  # convert model to lmerModLmerTest
  temp <- mod
  # if (!inherits(mod, "lmerModLmerTest")) {
  #   temp <- as_lmerModLmerTest(mod)
  # }

  # calculate CI
  # parameters <- temp |>
  #   tidy() |>
  #   mutate(
  #     t_score = qt(p = (1- level) / 2, df = .data$df, lower.tail = FALSE),
  #     moe = .data$t_score * .data$std.error,
  #     lci = .data$estimate - .data$moe,
  #     uci = .data$estimate + .data$moe
  #   ) |>
  #   filter(.data$effect == "fixed") |>
  #   select(c("term", "estimate", "std.error", "df", "p.value", "lci", "uci"))
  #
  # parameters

  temp |>
    tidy(
      mod,
      effects="fixed",
      conf.int = TRUE,
      conf.method = method,
      conf.level = level
    )
}



#' Tabulate model parameters
#'
#' @param mod The linear mixed-effects model
#' @param level The confidence interval level.
#' @param round Number of decimal places in output.
#'
#' @returns A data frame.
#' @export
#' @importFrom stats coef
#' @importFrom stats qt
#' @importFrom broom.mixed tidy
#' @import lmerTest
#'
cqtc_model_table <- function(
    mod,
    level = 0.9,
    round = 3
) {
  cqtc_model_fixed_effects(mod, level) |>
    mutate(across(where(is.numeric), function(x) round(x, round)))
}


#' Plot HR over time
#'
#' @param obj A cqtc object.
#' @param param The parameter to plot.
#' @param group The grouping variable.
#' @param title The plot title.
#' @param size Point size.
#' @param alpha The alpha value for points.
#' @param lwd The line width.
#'
#' @returns A ggplot2 object.
#' @export
hr_by_time_plot <- function(
    obj,
    param = "HR",
    group = NULL,
    title = "",
    size = 0.5,
    alpha = 1,
    lwd = 0.6
    ) {
  # input validation
  validate_cqtc(obj)

  # plot
  temp <- obj |>
    reframe(
      mean = mean(.data[[param]], na.rm = T),
      sd = sd(.data[[param]], na.rm = T),
      n = n(),
      .by = any_of(c("NTIME", group))) |>
    mutate(lower_ci = lower_ci(.data$mean, .data$sd, .data$n, 0.9)) |>
    mutate(upper_ci = upper_ci(.data$mean, .data$sd, .data$n, 0.9))

  p <- if(!is.null(group)) {
    ggplot(temp, aes(
      x = .data$NTIME,
      y = .data$mean,
      color = as.factor(.data[[group]])))
    } else {
        ggplot(temp, aes(
          x = .data$NTIME,
          y = .data$mean))
    }

  p <- p +
    # geom_point(size = size, alpha = alpha) +
    geom_pointrange(
      aes(ymin = .data$lower_ci, ymax = .data$upper_ci),
      lwd = lwd, alpha = alpha, size = size) +
    geom_line(lwd = lwd) +
    labs(y = param, color = group, title = title,
         caption = "mean with 90% CI") +
    theme_bw()

  if (!is.null(group)) {
    p <- p +
      theme(legend.position = "bottom")
  } else {
    p <- p +
      theme(legend.position = "non2")
  }

  return(p)
}









