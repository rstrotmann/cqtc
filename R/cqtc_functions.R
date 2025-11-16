#' Add population mean column
#'
#' @param cqtc A cqtc object.
#' @param param The column name for the baseline value.
#'
#' @returns a cqtc object with the population mean column(n) added for the
#' specified parameters. The name(s) follow the naming "PM_xx", where "xx" is
#' the parameter name.
#' @export
add_bl_popmean <- function(
    cqtc,
    param = "QTCF") {
  # input validation
  validate_cqtc(cqtc)
  validate_col_param(param, cqtc, allow_multiple = TRUE)

  # business logic
  bl <- cqtc %>%
    as.data.frame() %>%
    select(all_of(c("ID", param))) %>%
    distinct()

  if(nrow(bl) != length(unique(cqtc$ID)))
    stop("Non-unique parameters by subject!")

  popmean <- bl %>%
    pivot_longer(cols = param, names_to = "param", values_to = "value") %>%
    reframe(popmean = mean(.data$value, na.rm = TRUE), .by = param) %>%
    pivot_wider(names_from = "param", values_from = popmean) %>%
    rename_with(~ paste0("PM_", .x), everything())

  # Bind popmean to each row of cqtc (recycling single row)
  bind_cols(cqtc, popmean)
}
