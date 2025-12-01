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
    param = "BL_QTCF") {
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


#' Add delta to reference group by NTIME
#'
#' @param cqtc A cqtc data set.
#' @param parameter The parameter to calculate the delta over. Defaults to DQTCF.
#' @param reference_filter The filter term to identify the control group.
#'
#' @returns A cqtc object.
#' @export
derive_group_delta <- function(
    cqtc,
    parameter = "DQTCF",
    reference_filter = "ACTIVE == 0") {
  # input validation
  validate_cqtc(cqtc)
  if(!nif:::is_valid_filter(as.data.frame(cqtc), reference_filter))
    stop("Invalid filter!")

  out_name <- paste0("D", parameter)
  if(out_name %in% names(cqtc))
    stop(paste0("Parameter ", out_name, " alread in data set!"))

  ref <- cqtc %>%
    # as.data.frame() %>%
    filter(eval(parse(text = reference_filter))) %>%
    pivot_longer(
      cols = all_of(parameter),
      names_to = "PARAM", values_to = "VAL") %>%
    reframe(REF = mean(VAL), .by = c("NTIME"))

  if(nrow(ref) == 0)
    stop(paste0("No data after applying filter term '", reference_filter, "'!"))

  temp <- cqtc %>%
    as.data.frame() %>%
    pivot_longer(
      cols = any_of(c("QT", "QTCF", "DQTCF", "RR", "HR")),
      names_to = "PARAM", values_to = "VAL")

  delta <- temp %>%
    filter(PARAM == parameter) %>%
    left_join(ref, by = c("NTIME")) %>%
    mutate(VAL = VAL- REF) %>%
    mutate(PARAM = out_name) %>%
    select(-c("REF"))

  out <- bind_rows(temp, delta) %>%
    pivot_wider(names_from = PARAM, values_from = VAL) %>%
    new_cqtc()

  return(out)
}
