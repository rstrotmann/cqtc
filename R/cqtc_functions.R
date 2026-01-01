#' Add baseline field for parameter across treatment groups
#'
#' @param obj A cqtc object.
#' @param param The parameter to find the baseline for.
#' @param silent Suppress messages.
#' @param baseline_filter A filter term to identify the baseline condition, as
#' character.
#'
#' @returns A cqtc object with the baseline columns added.
#' @export
cqtc_add_baseline <- function(
    obj,
    param = "QTCF",
    baseline_filter = "NTIME < 0",
    silent = NULL) {
  # input validation
  validate_cqtc(obj)
  validate_col_param(param, obj, allow_multiple = TRUE)

  # identify baseline
  bl <- obj |>
    as.data.frame() |>
    filter(eval(parse(text = baseline_filter))) |>
    select(all_of(c("ID", "ACTIVE", param))) |>
    rename_with(.fn = function(x) {paste0("BL_", x)}, .cols = all_of(param)) |>
    distinct()

  # Missing baseline values
  missing_baseline_sbs <- setdiff(unique(obj$ID), unique(bl$ID))
  if(length(missing_baseline_sbs) > 0)
    nif:::conditional_message(
      "Missing baseline values in ", length(missing_baseline_sbs),
      " subjects!", silent = silent)

  # Duplicate baseline values
  test <- bl |>
    reframe(n = n(), .by = c("ID", "ACTIVE")) |>
    filter(n > 1)
  if(nrow(test) > 0)
    stop(paste0(
      "Multiple baseline values for ", length(unique(test$ID)), " subjects. ",
      "Consider providing an appropriate baseline_filter term!"
    ))

  # existing baseline fields
  new_bl_fields <- names(bl)[-(1:2)]
  duplicate_bl_fields <- intersect(new_bl_fields, names(obj))
  if(length(duplicate_bl_fields) > 0) {
    nif:::conditional_message(
      "The following baseline ",
      ifelse(length(duplicate_bl_fields) == 1, "field is", "fields are"),
      " already in the input and will be ",
      "overwritten: ",
      nif::nice_enumeration(duplicate_bl_fields),
      silent = silent)
  }

  out <- obj |>
    select(-all_of(duplicate_bl_fields)) |>
    left_join(bl, by = c("ID", "ACTIVE"))

  return(out)
}


#' Add population mean column
#'
#' @param cqtc A cqtc object.
#' @param param The column name for the baseline value.
#' @param silent Suppress messages.
#'
#' @returns a cqtc object with the population mean column(n) added for the
#' specified parameters. The name(s) follow the naming "PM_xx", where "xx" is
#' the parameter name.
#' @export
#'
add_bl_popmean <- function(
    cqtc,
    param = "BL_QTCF",
    silent = NULL) {
  # input validation
  validate_cqtc(cqtc)
  validate_col_param(param, cqtc, allow_multiple = TRUE)

  # business logic
  bl <- cqtc |>
    as.data.frame() |>
    select(all_of(c("ID", "ACTIVE", param))) |>
    distinct()

  missing_baseline_sbs <- setdiff(unique(cqtc$ID), unique(bl$ID))
  if(length(missing_baseline_sbs) > 0)
    nif:::conditional_message(
      "Missing baseline values in ", length(missing_baseline_sbs),
      " subjects!", silent = silent)

  # Duplicate baseline values
  test <- bl |>
    reframe(n = n(), .by = c("ID", "ACTIVE")) |>
    filter(n > 1)
  if(nrow(test) > 0)
    stop(paste0(
      "Multiple baseline values for ", length(unique(test$ID)), " subjects!"
    ))

  popmean <- bl |>
    pivot_longer(cols = all_of(param), names_to = "param", values_to = "value") |>
    reframe(popmean = mean(.data$value, na.rm = TRUE),
            .by = c("ACTIVE", param)) |>
    pivot_wider(names_from = "param", values_from = "popmean", names_prefix = "PM_")

  # Bind popmean to each row of cqtc (recycling single row)
  cqtc |>
    left_join(popmean, by = "ACTIVE")
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

  ref <- cqtc |>
    # as.data.frame() |>
    filter(eval(parse(text = reference_filter))) |>
    pivot_longer(
      cols = all_of(parameter),
      names_to = "PARAM", values_to = "VAL") |>
    reframe(REF = mean(.data$VAL), .by = c("NTIME"))

  if(nrow(ref) == 0)
    stop(paste0("No data after applying filter term '", reference_filter, "'!"))

  temp <- cqtc |>
    as.data.frame() |>
    pivot_longer(
      cols = any_of(c("QT", "QTCF", "DQTCF", "RR", "HR")),
      names_to = "PARAM", values_to = "VAL")

  delta <- temp |>
    filter(.data$PARAM == parameter) |>
    left_join(ref, by = c("NTIME")) |>
    mutate(VAL = .data$VAL - .data$REF) |>
    mutate(PARAM = out_name) |>
    select(-c("REF"))

  out <- bind_rows(temp, delta) |>
    pivot_wider(names_from = "PARAM", values_from = "VAL") |>
    new_cqtc()

  return(out)
}


#' Add ntiles (quantiles) for a specific column across all subjects
#'
#' @param obj A cqtc object.
#' @param n The number of quantiles.
#' @param input_col The column to calculate quantiles over.
#' @param ntile_name The name of the quantile column.
#'
#' @returns A cqtc object with the ntile_name colunn added.
#' @export
add_ntile <- function(obj, input_col, n, ntile_name = NULL) {
  UseMethod("add_ntile")
}


#' Add ntiles (quantiles) for a specific column across all subjects
#'
#' @param obj A cqtc object.
#' @param n The number of quantiles.
#' @param input_col The column to calculate quantiles over.
#' @param ntile_name The name of the quantile column.
#'
#' @returns A cqtc object with the ntile_name colunn added.
#' @export
#' @importFrom rlang :=
#'
#' @examples
#' head(add_ntile(dofetilide_cqtc, "CONC", 10))
add_ntile.cqtc <- function(obj, input_col = "CONC", n = 10, ntile_name = NULL) {
  # Validate that input is a nif object
  if (!inherits(obj, "cqtc")) {
    stop("Input must be a cqtc object")
  }

  nif:::validate_char_param(input_col, "input_col")
  nif:::validate_numeric_param(n, "n")
  if(n > 10 || n < 2)
    stop("n must be between 1 and 10!")
  nif:::validate_char_param(input_col, "input_col", allow_null = TRUE)
  nif:::validate_char_param(ntile_name, "ntile_name", allow_null = TRUE)
  if(is.null(ntile_name))
    ntile_name <- paste(input_col, "NTILE", sep = "_")

  # Check that required columns exist: ID, input_col
  required_cols <- c(input_col)
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0)
    stop("Missing required columns: ", nif::nice_enumeration(missing_cols))

  # Validate data types (input_col should be numeric)
  if (!is.numeric(obj[[input_col]])) {
    stop("Column '", input_col, "' must contain numeric values")
  }

  out <- obj |>
    mutate(!!ntile_name := ntile(.data[[input_col]], n = n))

  return(out)
}
