#' Add baseline field for parameter across treatment groups
#'
#' @param obj A cqtc object.
#' @param param The parameter to find the baseline for.
#' @param silent Suppress messages.
#' @param baseline_filter A filter term to identify the baseline condition, as
#' character.
#' @param summary_function Function to resolve multiple individual baseline
#' values.
#'
#' @returns A cqtc object with the baseline columns added.
#' @export
cqtc_add_baseline <- function(
    obj,
    param = "QTCF",
    baseline_filter = "NTIME < 0",
    summary_function = mean,
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
  n_no_baseline <- length(missing_baseline_sbs)
  if(n_no_baseline > 0)
    nif:::conditional_cli(
      cli_alert_warning(paste0(
        "Missing baseline values in ", n_no_baseline,
        plural(" subject", n_no_baseline > 1), "!"
      )),
      silent = silent
    )

  # Duplicate baseline values
  multiple_baseline <- bl |>
    reframe(n = n(), .by = c("ID", "ACTIVE")) |>
    filter(n > 1)
  n_id_multiple <- length(unique(multiple_baseline$ID))

  if(n_id_multiple > 0) {
    nif:::conditional_cli({
      cli_alert_warning(paste0(
        "Multiple baseline values in ", n_id_multiple,
        plural(" subject", n_id_multiple > 1),
        " resolved using function '",
        deparse(substitute(summary_function)), "'!"
        ))
      },
      silent = silent
    )

    bl <- bl |>
      pivot_longer(
        cols = all_of(names(bl)[-(1:2)]),
        names_to = "param",
        values_to = "value") |>
      reframe(
        value = summary_function(.data$value),
        .by = c("ID", "ACTIVE", "param")) |>
      pivot_wider(names_from = "param", values_from = "value")
  }

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

  bind_rows(temp, delta) |>
    pivot_wider(names_from = "PARAM", values_from = "VAL") |>
    cqtc()
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


#' Derive HR from RR
#'
#' @param obj A cqtc object.
#' @param silent Suppress messages.
#'
#' @returns A cqtc object with the HR field added.
#' @export
#'
#' @examples
#' derive_hr(dofetilide_cqtc)
#' dplyr::select(dofetilide_cqtc, -"HR") |>
#'   derive_hr()
derive_hr <- function(obj, silent = NULL) {
  # input validation
  # validate_cqtc(obj)
  nif:::validate_logical_param(silent, allow_null = TRUE)

  if (!"RR" %in% names(obj))
    stop("RR field not found!")

  if ("HR" %in% names(obj)) {
    nif:::conditional_cli(
      cli_alert_warning("HR field was be replaced!"),
      silent = silent
    )
  } else {
    nif:::conditional_cli(
      cli_alert_info("HR was derived from RR!"),
      silent = silent
    )
  }

  obj |>
    mutate(HR = round(1000 / .data$RR * 60, 0))
}


#' Derive RR from HR
#'
#' @param obj A cqtc object.
#' @param silent Suppress messages.
#'
#' @returns A cqtc object with the RR field added.
#' @export
#'
#' @examples
#' derive_rr(dofetilide_cqtc)
#' dplyr::select(dofetilide_cqtc, -"RR") |>
#'   derive_rr()
derive_rr <- function(obj, silent = NULL) {
  # input validation
  # validate_cqtc(obj)
  nif:::validate_logical_param(silent, allow_null = TRUE)

  if (!"HR" %in% names(obj))
    stop("HR field not found!")

  if ("RR" %in% names(obj)) {
    nif:::conditional_cli(
      cli_alert_warning("RR field was replaced!"),
      silent = silent
    )
  } else {
    nif:::conditional_cli(
      cli_alert_info("RR was derived from HR!"),
      silent = silent
    )
  }

  obj |>
    mutate(RR = round(60 / .data$HR * 1000))
}


#' Test whether the HR and RR fields are consistent with each other
#'
#' @param obj A cqtc object.
#' @param threshold The allowed relative difference between the original RR and
#'   the RR as recalculated from HR, defaults to 0.05.
#' @param silent Suppress messages.
#'
#' @returns A logical value.
#' @export
#'
#' @examples
#' is_hr_rr_consistent(dofetilide_cqtc)
is_hr_rr_consistent <- function(
    obj,
    threshold = 0.05,
    silent = NULL) {
  # input validation
  validate_cqtc(obj)
  nif:::validate_logical_param(silent, allow_null = TRUE)
  missing_fields <- setdiff(c("HR", "RR"), names(obj))
  if (length(missing_fields) > 0)
    stop(paste0(
      "Missing ", plural("field", length(missing_fields) > 1),
      ": ", nice_enumeration(missing_fields)
    ))

  temp <- obj |>
    rename(.rr_original = "RR") |>
    derive_rr(silent = TRUE) |>
    mutate(.rr_rel_diff = abs(.data$RR - .data$.rr_original)/.data$.rr_original) |>
    filter(.data$.rr_rel_diff > threshold)

  if (nrow(temp) > 0)
    nif:::conditional_cli(
      cli_alert_warning(paste0(
        "Inconsistency between HR and RR above ", round(threshold *100, 1),
        "% in ", nrow(temp), plural(" observation", nrow(temp) > 1), "!")),
      silent = silent
    )

  return(nrow(temp) == 0)
}

