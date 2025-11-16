test_that("validate_col_param rejects NULL when allow_null is FALSE", {
  expect_error(
    validate_col_param(NULL, allow_null = FALSE),
    "Parameter must not be NULL!"
  )
})

test_that("validate_col_param accepts NULL when allow_null is TRUE", {
  expect_silent(validate_col_param(NULL, allow_null = TRUE))
  expect_invisible(validate_col_param(NULL, allow_null = TRUE))
})

test_that("validate_col_param rejects non-character types", {
  expect_error(
    validate_col_param(123),
    "Parameter type must be character!"
  )
  expect_error(
    validate_col_param(TRUE),
    "Parameter type must be character!"
  )
  expect_error(
    validate_col_param(c(1, 2, 3)),
    "Parameter type must be character!"
  )
})

test_that("validate_col_param accepts character values", {
  expect_silent(validate_col_param("col1"))
  expect_silent(validate_col_param(""))
})

test_that("validate_col_param rejects multiple values when allow_multiple is FALSE", {
  expect_error(
    validate_col_param(c("col1", "col2"), allow_multiple = FALSE),
    "Parameter must be a single value!"
  )
})

test_that("validate_col_param accepts multiple values when allow_multiple is TRUE", {
  expect_silent(validate_col_param(c("col1", "col2"), allow_multiple = TRUE))
  expect_silent(validate_col_param(c("a", "b", "c"), allow_multiple = TRUE))
})

test_that("validate_col_param checks column existence in data frame", {
  df <- tibble::tribble(
    ~col1, ~col2, ~col3,
    1,     2,     3,
    4,     5,     6
  )

  expect_silent(validate_col_param("col1", df = df))
  expect_silent(validate_col_param("col2", df = df))
  expect_silent(validate_col_param("col3", df = df))

  expect_error(
    validate_col_param("missing_col", df = df),
    "Column not found in input: missing_col"
  )
})

test_that("validate_col_param checks multiple columns when allow_multiple is TRUE", {
  df <- tibble::tribble(
    ~col1, ~col2, ~col3,
    1,     2,     3,
    4,     5,     6
  )

  expect_silent(validate_col_param(c("col1", "col2"), df = df, allow_multiple = TRUE))
  expect_silent(validate_col_param(c("col1", "col2", "col3"), df = df, allow_multiple = TRUE))

  expect_error(
    validate_col_param(c("col1", "missing_col"), df = df, allow_multiple = TRUE),
    "Column not found in input: missing_col"
  )

  expect_error(
    validate_col_param(c("missing1", "missing2"), df = df, allow_multiple = TRUE),
    "Columns not found in input: missing1 and missing2"
  )
})

test_that("validate_col_param rejects non-data.frame for df parameter", {
  expect_error(
    validate_col_param("col1", df = list(a = 1, b = 2)),
    "df must be a data frame!"
  )
  expect_error(
    validate_col_param("col1", df = "not_a_df"),
    "df must be a data frame!"
  )
  expect_error(
    validate_col_param("col1", df = 123),
    "df must be a data frame!"
  )
})

test_that("validate_col_param works with NULL df", {
  expect_silent(validate_col_param("col1", df = NULL))
  expect_silent(validate_col_param(c("col1", "col2"), df = NULL, allow_multiple = TRUE))
})

test_that("validate_col_param handles empty character vector", {
  expect_silent(validate_col_param(character(0), allow_multiple = TRUE))
  # Empty vector has length 0, which is not > 1, so it passes when allow_multiple = FALSE
  expect_silent(validate_col_param(character(0), allow_multiple = FALSE))
})

test_that("validate_col_param handles NULL with df when allow_null is TRUE", {
  df <- tibble::tribble(
    ~col1, ~col2,
    1,     2
  )

  expect_silent(validate_col_param(NULL, df = df, allow_null = TRUE))
})

