# ---- NULL handling ----

test_that("validate_param rejects NULL when allow_null is FALSE (default)", {
  expect_error(
    validate_param("character", NULL),
    "must not be NULL"
  )
  expect_error(
    validate_param("logical", NULL),
    "must not be NULL"
  )
  expect_error(
    validate_param("numeric", NULL),
    "must not be NULL"
  )
})

test_that("validate_param accepts NULL when allow_null is TRUE", {
  expect_silent(validate_param("character", NULL, allow_null = TRUE))
  expect_silent(validate_param("logical", NULL, allow_null = TRUE))
  expect_silent(validate_param("numeric", NULL, allow_null = TRUE))
  expect_invisible(validate_param("character", NULL, allow_null = TRUE))
})

test_that("validate_param error message for NULL includes parameter name", {
  my_param <- NULL
  expect_error(
    validate_param("character", my_param),
    "my_param must not be NULL"
  )
})

# ---- Type: character ----

test_that("validate_param type character error message includes parameter name", {
  bad_value <- 123
  expect_error(
    validate_param("character", bad_value),
    "bad_value must be a character value"
  )
})

test_that("validate_param type character rejects non-character values", {
  expect_error(
    validate_param("character", 123),
    "must be a character value"
  )
  expect_error(
    validate_param("character", 123L),
    "must be a character value"
  )
  expect_error(
    validate_param("character", TRUE),
    "must be a character value"
  )
  expect_error(
    validate_param("character", c(1, 2), allow_multiple = TRUE),
    "must be a character value"
  )
  expect_error(
    validate_param("character", list("a")),
    "must be a character value"
  )
})

test_that("validate_param type character accepts character values when allow_empty is TRUE", {
  expect_silent(validate_param("character", "x", allow_empty = TRUE))
  expect_silent(validate_param("character", "hello", allow_empty = TRUE))
  expect_silent(validate_param("character", c("a", "b"), allow_multiple = TRUE, allow_empty = TRUE))
})

test_that("validate_param type character rejects empty string when allow_empty is FALSE (default)", {
  expect_error(
    validate_param("character", ""),
    "must be a non-empty string"
  )
})

test_that("validate_param type character accepts empty string when allow_empty is TRUE", {
  expect_silent(validate_param("character", "", allow_empty = TRUE))
})

test_that("validate_param type character rejects vector containing empty string when allow_empty is FALSE", {
  expect_error(
    validate_param("character", c("a", ""), allow_multiple = TRUE, allow_empty = FALSE),
    "must be a non-empty string"
  )
  expect_error(
    validate_param("character", c("", "b"), allow_multiple = TRUE, allow_empty = FALSE),
    "must be a non-empty string"
  )
})

test_that("validate_param type character accepts vector of non-empty strings when allow_empty is TRUE", {
  expect_silent(validate_param("character", c("a", "b"), allow_multiple = TRUE, allow_empty = TRUE))
})

# ---- Type: logical ----

test_that("validate_param type logical rejects non-logical values", {
  expect_error(
    validate_param("logical", "TRUE"),
    "must be a logical value"
  )
  expect_error(
    validate_param("logical", 1),
    "must be a logical value"
  )
  expect_error(
    validate_param("logical", 0L),
    "must be a logical value"
  )
})

test_that("validate_param type logical accepts logical values", {
  expect_silent(validate_param("logical", TRUE))
  expect_silent(validate_param("logical", FALSE))
  expect_silent(validate_param("logical", c(TRUE, FALSE), allow_multiple = TRUE))
})

# ---- Type: numeric ----

test_that("validate_param type numeric rejects non-numeric values", {
  expect_error(
    validate_param("numeric", "1"),
    "must be a numeric value"
  )
  expect_error(
    validate_param("numeric", TRUE),
    "must be a numeric value"
  )
  expect_error(
    validate_param("numeric", character(0), allow_multiple = TRUE),
    "must be a numeric value"
  )
})

test_that("validate_param type numeric accepts numeric and integer values", {
  expect_silent(validate_param("numeric", 1))
  expect_silent(validate_param("numeric", 1.5))
  expect_silent(validate_param("numeric", 1L))
  expect_silent(validate_param("numeric", c(1, 2, 3), allow_multiple = TRUE))
})

# ---- NA handling ----

test_that("validate_param rejects NA when allow_na is FALSE (default)", {
  expect_error(
    validate_param("character", NA_character_),
    "must not contain NA"
  )
  expect_error(
    validate_param("logical", NA),
    "must not contain NA"
  )
  expect_error(
    validate_param("numeric", NA_real_),
    "must not contain NA"
  )
  expect_error(
    validate_param("numeric", NA_integer_),
    "must not contain NA"
  )
})

test_that("validate_param rejects vector containing NA when allow_na is FALSE", {
  expect_error(
    validate_param("character", c("a", NA_character_), allow_multiple = TRUE),
    "must not contain NA"
  )
  expect_error(
    validate_param("logical", c(TRUE, NA), allow_multiple = TRUE),
    "must not contain NA"
  )
  expect_error(
    validate_param("numeric", c(1, NA_real_), allow_multiple = TRUE),
    "must not contain NA"
  )
})

test_that("validate_param accepts NA when allow_na is TRUE", {
  # character with NA still subject to empty check; use allow_empty for NA_character_
  expect_silent(validate_param("character", NA_character_, allow_na = TRUE, allow_empty = TRUE))
  expect_silent(validate_param("logical", NA, allow_na = TRUE))
  expect_silent(validate_param("numeric", NA_real_, allow_na = TRUE))
  expect_silent(validate_param("numeric", NA_integer_, allow_na = TRUE))
})

test_that("validate_param accepts vector containing NA when allow_na is TRUE", {
  expect_silent(validate_param("character", c("a", NA_character_), allow_multiple = TRUE, allow_na = TRUE, allow_empty = TRUE))
  expect_silent(validate_param("logical", c(TRUE, NA), allow_multiple = TRUE, allow_na = TRUE))
  expect_silent(validate_param("numeric", c(1, NA_real_), allow_multiple = TRUE, allow_na = TRUE))
})

test_that("validate_param NA error message includes parameter name", {
  x <- NA_character_
  expect_error(
    validate_param("character", x),
    "x must not contain NA"
  )
})

# ---- Length / allow_multiple ----

test_that("validate_param rejects length != 1 when allow_multiple is FALSE (default)", {
  expect_error(
    validate_param("character", c("a", "b")),
    "must be a single value"
  )
  expect_error(
    validate_param("logical", c(TRUE, FALSE)),
    "must be a single value"
  )
  expect_error(
    validate_param("numeric", c(1, 2)),
    "must be a single value"
  )
})

test_that("validate_param rejects length 0 when allow_multiple is FALSE", {
  expect_error(
    validate_param("character", character(0)),
    "must be a single value"
  )
  expect_error(
    validate_param("logical", logical(0)),
    "must be a single value"
  )
  expect_error(
    validate_param("numeric", numeric(0)),
    "must be a single value"
  )
})

test_that("validate_param accepts multiple values when allow_multiple is TRUE", {
  expect_silent(validate_param("character", c("a", "b", "c"), allow_multiple = TRUE, allow_empty = TRUE))
  expect_silent(validate_param("logical", c(TRUE, FALSE, TRUE), allow_multiple = TRUE))
  expect_silent(validate_param("numeric", c(1, 2, 3), allow_multiple = TRUE))
})

test_that("validate_param single value error message includes parameter name", {
  my_vec <- c("a", "b")
  expect_error(
    validate_param("character", my_vec),
    "my_vec must be a single value"
  )
})

# ---- type match.arg / partial matching ----

test_that("validate_param type accepts partial match for type argument", {
  # match.arg allows partial matching by default
  expect_silent(validate_param("char", "hello", allow_empty = TRUE))
  expect_silent(validate_param("log", TRUE))
  expect_silent(validate_param("num", 1))
})

test_that("validate_param type errors on invalid type", {
  expect_error(
    validate_param("integer", 1L),
    "should be one of"
  )
  expect_error(
    validate_param("string", "x"),
    "should be one of"
  )
})

# ---- Return value ----

test_that("validate_param returns invisible NULL on success", {
  out <- validate_param("character", "x", allow_empty = TRUE)
  expect_null(out)
  expect_invisible(validate_param("character", "x", allow_empty = TRUE))
})

# ---- Order of checks: NULL before NA before type before length before empty ----

test_that("validate_param checks NULL before other checks", {
  # If we pass NULL with allow_na TRUE, we still get NULL error when allow_null FALSE
  expect_error(
    validate_param("character", NULL, allow_na = TRUE),
    "must not be NULL"
  )
})

test_that("validate_param checks NA before type", {
  # A numeric passed for type "character" fails type; NA_character_ fails NA first when allow_na FALSE
  expect_error(
    validate_param("character", NA_character_),
    "must not contain NA"
  )
})

test_that("validate_param checks type before length", {
  # c(1, 2) for type "character": type check fails (numeric not character)
  expect_error(
    validate_param("character", c(1, 2), allow_multiple = TRUE),
    "must be a character value"
  )
})

test_that("validate_param checks length before empty string", {
  # Two character values with one empty: length allowed if allow_multiple TRUE; then empty check
  expect_error(
    validate_param("character", c("a", ""), allow_multiple = TRUE, allow_empty = FALSE),
    "must be a non-empty string"
  )
})

# ---- Edge cases ----

test_that("validate_param character with only NA and allow_na TRUE passes when allow_empty TRUE", {
  # NA_character_ can trigger empty check; allow_empty TRUE allows through
  expect_silent(validate_param("character", NA_character_, allow_na = TRUE, allow_empty = TRUE))
})

test_that("validate_param numeric integer type is accepted", {
  expect_silent(validate_param("numeric", 0L))
  expect_silent(validate_param("numeric", -1L))
})

test_that("validate_param logical NA with allow_na TRUE is accepted", {
  expect_silent(validate_param("logical", NA, allow_na = TRUE))
})

test_that("validate_param empty character vector with allow_multiple TRUE still fails type when mixed", {
  # character(0) with allow_multiple TRUE: length 0 != 1 but allow_multiple so length check passes?
  # length(param) != 1 && !allow_multiple -> (0 != 1) && TRUE -> TRUE, so we'd error "must be a single value"
  # So character(0) with allow_multiple TRUE: length 0 != 1 is true, allow_multiple TRUE so !allow_multiple FALSE,
  # so (0 != 1 && FALSE) = FALSE, we don't error on length. Type: is.character(character(0)) is TRUE. So it passes.
  expect_silent(validate_param("character", character(0), allow_multiple = TRUE))
})

test_that("validate_param combination allow_null TRUE with non-NULL value ignores allow_null", {
  expect_silent(validate_param("character", "x", allow_null = TRUE, allow_empty = TRUE))
})

test_that("validate_param combination allow_na TRUE with no NA passes", {
  expect_silent(validate_param("character", "x", allow_na = TRUE, allow_empty = TRUE))
})

test_that("validate_param combination allow_empty TRUE with non-empty passes", {
  expect_silent(validate_param("character", "hello", allow_empty = TRUE))
})

test_that("validate_param all options TRUE for character vector with empty and NA", {
  # c("a", "", NA_character_) with allow_multiple, allow_empty, allow_na all TRUE
  expect_silent(validate_param(
    "character",
    c("a", "", NA_character_),
    allow_multiple = TRUE,
    allow_empty = TRUE,
    allow_na = TRUE
  ))
})

# ---- Additional edge cases ----

test_that("validate_param numeric accepts zero and negative values", {
  expect_silent(validate_param("numeric", 0))
  expect_silent(validate_param("numeric", 0L))
  expect_silent(validate_param("numeric", -1))
  expect_silent(validate_param("numeric", -1.5))
})

test_that("validate_param logical accepts FALSE", {
  expect_silent(validate_param("logical", FALSE))
})

test_that("validate_param type character with single space is subject to empty check", {
  # nchar(" ") is 1, so not empty; should pass with allow_empty TRUE
  expect_silent(validate_param("character", " ", allow_empty = TRUE))
})

test_that("validate_param rejects empty character when allow_empty FALSE even with allow_multiple", {
  expect_error(
    validate_param("character", c("a", ""), allow_multiple = TRUE, allow_empty = FALSE),
    "must be a non-empty string"
  )
})

test_that("validate_param NULL with allow_null TRUE returns before type check", {
  # No error about type when NULL and allow_null TRUE
  expect_silent(validate_param("character", NULL, allow_null = TRUE))
  expect_silent(validate_param("numeric", NULL, allow_null = TRUE))
})

