test_that("find_time_matched correctly matches ECG and concentration measurements", {
  # Create mock nif object
  mock_nif <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~EVID, ~DTC,                             ~REF, ~DV,
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 10:00:00"), 1,   420,
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 14:00:00"), 2,   425,
    "001",    "A",      0,     as.POSIXct("2023-01-01 10:05:00"), 3,   100,
    "001",    "A",      0,     as.POSIXct("2023-01-01 14:10:00"), 4,   80,
    "002",    "QTCF",   0,     as.POSIXct("2023-01-01 11:00:00"), 5,   410,
    "002",    "A",      0,     as.POSIXct("2023-01-01 11:30:00"), 6,   120
  )
  class(mock_nif) <- c("nif", "data.frame")

  # Test default settings (should match measurements within 10 minutes)
  result1 <- find_time_matched(mock_nif, conc_analyte = "A")

  # Should return 1 pair (only the first QTCF and A are within 10 minutes)
  expect_equal(nrow(result1), 1)
  expect_equal(result1$REF, 1)
  expect_equal(result1$pc_REF, 3)

  # Test with larger time window (30 minutes)
  result2 <- find_time_matched(mock_nif, conc_analyte = "A", time_window = 30/60)

  # Should return 2 pairs (both QTCF measurements for subject 001)
  expect_equal(nrow(result2), 2)
  expect_equal(result2$REF, c(1, 2))
  expect_equal(result2$pc_REF, c(3, 4))
})


test_that("find_time_matched handles empty datasets appropriately", {
  # Create empty mock nif object
  empty_nif <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~EVID, ~DTC, ~REF, ~DV
  )
  class(empty_nif) <- c("nif", "data.frame")

  # Should issue a warning and return empty data frame
  expect_error(
    find_time_matched(empty_nif, conc_analyte = "A"),
    "A not found in analytes!"
  )
})

test_that("find_time_matched validates input parameters", {
  # Create mock nif object
  mock_nif <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~EVID, ~DTC,                             ~REF, ~DV,
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 10:00:00"), 1,   420,
    "001",    "A",      0,     as.POSIXct("2023-01-01 10:05:00"), 2,   100
  )
  class(mock_nif) <- c("nif", "data.frame")

  # Test with invalid conc_analyte
  expect_error(
    find_time_matched(mock_nif, conc_analyte = "NOT_EXIST"),
    "NOT_EXIST not found in analytes!"
  )

  # Test with invalid ecg_analyte
  expect_error(
    find_time_matched(mock_nif, ecg_analyte = "NOT_EXIST"),
    "NOT_EXIST not found in analytes!"
  )

  # Test with invalid input type
  expect_error(
    find_time_matched("not_a_nif_object"),
    "Input must be a nif object or data frame"
  )
})

test_that("find_time_matched handles multiple subjects correctly", {
  # Create mock nif object with multiple subjects
  mock_nif <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~EVID, ~DTC,                              ~REF, ~DV,
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 10:00:00"), 1,   420,
    "001",    "A",      0,     as.POSIXct("2023-01-01 10:05:00"), 2,   100,
    "002",    "QTCF",   0,     as.POSIXct("2023-01-01 11:00:00"), 3,   410,
    "002",    "A",      0,     as.POSIXct("2023-01-01 11:05:00"), 4,   150,
    "003",    "QTCF",   0,     as.POSIXct("2023-01-01 12:00:00"), 5,   430,
    "003",    "A",      0,     as.POSIXct("2023-01-01 12:15:00"), 6,   120 # 15 min difference, outside default window
  )
  class(mock_nif) <- c("nif", "data.frame")

  # Test with default time window (10 minutes)
  result <- find_time_matched(mock_nif, conc_analyte = "A")

  # Should return 2 pairs (subject 001 and 002, not 003 as it's outside window)
  expect_equal(nrow(result), 2)
  expect_equal(unique(result$USUBJID), c("001", "002"))

  # Test with larger time window (20 minutes)
  result2 <- find_time_matched(mock_nif, conc_analyte = "A", time_window = 20/60)

  # Should return 3 pairs (all subjects)
  expect_equal(nrow(result2), 3)
  expect_equal(unique(result2$USUBJID), c("001", "002", "003"))
})

