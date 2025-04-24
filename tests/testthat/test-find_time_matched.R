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
  expect_equal(nrow(result1), 2)
  expect_equal(result1$REF, c(1, 2))
  expect_equal(result1$pc_REF, c(3, 4))

  # Test with larger time window (30 minutes)
  result2 <- find_time_matched(mock_nif, conc_analyte = "A", time_window = 30/60)

  # Should return 2 pairs (both QTCF measurements for subject 001)
  expect_equal(nrow(result2), 3)
  expect_equal(result2$REF, c(1, 2, 5))
  expect_equal(result2$pc_REF, c(3, 4, 6))
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

test_that("find_time_matched handles multiple concentration measurements per ECG", {
  # Create mock nif object with multiple concentration measurements near an ECG
  mock_nif <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~EVID, ~DTC,                             ~REF, ~DV,
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 10:00:00"), 1,   420,
    "001",    "A",      0,     as.POSIXct("2023-01-01 09:55:00"), 2,   90,  # 5 min before
    "001",    "A",      0,     as.POSIXct("2023-01-01 10:03:00"), 3,   100, # 3 min after (closest)
    "001",    "A",      0,     as.POSIXct("2023-01-01 10:07:00"), 4,   95   # 7 min after
  )
  class(mock_nif) <- c("nif", "data.frame")

  # Test - should pick the closest concentration measurement
  result <- find_time_matched(mock_nif, conc_analyte = "A")

  # Should return 1 pair, with the closest concentration (3 min after)
  expect_equal(nrow(result), 1)
  expect_equal(result$REF, 1)
  expect_equal(result$pc_REF, 3)
})

test_that("find_time_matched correctly handles different analyte names", {
  # Create mock nif object with non-standard analyte names
  mock_nif <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~EVID, ~DTC,                             ~REF, ~DV,
    "001",    "QT",     0,     as.POSIXct("2023-01-01 10:00:00"), 1,   420, # Using QT instead of QTCF
    "001",    "DRUG_X", 0,     as.POSIXct("2023-01-01 10:05:00"), 2,   100  # Using DRUG_X for concentration
  )
  class(mock_nif) <- c("nif", "data.frame")

  # Test with custom analyte names
  result <- find_time_matched(mock_nif, conc_analyte = "DRUG_X", ecg_analyte = "QT")

  # Should return 1 pair
  expect_equal(nrow(result), 1)
  expect_equal(result$ANALYTE, "QT")
  expect_equal(result$pc_DV, 100)
})

test_that("find_time_matched handles missing time-matched pairs", {
  # Create mock nif object with measurements outside the time window
  mock_nif <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~EVID, ~DTC,                             ~REF, ~DV,
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 10:00:00"), 1,   420,
    "001",    "A",      0,     as.POSIXct("2023-01-01 10:30:00"), 2,   100, # 30 min after (outside default window)
    "002",    "QTCF",   0,     as.POSIXct("2023-01-01 11:00:00"), 3,   430,
    "002",    "A",      0,     as.POSIXct("2023-01-01 12:00:00"), 4,   90   # 60 min after (outside default window)
  )
  class(mock_nif) <- c("nif", "data.frame")

  # Test with default time window (10 minutes)
  result <- find_time_matched(mock_nif, conc_analyte = "A")

  # Should return empty dataframe as no matches within window
  expect_equal(nrow(result), 0)

  # Test with larger time window (35 minutes)
  result2 <- find_time_matched(mock_nif, conc_analyte = "A", time_window = 35/60)

  # Should return 1 pair (only subject 001)
  expect_equal(nrow(result2), 1)
  expect_equal(result2$USUBJID, "001")
})

test_that("find_time_matched correctly guesses analyte when not specified", {
  # Create mock nif object with a clear main analyte (more measurements of A than B)
  mock_nif <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~EVID, ~DTC,                             ~REF, ~DV,
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 10:00:00"), 1,   420,
    "001",    "A",      0,     as.POSIXct("2023-01-01 10:05:00"), 2,   100,
    "001",    "A",      0,     as.POSIXct("2023-01-01 11:05:00"), 3,   90,
    "001",    "A",      0,     as.POSIXct("2023-01-01 12:05:00"), 4,   80,
    "001",    "B",      0,     as.POSIXct("2023-01-01 13:05:00"), 5,   50
  )
  class(mock_nif) <- c("nif", "data.frame")

  # Mock the analytes function to work with our test data
  # (This depends on how analytes is implemented in the actual code)
  analytes <- function(nif) {
    unique(nif$ANALYTE)
  }

  # Test with no conc_analyte specified (should guess "A")
  result <- find_time_matched(mock_nif)

  # Should return 1 pair with "A" as the concentration analyte
  expect_equal(nrow(result), 1)
  expect_equal(result$pc_REF, 2)
})

test_that("find_time_matched with precise time window setting", {
  # Create mock nif object with measurements at very specific intervals
  mock_nif <- tibble::tribble(
    ~USUBJID, ~ANALYTE, ~EVID, ~DTC,                                 ~REF, ~DV,
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 10:00:00"),     1,   420,
    "001",    "A",      0,     as.POSIXct("2023-01-01 10:09:00"),     2,   100, # 9 min after
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 11:00:00"),     3,   425,
    "001",    "A",      0,     as.POSIXct("2023-01-01 11:10:10"),     4,   95,  # 10 min 10 sec after
    "001",    "QTCF",   0,     as.POSIXct("2023-01-01 12:00:00"),     5,   430,
    "001",    "A",      0,     as.POSIXct("2023-01-01 12:10:00"),     6,   90   # Exactly 10 min after
  )
  class(mock_nif) <- c("nif", "data.frame")

  # Test with default 10 minute window (should include exactly 10 min but not 10:10)
  result <- find_time_matched(mock_nif, conc_analyte = "A")

  # Should return 2 pairs (the 9 min and exactly 10 min pairs only)
  expect_equal(nrow(result), 2)
  expect_equal(result$REF, c(1, 5))
  expect_equal(result$pc_REF, c(2, 6))

  # Test with slightly larger window (10.5 minutes)
  result2 <- find_time_matched(mock_nif, conc_analyte = "A", time_window = 10.5/60)

  # Should return all 3 pairs
  expect_equal(nrow(result2), 3)
})

