test_that("cqtc_add_baseline works with real data set", {
  test_data <- dofetilide_cqtc

  result <- test_data %>%
    cqtc_add_baseline(c("QT", "QTCF"), baseline_filter = "NTIME == -.5")

  temp <- result %>%
    as.data.frame() %>%
    filter(NTIME == -0.5)

  expect_equal(nrow(filter(temp, QTCF != BL_QTCF)), 0)
  expect_equal(nrow(filter(temp, QT != BL_QT)), 0)
})
