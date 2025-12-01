test_that("add_bl_popmean works as intended for multiple parameters", {
  test_data <- tibble::tribble(
    ~ID, ~ACTIVE, ~NTIME, ~CONC, ~QTCF, ~HR,
    1,       1,      0,     1,    10,  60,
    1,       1,      1,     2,    20,  60,
    1,       1,      2,     3,    30,  60,
    1,       1,      3,     4,    40,  60,

    2,       1,      0,     2,    20,  70,
    2,       1,      1,     3,    30,  70,
    2,       1,      2,     4,    40,  70,
    2,       1,      3,     5,    50,  70,

    3,       0,      0,     3,    30,  80,
    3,       0,      1,     4,    40,  80,
    3,       0,      2,     5,    50,  80,
    3,       0,      3,     6,    60,  80,

    4,       0,      0,     4,    40,  70,
    4,       0,      1,     5,    50,  70,
    4,       0,      2,     6,    60,  70,
    4,       0,      3,     7,    70,  70
  )

  test <- new_cqtc(test_data)

  result <- test %>%
    add_bl_popmean(param = c("QTCF", "HR"), baseline_filter = "NTIME == 0")
})

