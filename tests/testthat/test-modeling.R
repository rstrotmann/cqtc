test_that("Modeling for dofetilitde works as intended", {
  library(lsmeans)

  dof <- dofetilide_cqtc %>%
    mutate(BL_QTCF = QTCF[NTIME == -0.5], .by = "ID") %>%
    add_bl_popmean("BL_QTCF") %>%
    mutate(DPM_BL_QTCF = BL_QTCF - PM_BL_QTCF) %>%
    mutate(NTIME = as.factor(NTIME)) %>%
    as.data.frame()
})
