test_that("Modeling for dofetilitde works as intended", {
  library(lsmeans)

  expect_no_error({
    dof <- dofetilide_cqtc %>%
      mutate(BL_QTCF = QTCF[NTIME == -0.5], .by = "ID") %>%
      add_bl_popmean("BL_QTCF") %>%
      mutate(DPM_BL_QTCF = BL_QTCF - PM_BL_QTCF) %>%
      mutate(NTIME = as.factor(NTIME)) %>%
      as.data.frame()

    mod <- lmerTest::lmer(
      DQTCF ~ NTIME + ACTIVE + DPM_BL_QTCF + CONC + (CONC||ID),
      data = dof)

    grid <- ref.grid(mod, at = list(CONC = seq(0, max(dof$CONC, na.rm = TRUE))))

    grid <- ref.grid(
      mod,
      at = list(
        CONC = seq(0, max(dof$CONC, na.rm = TRUE)),
        ACTIVE = c(FALSE, TRUE),
        DPM_BL_QTCF = 0))

    temp1 <- summary(lsmeans::lsmeans(
      grid,
      c("CONC", "ACTIVE"),
      level = 0.9)) %>%
      filter(ACTIVE == TRUE)

    dofetilide_cqtc %>%
      cqtc_decile_plot(param = "DQTCF") +
      geom_line(data = temp1, aes(x = CONC, y = lsmean)) +
      geom_ribbon(
        data = temp1,
        aes(x = CONC, ymin = lower.CL, ymax = upper.CL, y = lsmean),
        alpha = 0.2)
  })
})
