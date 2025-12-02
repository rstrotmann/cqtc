test_that("Modeling for dofetilitde works as intended", {
  library(lsmeans)
  library(lmerTest)

  expect_no_error({
    # add baseline QTcF, population mean-centered baseline QTcF
    dof <- dofetilide_cqtc %>%
      mutate(BL_QTCF = QTCF[NTIME == -0.5], .by = c("ID", "ACTIVE")) %>%
      add_bl_popmean("BL_QTCF") %>%
      mutate(DPM_BL_QTCF = BL_QTCF - PM_BL_QTCF) %>%
      mutate(NTIME = as.factor(NTIME)) %>%
      as.data.frame()

    # pre-specified model
    mod <- lmerTest::lmer(
      DQTCF ~ NTIME + ACTIVE + DPM_BL_QTCF + CONC + (CONC||ID),
      data = dof)

    # parameter estimates
    temp <-as.data.frame(coef(summary(mod, ddf="Kenward-Roger")))
    colnames(temp) <- c("estimate", "se", "df", "t", "p")
    parameters <- temp %>%
      mutate(
        rse = se/estimate * 100,
          lci = estimate + qt(0.025, df = df) * se,
          uci = estimate + qt(0.975, df = df) * se,
          p = ifelse(p < 0.001, "< 0.001", signif(p, 3))) %>%
      select(estimate, lci, uci, rse, p)


    # grid <- ref.grid(mod, at = list(CONC = seq(0, max(dof$CONC, na.rm = TRUE))))

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
      cqtc_decile_plot(param = "DQTCF", n = 10) +
      geom_line(data = temp1, aes(x = CONC, y = lsmean)) +
      geom_ribbon(
        data = temp1,
        aes(x = CONC, ymin = lower.CL, ymax = upper.CL, y = lsmean),
        alpha = 0.2)
  })
})
