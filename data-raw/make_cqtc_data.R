dof <- read.csv("data-raw/qtpk_dofetilide.csv")
ver <- read.csv("data-raw/qtpk_verapamil.csv")

dofetilide_cqtc <- dof %>%
  mutate(ID = as.numeric(as.factor(USUBJID))) %>%
  mutate(
    QTCF = QTcF, DQTCF = QTcF.CFB, QT = QTm, RR = RRm, CONC = CONC,
    NTIME = TIME) %>%
  select(ID, NTIME, CONC, QT, QTCF, DQTCF, RR) %>%
  new_cqtc() %>%
  mutate(BL_QTCF = QTCF[NTIME == -0.5], .by = "ID")

verapamil_cqtc <- ver %>%
  mutate(ID = as.numeric(as.factor(USUBJID))) %>%
  mutate(
    QTCF = QTcF, DQTCF = QTcF.CFB, QT = QTm, RR = RRm, CONC = CONC,
    NTIME = TIME) %>%
  select(ID, NTIME, CONC, QT, QTCF, DQTCF, RR) %>%
  new_cqtc() %>%
  mutate(BL_QTCF = QTCF[NTIME == -0.5], .by = "ID")


