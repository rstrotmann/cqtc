# Data source: https://github.com/joannaparkinson/C-QTc-hands-on-tutorial

dof <- read.csv("data-raw/qtpk_dofetilide.csv")
ver <- read.csv("data-raw/qtpk_verapamil.csv")

dofetilide_cqtc <- dof %>%
  mutate(ID = as.numeric(as.factor(USUBJID))) %>%
  mutate(
    QTCF = QTcF, DQTCF = QTcF.CFB, QT = QTm, RR = RRm, CONC = CONC,
    NTIME = TIME, ACTIVE = as.logical(ACTIVE)) %>%
  select(ID, ACTIVE, NTIME, CONC, QT, QTCF, DQTCF, RR) %>%
  cqtc()

verapamil_cqtc <- ver %>%
  mutate(ID = as.numeric(as.factor(USUBJID))) %>%
  mutate(
    QTCF = QTcF, DQTCF = QTcF.CFB, QT = QTm, RR = RRm, CONC = CONC,
    NTIME = TIME, ACTIVE = as.logical(ACTIVE)) %>%
  select(ID, ACTIVE, NTIME, CONC, QT, QTCF, DQTCF, RR) %>%
  cqtc()


