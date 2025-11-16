#' Dofetilide c-QTc data set
#'
#' A concentration-QTc analysis set for dofetilide.
#'
#' @format
#' A cqtc object, i.e., a class wrapper around a data frame with 704 rows and 7
#' columns:
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{NTIME}{Nominal time in hours}
#'   \item{Dofetilide}{Dofetilide plasma concentration in ng/ml}
#'   \item{QT}{The QT interval in ms}
#'   \item{QTCF}{The QTcF interval in ms}
#'   \item{DQTCF}{The change from baseline in QTCF in ms}
#'   \item{RR}{The RR interval in ms}
#'   \item{HR}{Heart rate, derived from RR in the original data set}
#'   \item{BL_QTCF}{Baseline QTCF, derived from QTCF at NTIME -0.5}
#'   ...
#' }
#' @source <https://github.com/joannaparkinson/C-QTc-hands-on-tutorial>
#' @references Parkinson, J., Dota, C. & Rekić, D. Practical guide to
#' concentration-QTc modeling: a hands-on tutorial. J Pharmacokinet Pharmacodyn
#' 52, 43 (2025). https://doi.org/10.1007/s10928-025-09981-8
"dofetilide_cqtc"


#' Verapamil c-QTc data set
#'
#' A concentration-QTc analysis set for verapamil
#'
#' @format
#' A cqtc object, i.e., a class wrapper around a data frame with 704 rows and 7
#' columns:
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{NTIME}{Nominal time in hours}
#'   \item{Verapamil}{Verapamil plasma concentration in ng/ml}
#'   \item{QT}{The QT interval in ms}
#'   \item{QTCF}{The QTcF interval in ms}
#'   \item{DQTCF}{The change from baseline in QTCF in ms}
#'   \item{RR}{The RR interval in ms}
#'   \item{HR}{Heart rate, derived from RR in the original data set}
#'   \item{BL_QTCF}{Baseline QTCF, derived from QTCF at NTIME -0.5}
#'   ...
#' }
#' @source <https://github.com/joannaparkinson/C-QTc-hands-on-tutorial>
#' @references Parkinson, J., Dota, C. & Rekić, D. Practical guide to
#' concentration-QTc modeling: a hands-on tutorial. J Pharmacokinet Pharmacodyn
#' 52, 43 (2025). https://doi.org/10.1007/s10928-025-09981-8
"verapamil_cqtc"
