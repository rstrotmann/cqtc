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


#' ECG data set "ECG Effects of Ranolazine, Dofetilide, Verapamil, and Quinidine"
#'
#' @format A data.table with 5232 rows and 32 columns:
#' \describe{
#'		\item{EGREFID}{Unique ECG ID (same as file name)}
#'		\item{RANDID}{Subject Randomization number}
#'		\item{SEX}{Sex either M (Male) or F (Female)}
#'		\item{AGE}{Age in years at screening}
#'		\item{HGHT}{Height in cm at screening}
#'		\item{WGHT}{Weight in kg at screening}
#'		\item{SYSBP}{Baseline systolic blood pressure in mmHg (across visits)}
#'		\item{DIABP}{Baseline diastolic blood pressure in mmHg (across visits)}
#'		\item{RACE}{Race as provided}
#'		\item{ETHNIC}{Ethnicitiy as provided}
#'		\item{ARMCD}{Sequence of treatments using treatment codes (A: Ranolazine, B: Dofetilide, C: Verapamil, D: Quinidine, E: Placebo)}
#'		\item{VISIT}{Visit code, PERIOD-X-Dosing refers to the Xth dosing}
#'		\item{EXTRT}{Treatment}
#'		\item{EXDOS}{Dose of treatment}
#'    \item{EXDOSU}{Unit of dose}
#'    \item{TPT}{Nominal time-point, relative to dose. ECGs from the same nominal time-point or triplicate have same time-point}
#'    \item{BASELINE}{Baseline Y/N}
#'    \item{PCTEST}{Pharmacokinetic test}
#'    \item{PCSTRESN}{Measured concentration or missing if no measurement were performed, this is the case for baseline. As only one sample were performed for each nominal time-point the value is repeated for all ECGs}
#'    \item{PCSTRESU}{Units of measured concentration}
#'    \item{EGREFID}{Unique ECG id. ECGs are stored within the zipfiles as follows: randid/egrefid.ecg}
#'    \item{RR}{RR interval in ms}
#'    \item{PR}{PR interval in ms}
#'    \item{QT}{QT interval in ms}
#'    \item{TPEAKTEND}{Tpeak-Tend interval in ms (from the first peak of the T-wave to the end of the T-wave)}
#'    \item{TPEAKTPEAKP}{Interval between the two peaks of the T-wave (if secondary peak is present in the T-wave) in ms}
#'    \item{ERD_30}{30\% of early repolarization duration in ms}
#'    \item{LRD_30}{30\% of late repolarization duration in ms}
#'    \item{QRS}{QRS interval in ms}
#'    \item{JTPEAK}{J-Tpeak interval in ms (from end of QRS to the first peak of the T-wave)}
#'    \item{TPEAKTEND}{Tpeak-Tend interval in ms (from the first peak of the T-wave to the end of the T-wave)}
#'    \item{TPEAKTPEAKP}{Interval between the two peaks of the T-wave (if secondary peak is present in the T-wave) in ms}
#'    \item{Twave_amplitude}{Amplitude of the T-wave measured in the vector magnitude lead in uV}
#'    \item{Twave_asymmetry}{T-wave asymmetry score (dimensionless units)}
#'    \item{Twave_flatness}{T-wave flatness score (dimensionless units)}
#'	}
#' @source <https://physionet.org/content/ecgrdvq/1.0.0/#files-panel>
#' @references Johannesen L, Vicente J, Mason JW, Sanabria C, Waite-Labott K,
#'  Hong M, Guo P, Lin J, Sørensen JS, Galeotti L, Florian J, Ugander M,
#'  Stockbridge N, Strauss DG. Differentiating Drug-Induced Multichannel Block
#'  on the Electrocardiogram: Randomized Study of Dofetilide, Quinidine,
#'  Ranolazine, and Verapamil. Clin Pharmacol Ther. 2014 Jul 23. doi:
#'  10.1038/clpt.2014.155.
#'
"SCR_002_Clinical_Data"


#' Quinidine c-QTc data set
#'
#' A concentration-QTc analysis set for quinidine with triplicate observations,
#' derived from 'SCR_002_Clinical_Data'.
#'
#' @format
#' A cqtc object, i.e., a class wrapper around a data frame with 1008 rows and
#' 38 columns:
#'
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
#' @source Derived from <https://physionet.org/content/ecgrdvq/1.0.0/#files-panel>
#' @references Johannesen L, Vicente J, Mason JW, Sanabria C, Waite-Labott K,
#'  Hong M, Guo P, Lin J, Sørensen JS, Galeotti L, Florian J, Ugander M,
#'  Stockbridge N, Strauss DG. Differentiating Drug-Induced Multichannel Block
#'  on the Electrocardiogram: Randomized Study of Dofetilide, Quinidine,
#'  Ranolazine, and Verapamil. Clin Pharmacol Ther. 2014 Jul 23. doi:
#'  10.1038/clpt.2014.155.
"quinidine_cqtc"


#' Ranolazine c-QTc data set
#'
#' A concentration-QTc analysis set for ranolazine with triplicate observations,
#' derived from 'SCR_002_Clinical_Data'.
#'
#' @format
#' A cqtc object, i.e., a class wrapper around a data frame with 1056 rows and
#' 38 columns:
#'
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
#' @source Derived from <https://physionet.org/content/ecgrdvq/1.0.0/#files-panel>
#' @references Johannesen L, Vicente J, Mason JW, Sanabria C, Waite-Labott K,
#'  Hong M, Guo P, Lin J, Sørensen JS, Galeotti L, Florian J, Ugander M,
#'  Stockbridge N, Strauss DG. Differentiating Drug-Induced Multichannel Block
#'  on the Electrocardiogram: Randomized Study of Dofetilide, Quinidine,
#'  Ranolazine, and Verapamil. Clin Pharmacol Ther. 2014 Jul 23. doi:
#'  10.1038/clpt.2014.155.
"ranolazine_cqtc"



