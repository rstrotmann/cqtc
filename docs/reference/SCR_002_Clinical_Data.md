# ECG data set "ECG Effects of Ranolazine, Dofetilide, Verapamil, and Quinidine"

ECG data set "ECG Effects of Ranolazine, Dofetilide, Verapamil, and
Quinidine"

## Usage

``` r
SCR_002_Clinical_Data
```

## Format

A data.table with 5232 rows and 32 columns:

- EGREFID:

  Unique ECG ID (same as file name)

- RANDID:

  Subject Randomization number

- SEX:

  Sex either M (Male) or F (Female)

- AGE:

  Age in years at screening

- HGHT:

  Height in cm at screening

- WGHT:

  Weight in kg at screening

- SYSBP:

  Baseline systolic blood pressure in mmHg (across visits)

- DIABP:

  Baseline diastolic blood pressure in mmHg (across visits)

- RACE:

  Race as provided

- ETHNIC:

  Ethnicitiy as provided

- ARMCD:

  Sequence of treatments using treatment codes (A: Ranolazine, B:
  Dofetilide, C: Verapamil, D: Quinidine, E: Placebo)

- VISIT:

  Visit code, PERIOD-X-Dosing refers to the Xth dosing

- EXTRT:

  Treatment

- EXDOS:

  Dose of treatment

- EXDOSU:

  Unit of dose

- TPT:

  Nominal time-point, relative to dose. ECGs from the same nominal
  time-point or triplicate have same time-point

- BASELINE:

  Baseline Y/N

- PCTEST:

  Pharmacokinetic test

- PCSTRESN:

  Measured concentration or missing if no measurement were performed,
  this is the case for baseline. As only one sample were performed for
  each nominal time-point the value is repeated for all ECGs

- PCSTRESU:

  Units of measured concentration

- EGREFID:

  Unique ECG id. ECGs are stored within the zipfiles as follows:
  randid/egrefid.ecg

- RR:

  RR interval in ms

- PR:

  PR interval in ms

- QT:

  QT interval in ms

- TPEAKTEND:

  Tpeak-Tend interval in ms (from the first peak of the T-wave to the
  end of the T-wave)

- TPEAKTPEAKP:

  Interval between the two peaks of the T-wave (if secondary peak is
  present in the T-wave) in ms

- ERD_30:

  30% of early repolarization duration in ms

- LRD_30:

  30% of late repolarization duration in ms

- QRS:

  QRS interval in ms

- JTPEAK:

  J-Tpeak interval in ms (from end of QRS to the first peak of the
  T-wave)

- TPEAKTEND:

  Tpeak-Tend interval in ms (from the first peak of the T-wave to the
  end of the T-wave)

- TPEAKTPEAKP:

  Interval between the two peaks of the T-wave (if secondary peak is
  present in the T-wave) in ms

- Twave_amplitude:

  Amplitude of the T-wave measured in the vector magnitude lead in uV

- Twave_asymmetry:

  T-wave asymmetry score (dimensionless units)

- Twave_flatness:

  T-wave flatness score (dimensionless units)

## Source

\<https://physionet.org/content/ecgrdvq/1.0.0/#files-panel\>

## References

Johannesen L, Vicente J, Mason JW, Sanabria C, Waite-Labott K, Hong M,
Guo P, Lin J, Sørensen JS, Galeotti L, Florian J, Ugander M, Stockbridge
N, Strauss DG. Differentiating Drug-Induced Multichannel Block on the
Electrocardiogram: Randomized Study of Dofetilide, Quinidine,
Ranolazine, and Verapamil. Clin Pharmacol Ther. 2014 Jul 23. doi:
10.1038/clpt.2014.155.
