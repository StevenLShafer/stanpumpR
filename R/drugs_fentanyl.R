fentanyl <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  # Data from
  # 1 = McClain and Hug, Clin Pharmacol Ther. 1980;28:106-14.
  # 2 = Scott and Stanski, J Pharmacol Exp Ther. 1987;240:159-66.
  # 3 = Hudson, Anesthesiology. 1986;64:334-8.
  # 4 = Varvel, Anesthesiology. 1989;70:928-34.
  # 5 = Shafer, Anesthesiology. 1990;73:1091-102.
  # The set below pools these five; its values are printed in none of them
  # (the 2026 paper digests checked each).  It replaced the earlier Scott and
  # Stanski set (V1 12.7 L) in November 2019 (a20fd1c), and its derivation is
  # not recorded.  Shafer 1990's own pooled model of the
  # same five data sets is different (V1 6.09 L, CL 0.504 L/min).

  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: volumes x weight/70, clearances x (weight/70)^0.75.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)

  default <- list(
    v1  = 12.1  * size$volume,
    v2  = 35.7  * size$volume,
    v3  = 224   * size$volume,
    cl1 = 0.632 * size$clearance,
    cl2 = 2.8   * size$clearance,
    cl3 = 1.55  * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 3.694		# from Shafer/Varvel, t_peaks.xls
  MEAC <- 0.6
  typical <- MEAC * 1.2
  lowerTypical <- MEAC * 0.8
  upperTypical <- MEAC * 2.0
  reference <- paste0(
    "Pooled parameter set from five fentanyl studies; the values are printed ",
    "in none of them. McClain DA, Hug CC. Clin Pharmacol Ther 1980;28:106-114. ",
    "https://pubmed.ncbi.nlm.nih.gov/7389247/ Scott JC, Stanski DR. ",
    "J Pharmacol Exp Ther 1987;240:159-166. ",
    "https://pubmed.ncbi.nlm.nih.gov/3100765/ Hudson RJ et al. ",
    "Anesthesiology 1986;64:334-338. https://pubmed.ncbi.nlm.nih.gov/3954128/ ",
    "Varvel JR et al. Anesthesiology 1989;70:928-934. ",
    "https://pubmed.ncbi.nlm.nih.gov/2729633/ Shafer SL et al. ",
    "Anesthesiology 1990;73:1091-1102. https://pubmed.ncbi.nlm.nih.gov/2248388/"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
