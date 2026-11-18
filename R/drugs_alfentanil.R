# BODY SIZE ANCHOR (Steven L. Shafer, 2026-11-18; docs/weight-adjustment.md,
# Anchoring at the source's cohort): fixed published values belong to the
# source's cohort, not the reference man. Scott 1987 prints no weight (read
# 2026-11-18), so the fixed values stay anchored at the 70 kg reference man,
# as before the rule.
alfentanil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  # The parameter set: V1 2.1853 L with the microconstants k10 0.0910, k12
  # 0.656, k13 0.113, k21 0.214 and k31 0.017 /min, exactly those Shafer and
  # Varvel printed in Table 1 of Anesthesiology 1991;74:53-63 (card
  # papers/digests/PMID_1824743.md) and attributed to Scott and Stanski.
  # Scott 1987's own Table 4 prints the same V1 (2.19 L) and half-lives close
  # to the set's (harmonic means 0.67, 14 and 118 min against 0.67, 13.4 and
  # 111), but mean clearances a little lower (CL 0.195, Q 1.266 and 0.224
  # L/min against 0.199, 1.434 and 0.247), so the set is not those means
  # (card papers/digests/PMID_3100765.md; recorded 2026-11-18).
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: no size scaling, the parameters were used as published.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = 2.1853    * size$volume,
    v2 = 6.698864  * size$volume,
    v3 = 14.52582  * size$volume,
    cl1 = 0.1988623 * size$clearance,
    cl2 = 1.433557  * size$clearance,
    cl3 = 0.2469389 * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 1.4		# see t_peaks.xls
  MEAC <- 39
  typical <- MEAC * 1.2
  lowerTypical <- MEAC * 0.8
  upperTypical <- MEAC * 2.0
  reference <- paste0(
    "Scott JC, Stanski DR. J Pharmacol Exp Ther 1987;240(1):159-166. ",
    "https://pubmed.ncbi.nlm.nih.gov/3100765/; the parameter set as printed in ",
    "Shafer SL, Varvel JR. Anesthesiology 1991;74:53-63, Table 1. ",
    "https://pubmed.ncbi.nlm.nih.gov/1824743/")

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
