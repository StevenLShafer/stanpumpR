# -----------------------------------------------------------------------------
# Dexamethasone: two-compartment intravenous model (Hong 2007) with oral and
# intramuscular routes from separate studies
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# DISPOSITION
# ===========
# Hong et al. gave intravenous dexamethasone phosphate to five healthy men in
# a five-way crossover (with hydrocortisone, methylprednisolone, oral
# prednisolone and placebo) and fitted: CL 18.1 L/h, Vc 41.6 L,
# k12 1.09 /h, k21 1.02 /h.  The macroparameters follow: Q = k12 Vc =
# 45.34 L/h, Vp = Q/k21 = 44.45 L, Vss 86.1 L.  Half-times 0.29 and 3.68 h.
# No covariates were fitted.
#
# DOSE BASIS
# ==========
# Dexamethasone products are labelled three ways: 4 mg of dexamethasone
# phosphate is 3.3 mg of dexamethasone base and 4.4 mg of the sodium
# phosphate.  Hong reported doses of the phosphate product without a full
# reconciliation to base in the retrieved methods, so the model is used on
# the convention the labels in common use follow, dexamethasone PHOSPHATE
# milligrams ("dexamethasone 4 mg" on a vial), and no further correction is
# applied.  A dose already stated as base equivalent is about 20% more
# potent per labelled milligram than the model assumes.
#
# EXTRAVASCULAR ROUTES: CROSS-STUDY
# =================================
# Oral bioavailability 0.81 is from Spoorenberg 2014, a parallel-group
# comparison in pneumonia patients (95% CI 0.54-1.21).  The oral absorption
# constant, 0.936 /h, and the intramuscular one, 0.460 /h, are from
# Krzyzanski 2021, a formal population model of oral and intramuscular
# dexamethasone phosphate in healthy Indian women, which also found oral
# availability 1.04 times intramuscular; the intramuscular F here is
# therefore 0.81 / 1.04 = 0.78.  Pairing those inputs with Hong's healthy-
# male disposition is a cross-study assembly, not a fitted joint model.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed published parameters: volumes scale with fat-free mass relative to
# the reference male, clearances with that ratio ^ 0.75; the switch off uses
# them as published.
#
# PREGNANCY
# =========
# Ansari, Carvalho, Shafer and Flood (Anesth Analg 2016;122:786-804, Table 8,
# from Tsuei 1980 and Pacheco 2007): "Clearance doubled in single gestation",
# by the placenta's own metabolism of the synthetic glucocorticoids, with a
# multiple gestation needing more still.  The primary data agree.  Tsuei 1980
# gave 8 mg of dexamethasone phosphate to women at term before caesarean
# delivery (six intravenously, six intramuscularly) and found a total plasma
# clearance of 559 mL/min, against 243 mL/min in nonpregnant women (a ratio
# of 2.30), with the plasma binding the same (66.6% against 68.1% bound) and
# the terminal half-life UNCHANGED (mean 142 min, n = 10); a clearance that
# doubles with an unchanged half-life means the volume doubled with it.
# Krzyzanski 2023, a minimal physiologically based meta-analysis of all the
# parturient data, estimated a maternal clearance of 26.7 L/h against 9.5
# L/h in nonpregnant women from the same group (2.81) and a maternal tissue
# partition coefficient of 2.37 against 1.07, a larger volume.
#
# With the Patient Profile's Pregnant field on, the elimination clearance CL1
# is multiplied by DEXAMETHASONE_PREGNANCY_CL_FACTOR = 2 (Ansari's figure,
# between Tsuei's 2.30 and Krzyzanski's 2.81 within-study ratios), and BOTH
# volumes by DEXAMETHASONE_PREGNANCY_V_FACTOR = 2 (Tsuei; added 2026-10-19).
# The distribution clearance Q is doubled too, because Hong fitted the
# distribution as the microconstants k12 and k21, and they are kept: Q =
# k12 x Vc follows the doubled Vc.  So every rate constant (k10, k12, k21) is
# Hong's, both half-lives (0.29 and 3.68 h) are unchanged as Tsuei measured,
# and a dose gives exactly half the nonpregnant concentration at every time.
# Doubling CL1 alone, as the library did from 2026-10-13, shortened the
# terminal half-life from 3.68 to 2.07 h, which contradicts the only
# intravenous data in pregnancy; doubling the volumes with Q left at Hong's
# 45.3 L/h would halve k12 and k21 and lengthen it to 4.13 h instead (and
# the distribution half-life from 0.29 to 0.52 h).
#
# NOT APPLIED: Krzyzanski 2023's faster intramuscular absorption (0.893
# against 0.460 /h) and slower oral absorption (0.446 against 0.936 /h) in
# pregnancy, which are compared across studies; the absorption parameters
# are unchanged.  Tsuei's absolute clearance, 33.5 L/h, is 1.85 times Hong's
# 18.1 L/h in men, which is a cross-study comparison and is not used.  A
# singleton pregnancy at term is assumed.  (Claude Code, 2026-10-19, at the
# request of Steven L. Shafer; see R/pregnancy.R and
# inst/extdata/pregnancyRegistry.csv.)
#
# References
# ----------
# Hong Y et al., Pharm Res 2007;24:1088-1097.
#   https://doi.org/10.1007/s11095-006-9232-x
# Spoorenberg SMC et al., Br J Clin Pharmacol 2014;78:78-83.
#   https://doi.org/10.1111/bcp.12295
# Krzyzanski W et al., J Pharmacokinet Pharmacodyn 2021;48:261-272.
#   https://doi.org/10.1007/s10928-020-09730-z
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy)
# Tsuei SE et al.  Disposition of synthetic glucocorticoids.  II.
#   Dexamethasone in parturient women.  Clin Pharmacol Ther 1980;28:88-98.
#   PMID 7389259.
#   https://doi.org/10.1038/clpt.1980.136 (pregnancy: the volume factor)
# Krzyzanski W, Milad MA, Jobe AH, Jusko WJ.  CPT Pharmacometrics Syst
#   Pharmacol 2023;12:668-680.  https://doi.org/10.1002/psp4.12899
#   (pregnancy)
# -----------------------------------------------------------------------------

# Elimination clearance in pregnancy relative to Hong's value: Ansari 2016,
# "clearance doubled in single gestation" (see the header).
DEXAMETHASONE_PREGNANCY_CL_FACTOR <- 2
# Both volumes in pregnancy relative to Hong's values: Tsuei 1980, clearance
# 2.3 times higher at term with an unchanged terminal half-life, so the volume
# rose with it (see the header).  Q follows Vc through Hong's k12.
DEXAMETHASONE_PREGNANCY_V_FACTOR <- 2

#' Dexamethasone pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @param pregnant is the patient pregnant?  \code{TRUE} doubles the
#'   elimination clearance (Ansari 2016) and both volumes, with Q following
#'   the central volume, so the half-lives are unchanged (Tsuei 1980; see the
#'   file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dexamethasone <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          pregnant = FALSE)
{
  # Size scaling (see the header): fixed published parameters, unscaled
  # with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Hong 2007: CL 18.1 L/h, Vc 41.6 L, k12 1.09 /h, k21 1.02 /h
  vcRef <- 41.6
  k12   <- 1.09
  k21   <- 1.02

  # Pregnancy (see the header): CL1 x 2, both volumes x 2, and Q with Vc
  # through Hong's k12, so every rate constant is unchanged.
  clPregnancy <- if (isTRUE(pregnant)) DEXAMETHASONE_PREGNANCY_CL_FACTOR else 1
  vPregnancy  <- if (isTRUE(pregnant)) DEXAMETHASONE_PREGNANCY_V_FACTOR  else 1

  v1  <- vcRef * size$volume * vPregnancy
  v2  <- vcRef * k12 / k21 * size$volume * vPregnancy   # 44.45 L at the reference
  v3  <- 1                                        # no third compartment
  cl1 <- 18.1 / 60 * size$clearance * clPregnancy # L/min
  cl2 <- vcRef * k12 / 60 * size$clearance * vPregnancy # Q = 45.34 L/h
  cl3 <- 0

  # Oral and intramuscular: cross-study, see the header
  ka_PO              <- 0.936 / 60                # 1/min, Krzyzanski 2021
  bioavailability_PO <- 0.81                      # Spoorenberg 2014
  tlag_PO            <- 0
  ka_IM              <- 0.460 / 60                # 1/min, Krzyzanski 2021
  bioavailability_IM <- 0.81 / 1.04               # oral / IM ratio 1.04
  tlag_IM            <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO,
    ka_IM = ka_IM,
    bioavailability_IM = bioavailability_IM,
    tlag_IM = tlag_IM
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # genomic effect over hours; no effect-site model
  MEAC  <- 0

  # Band, ng/mL: what 4-8 mg produce over the first hours.  Orientation.
  typical      <- 50
  upperTypical <- 100
  lowerTypical <- 20

  reference <- paste0(
    "Hong Y et al., Pharm Res 2007;24:1088-1097 (intravenous two-compartment ",
    "model, phosphate-labelled dose); oral F 0.81 from Spoorenberg 2014; oral ",
    "and intramuscular absorption from Krzyzanski 2021. ",
    "https://doi.org/10.1007/s11095-006-9232-x"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", DEXAMETHASONE_PREGNANCY_CL_FACTOR)), "; ",
      pregnancyReference(
        paste0("volumes and Q x ", DEXAMETHASONE_PREGNANCY_V_FACTOR,
               ", half-lives unchanged"),
        paste0("Tsuei SE et al., Clin Pharmacol Ther 1980;28:88-98. ",
               "https://doi.org/10.1038/clpt.1980.136")))
  }

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
