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
# a crossover with methylprednisolone and fitted: CL 18.1 L/h, Vc 41.6 L,
# k12 1.09 /h, k21 1.02 /h.  The macroparameters follow: Q = k12 Vc =
# 45.34 L/h, Vp = Q/k21 = 44.45 L, Vss 86.1 L.  Half-times 0.29 and 3.68 h.
# No covariates were fitted.
#
# DOSE BASIS
# ==========
# Dexamethasone products are labelled three ways: 4 mg of dexamethasone
# phosphate is 3.3 mg of dexamethasone base and 4.3 mg of the sodium
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
# References
# ----------
# Hong Y et al., Pharm Res 2007;24:1088-1097.
#   https://doi.org/10.1007/s11095-006-9232-x
# Spoorenberg SMC et al., Br J Clin Pharmacol 2014;78:78-83.
#   https://doi.org/10.1111/bcp.12295
# Krzyzanski W et al., J Pharmacokinet Pharmacodyn 2021;48:261-272.
#   https://doi.org/10.1007/s10928-020-09730-z
# -----------------------------------------------------------------------------

#' Dexamethasone pharmacokinetics
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dexamethasone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published parameters, unscaled
  # with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Hong 2007: CL 18.1 L/h, Vc 41.6 L, k12 1.09 /h, k21 1.02 /h
  vcRef <- 41.6
  k12   <- 1.09
  k21   <- 1.02

  v1  <- vcRef * size$volume
  v2  <- vcRef * k12 / k21 * size$volume          # 44.45 L at the reference
  v3  <- 1                                        # no third compartment
  cl1 <- 18.1 / 60 * size$clearance               # L/min
  cl2 <- vcRef * k12 / 60 * size$clearance        # Q = 45.34 L/h
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
