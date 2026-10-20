# -----------------------------------------------------------------------------
# Cefalexin (cephalexin): oral only, apparent one-compartment model
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.
#
# ORAL ONLY, BECAUSE THE PARAMETERS ARE APPARENT
# ==============================================
# Haynes et al. fitted oral cefalexin (capsules and suspension) in 15
# children with musculoskeletal infection: CL/F 13.98 x (WT/70)^0.75 L/h,
# V/F 26.63 x (WT/70) L, ka 1.79 /h, no lag.  Those are clearance and volume
# divided by an unmeasured bioavailability.  They predict oral concentrations
# correctly, because F cancels, and intravenous ones wrong by 1/F, so
# cefalexin is offered as an oral unit only and bioavailability is carried
# as 1 because the apparent scale already contains it.  (There is no
# intravenous cefalexin product in any case.)
#
# PEDIATRIC ORIGIN, ADULT EXTRAPOLATION
# =====================================
# The 70 kg normalisation is a convention of the fit, not evidence that the
# model describes adults.  Ryder et al. (2026) used this vector to simulate
# adults, scaling both CL/F and V/F linearly with weight/70, supplemented it
# with oral pharmacokinetic anchors from published adult summary data, and
# judged it fit for that purpose: precedent for the extrapolation, and a
# check against adult means, but not a validation in adults.  The children had normal renal function and no renal covariate was
# fitted; the allometric weight term does not stand in for one.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Published as allometry on total weight (volumes x WT/70, clearance x
# (WT/70)^0.75).  With the switch on the same exponents apply to the
# fat-free-mass ratio; with it off the published total-weight scaling is
# reproduced exactly.
#
# NOT MODELLED
# ============
# Protein binding (about 15%), renal impairment, food and formulation
# differences.  Doses are cefalexin equivalents of the monohydrate product.
#
# TIME UNTIL THRESHOLD: FREE DRUG AT THE MIC
# ==========================================
# The default threshold (endCe in drugDefaults_global.csv) is the TOTAL
# concentration at which FREE cefalexin equals the MIC of 4 mg/L for
# methicillin-susceptible S. aureus: the MSSA MIC90 (Haynes 2022: MIC50 2,
# MIC90 4 mg/L, none above 4) and the target of Haynes 2024's free-drug
# attainment analysis.  Neither CLSI nor EUCAST has a human cefalexin
# breakpoint for staphylococci (inferred from oxacillin or cefoxitin).
# Binding is low and linear, about 12-15% (label 10-15%; Singhvi 1977 12.4%
# by ultrafiltration), so the threshold is MIC / fu = 4 / 0.85 = 4.7 mg/L.
# See R/antibioticThresholds.R.
#
# PREGNANCY
# =========
# No study of cefalexin's pharmacokinetics in pregnancy with a non-pregnant
# comparator was found: Creatsas 1980 measured serum and amniotic levels
# after 1 g orally in 60 pregnant women with no control group.  The closest
# analogue, cephradine (oral, renally cleared, little bound, nearly the same
# structure), was studied by Philipson and colleagues in 12 women during and
# after pregnancy: its intravenous AUC was 39% smaller in pregnancy, a
# clearance ratio of 1 / (1 - 0.39) = 1.64, its half-life 26% shorter, and its
# oral bioavailability did not differ.  Cefalexin itself is 70 to 100%
# excreted unchanged in the urine (Griffith 1983), so it should follow the
# 50% rise in glomerular filtration of pregnancy.  With the Patient Profile's
# Pregnant field on, the apparent clearance CL/F is multiplied by
# CEFALEXIN_PREGNANCY_CL_FACTOR = 1.5: the rise in filtration applied to the
# whole clearance, since this model has no renal term, and smaller than
# cephradine's 1.64.  V/F and the absorption are unchanged (cephradine's
# bioavailability did not change).  This is an extrapolation by analogy, with
# no cefalexin data behind it, and, the model having no creatinine input, it
# applies whatever the patient's renal function.  Late pregnancy is the
# reference state.  (Claude Code, 2026-10-19.)
#
# References
# ----------
# Haynes AS et al., Antimicrob Agents Chemother 2024;68:e00182-24.
#   https://doi.org/10.1128/aac.00182-24
# Haynes AS et al., Microbiol Spectr 2022;10:e01039-22.
#   https://doi.org/10.1128/spectrum.01039-22
# Singhvi SM et al., J Lab Clin Med 1977;89:414-420.  PMID 833477.
# Ryder JH et al., Pharmacotherapy 2026;46(7):e70179.
#   https://doi.org/10.1002/phar.70179
# Philipson A, Stiernstedt G, Ehrnebo M.  Comparison of the pharmacokinetics
#   of cephradine and cefazolin in pregnant and non-pregnant women.  Clin
#   Pharmacokinet 1987;12:136-144.
#   https://doi.org/10.2165/00003088-198712020-00004 (pregnancy)
# Griffith RS.  The pharmacology of cephalexin.  Postgrad Med J 1983;59
#   Suppl 5:16-27.  PMID 6364086.  (pregnancy: urinary excretion)
# Creatsas G et al.  A study of the kinetics of cephapirin and cephalexin in
#   pregnancy.  Curr Med Res Opin 1980;7:43-46.  PMID 7428412.  (pregnancy)
# -----------------------------------------------------------------------------

# Apparent clearance in late pregnancy relative to the published value: the
# 50% rise in glomerular filtration, by analogy with cephradine's paired
# clearance ratio 1 / (1 - 0.39) = 1.64 (Philipson 1987; see the header).
CEFALEXIN_PREGNANCY_CL_FACTOR <- 1.5

#' Cefalexin pharmacokinetics (oral, apparent)
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published allometry on total body weight (volumes linear, clearances to the
#'   0.75 power). No renal-function estimate.
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   apparent clearance by 1.5, by analogy with cephradine (Philipson 1987;
#'   see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cefalexin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      pregnant = FALSE)
{
  # Size scaling (see the header): published allometry on total weight,
  # reproduced exactly with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)

  # Haynes 2024, apparent: CL/F 13.98 L/h, V/F 26.63 L at 70 kg
  v1  <- 26.63 * size$volume
  # Pregnancy (see the header): the apparent clearance only.
  clPregnancy <- if (isTRUE(pregnant)) CEFALEXIN_PREGNANCY_CL_FACTOR else 1
  cl1 <- 13.98 * size$clearance / 60 * clPregnancy    # L/min
  v2  <- 1                              # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  ka_PO              <- 1.79 / 60       # 1/min, as published
  bioavailability_PO <- 1               # the apparent scale already carries F
  tlag_PO            <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # no effect site: exposure against the MIC is the effect
  MEAC  <- 0

  # Band: free drug at the expected MBC of the common pathogens, placed on
  # the plotted curve (R/antibioticMbc.R).
  band         <- antibioticMbcBand("cefalexin")$plotted
  typical      <- band[["typical"]]
  upperTypical <- band[["upper"]]
  lowerTypical <- band[["lower"]]

  reference <- paste0(
    "Haynes AS et al., Antimicrob Agents Chemother 2024;68:e00182-24. ",
    "Apparent (CL/F, V/F) oral model fitted in children and extrapolated to ",
    "adults; oral only. https://doi.org/10.1128/aac.00182-24"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("apparent clearance x ", CEFALEXIN_PREGNANCY_CL_FACTOR,
             ", by analogy with cephradine"),
      paste0("Philipson A et al., Clin Pharmacokinet 1987;12:136-144. ",
             "https://doi.org/10.2165/00003088-198712020-00004")))
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
