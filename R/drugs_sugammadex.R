# -----------------------------------------------------------------------------
# Sugammadex: two-compartment total-drug model (Kleijn 2011)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL of TOTAL sugammadex (free plus complexed).
#
# DISPOSITION
# ===========
# Kleijn et al. modelled free sugammadex, free rocuronium and their complex
# in 426 patients and 20 volunteers, assigning the complex the same
# disposition as free sugammadex.  Summing the free and complex states
# therefore gives an autonomous two-compartment model of TOTAL sugammadex,
# which is also what the assay measured.  With W total body weight in kg
# and CR the Cockcroft-Gault creatinine clearance in mL/min:
#
#     CL = 5.58 x [1 + 0.00378 (W - 74.5)] x [2 CR / (CR + 119)]^1.29   L/h
#     Vc = 4.42 x [1 - 0.00354 (W - 74.5)] x rS x (W/70)                L
#     Q  = 12.36 x (W/70)^0.75                                          L/h
#     Vp = 6.35 x exp[-0.00305 (CR - 119)] x (W/70)                     L
#
# rS is 1 for the source's non-Asian category and 0.84 for its Asian
# category; it is a reproduction term for the source population and is
# left at 1 here, as stanpumpR collects no such covariate.  The clearance
# receives no extra weight allometry: its renal term already carries size.
#
# Rocuronium binding, and the train-of-four recovery it produces, are NOT
# modelled.  This row is the sugammadex concentration alone; the rocuronium
# row is unaffected by giving sugammadex, which is the main limitation of
# simulating the two side by side.
#
# RENAL FUNCTION
# ==============
# CR is Cockcroft-Gault at the patient's serum creatinine from the Patient
# Profile, or at an ASSUMED NORMAL creatinine when none is entered
# (R/renalFunction.R; a child's creatinine, entered or assumed, is read on
# the adult scale).  Sugammadex is renally cleared and the label does not
# recommend it below 30 mL/min.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every parameter carries its own weight term, so the model is used with the
# pharmacokinetic weight (70 kg x FFM / FFM_ref) in place of W when the
# switch is on, and with total body weight when it is off.  Both the
# 74.5 kg centring and the 70 kg normalisation are kept where printed.
#
# GUARD: the two linear weight terms are evaluated at no more than
# SUGAMMADEX_WEIGHT_CAP.  Vc's term, 1 - 0.00354 (W - 74.5), reaches zero at
# 357 kg, and a 360 kg man with the fat-free-mass switch off (within the
# Patient Profile's range) got a negative volume, no curve and no message
# (2026-10-31).  The cap is well beyond the source population and changes
# nothing below it.
#
# [decision] Steven L. Shafer, 2026-10-31: 200 kg was chosen as the cap, over
# refusing such a patient with a message; to be confirmed.
#
# Doses are sugammadex equivalents, as the Bridion label states them.
#
# PREGNANCY
# =========
# Sugammadex is cleared almost entirely unchanged by the kidney.  Hodge 2026
# fitted a population model to 10 women at term given sugammadex after
# general anaesthesia for caesarean delivery (median 78 kg; no control
# group; only the abstract was read): V 13.9 L and CL 8.0 L/h, "at the upper
# end of published ranges from non-pregnant patients".  This model, for a
# 30-year-old 78 kg woman at the library's assumed creatinine of 0.8 mg/dL
# (Cockcroft-Gault 126.6 mL/min; the fat-free-mass switch off, so W is total
# weight), gives CL = 5.58 x [1 +
# 0.00378 x 3.5] x [2 x 126.6 / (126.6 + 119)]^1.29 = 5.88 L/h, so Hodge's
# women cleared it 1.36 times faster.  With the Patient Profile's Pregnant
# field on and the serum creatinine left blank, the creatinine clearance CR
# that the CLEARANCE term reads is multiplied by PREGNANCY_GFR_FACTOR = 1.5
# (R/constants.R), the rise in glomerular filtration of pregnancy (Ansari
# 2016): for that woman CR becomes 189.9 mL/min and CL = 5.58 x 1.0132 x
# 1.2295^1.29 = 7.38 L/h, within 8% of Hodge's 8.0 L/h.  Because the renal
# term saturates, CL rises by less than 1.5 (by 1.26 at that CR).
# The peripheral-volume term, exp[-0.00305 (CR - 119)], keeps the UNADJUSTED
# CR: passing the raised CR to it would shrink Vp from 6.9 to 5.7 L, against
# Hodge's larger volume (13.9 L, against Vc + Vp = 11.8 L here).  The volumes
# are otherwise unchanged; the 18% larger volume Hodge reports is not
# applied, being from one uncontrolled study whose Vc and Vp were not read.
# A measured creatinine already carries the rise in filtration (creatinine
# falls in pregnancy), so with one entered nothing is multiplied
# (pregnancyGfrFactor(), R/pregnancy.R).  Late pregnancy (term) is the state
# described.  (Claude Code, 2026-10-19, at the request of Steven L. Shafer;
# see R/pregnancy.R and inst/extdata/pregnancyRegistry.csv.)
#
# References
# ----------
# Kleijn HJ et al., Br J Clin Pharmacol 2011;72:415-433.
#   https://doi.org/10.1111/j.1365-2125.2011.04000.x
# Hodge AT, Tognolini AR, Liu X et al.  Int J Obstet Anesth 2026;66:104874.
#   https://doi.org/10.1016/j.ijoa.2026.104874 (pregnancy)
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy: the rise in
#   filtration)
# -----------------------------------------------------------------------------

#' Sugammadex pharmacokinetics (total)
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the serum
#'   creatinine blank, multiplies the creatinine clearance the clearance term
#'   reads by 1.5, the rise in filtration of pregnancy (see the file's
#'   header); the peripheral-volume term keeps the unadjusted value.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
SUGAMMADEX_WEIGHT_CAP <- 200   # kg; see GUARD in the header

sugammadex <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL, pregnant = FALSE)
{
  # Size scaling (see the header): the model's own weight terms are
  # evaluated at the pharmacokinetic weight with the switch on, total body
  # weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  W  <- 70 * size$volume
  Wl <- min(W, SUGAMMADEX_WEIGHT_CAP)          # the linear terms; GUARD above
  CR <- creatinineClearanceCG(W, age, sex,     # mL/min
                              adultEquivalentCreatinine(creatinine, age, sex))
  rS <- 1
  # Pregnancy (see the header): the clearance term alone reads the raised
  # creatinine clearance; the peripheral-volume term keeps CR.
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crCL <- CR * gfrPregnancy

  cl1 <- 5.58 * (1 + 0.00378 * (Wl - 74.5)) * (2 * crCL / (crCL + 119))^1.29 / 60
  v1  <- 4.42 * (1 - 0.00354 * (Wl - 74.5)) * rS * (W / 70)
  cl2 <- 12.36 * (W / 70)^0.75 / 60
  v2  <- 6.35 * exp(-0.00305 * (CR - 119)) * (W / 70)
  v3  <- 1                                     # no third compartment
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Sugammadex acts in plasma, by encapsulating rocuronium there; it has no
  # effect site of its own, and the reversal it produces is not modelled.
  tPeak <- 0
  MEAC  <- 0

  # Band, total mg/L: what 2-4 mg/kg produce over the first hour, after the
  # first 15 min or so, when 4 mg/kg is still above 30.
  typical      <- 10
  upperTypical <- 30
  lowerTypical <- 5

  reference <- paste0(
    "Kleijn HJ et al., Br J Clin Pharmacol 2011;72:415-433. ",
    "Two-compartment model of TOTAL sugammadex (free plus rocuronium ",
    "complex); creatinine clearance from the entered creatinine or an ",
    "assumed normal one; rocuronium binding not modelled. ",
    "https://doi.org/10.1111/j.1365-2125.2011.04000.x"
  )
  if (gfrPregnancy != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR,
             " in the clearance term only"),
      paste0(PREGNANCY_REFERENCE, "; checked against Hodge AT et al., ",
             "Int J Obstet Anesth 2026;66:104874. ",
             "https://doi.org/10.1016/j.ijoa.2026.104874")))
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
