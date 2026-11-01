# -----------------------------------------------------------------------------
# Cefazolin: unbound two-compartment disposition (Komatsu 2024)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L).
#
# THE PLOTTED CONCENTRATION IS UNBOUND CEFAZOLIN
# ==============================================
# Komatsu et al. jointly fitted total and unbound serum cefazolin from 152
# adults having prostatectomy or nephrectomy; the clinical doses were infused
# over 10 minutes (Methods), while the paper's own simulations and abstract
# used a separate, 15-minute infusion assumption.  The
# disposition in their final model is written on UNBOUND concentration: a
# linear two-compartment system whose clearance acts on free drug, with total
# concentration recovered afterwards through a saturable albumin binding
# relationship, Ct = Cu + Bmax Cu / (Kd + Cu).
#
# stanpumpR's engine is linear, so it simulates the part of that model that
# is linear, the unbound disposition, and plots UNBOUND cefazolin.  That is
# also the concentration that matters pharmacodynamically: the cefazolin
# target is the fraction of the dosing interval the FREE concentration spends
# above the organism's MIC (fT > MIC).  Anyone comparing the curve against a
# measured TOTAL level must apply the source's binding relationship
# themselves, not a fixed multiplier: Ct = Cu + Bmax Cu / (Kd + Cu) with
# Bmax 319 mg/L x (albumin/32)^0.401 and Kd 28.7 mg/L, so the total/unbound
# ratio is 1 + Bmax/(Kd + Cu), concentration- (and albumin-) dependent, and
# FALLS as the unbound level rises because the binding term saturates.  Over
# a 2 g dose this model's own free-drug peak the ratio is near 5, rising to
# roughly 10 as the level falls toward the trough; Komatsu's reported group
# medians (unbound about 12% of total near incision, a ratio near 8-9) are
# evidence of the same relationship at the concentrations they sampled, not
# individual paired conversions and not a fixed 2-4x as earlier stated here.
#
# The source's two-compartment "volumes" (36.6 and 42.2 L) are dose-accounting
# volumes for free drug, not anatomical spaces; dividing a dose by them gives
# the free concentration directly, which is why the free curve is implemented
# exactly as published and not through a bound reservoir.
#
# RENAL FUNCTION
# ==============
# CLu = 29.3 x (CrCL / 70)^0.586 L/h, with CrCL the Cockcroft-Gault creatinine
# clearance in mL/min, at the patient's serum creatinine from the Patient
# Profile (a child's read on the adult scale).  When none is entered CrCL is
# estimated from age, sex and body size at an ASSUMED NORMAL creatinine
# (R/renalFunction.R); renal impairment is
# then not represented, and the curve in a patient with a raised creatinine
# will decline too fast.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source retained no body-size term on its volumes or on Qu.  Those
# parameters follow the library convention: volumes x FFM / FFM_ref,
# clearances x that ratio ^ 0.75, with the switch off leaving them as
# published.  The renal term already carries size through Cockcroft-Gault,
# which uses the pharmacokinetic weight with the switch on and total body
# weight with it off.  CLu itself receives no further size factor.
#
# NOT MODELLED
# ============
# Albumin (the binding capacity Bmax = 319 x (albumin/32)^0.401 mg/L) plays no
# part in the free curve and is not an input.  No active metabolite.  Doses
# are cefazolin equivalents: the sodium salt label already states them so.
#
# TIME UNTIL THRESHOLD: FREE DRUG AT THE MIC
# ==========================================
# The curve is UNBOUND cefazolin, so the default threshold (endCe in
# drugDefaults_global.csv) is the MIC itself: 2 mg/L for methicillin-
# susceptible S. aureus.  That is the EUCAST S. aureus ECOFF and the MSSA
# MIC90 at standard inoculum (Nannini 2009), the unbound target anaesthesia
# prophylaxis studies use (Eley 2020), and the CLSI Enterobacterales
# susceptible breakpoint (S <= 2), so it covers E. coli too.  It is not a
# staphylococcal breakpoint: CLSI dropped those in 2013 and EUCAST infers
# staphylococcal susceptibility from cefoxitin.  See R/antibioticThresholds.R.
#
# PREGNANCY
# =========
# Ansari, Carvalho, Shafer and Flood (Anesth Analg 2016;122:786-804, Table 7
# and text, from Elkomy 2014 and others): "Pregnancy increases the clearance
# of cefazolin likely because of increased renal excretion", with a larger
# volume of distribution, so that 2 g and a 3- to 6-hour interval are
# recommended.  The review prints no factor for cefazolin itself; its
# physiological statement is that renal blood flow and glomerular filtration
# are "increased 50% by the second trimester".  With the Patient Profile's
# Pregnant field on, the creatinine clearance the published renal term reads
# is multiplied by PREGNANCY_GFR_FACTOR = 1.5 (R/constants.R), which with
# Komatsu's exponent of 0.586 raises the unbound clearance by 1.5^0.586 =
# 1.27.  The larger volume is NOT applied, as the review gives no figure, so
# the simulated peak in a parturient is too high.  Komatsu's model is of
# unbound cefazolin, whose fraction rises as albumin falls in pregnancy; the
# unbound curve is the one plotted, and the free fraction is not an input.
# The factor is applied only when the serum creatinine is left blank
# (pregnancyGfrFactor()): a pregnant woman's measured creatinine is already
# low, and the renal estimate made from it already carries the rise.
# See R/pregnancy.R.
#
# References
# ----------
# Komatsu T et al., Antimicrob Agents Chemother 2024;68:e00267-24.
#   https://doi.org/10.1128/aac.00267-24
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy)
# Nannini EC et al., Antimicrob Agents Chemother 2009;53:3437-3441.
#   https://doi.org/10.1128/AAC.00317-09
# Eley VA et al., Anesth Analg 2020;131:199-207.
#   https://doi.org/10.1213/ANE.0000000000004766
# -----------------------------------------------------------------------------

#' Cefazolin pharmacokinetics (unbound)
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string
#' @param adjustToFFM \code{TRUE} (the default) scales the model to the
#'   patient's fat-free mass as described in the
#'   \href{https://github.com/StevenLShafer/stanpumpR/blob/master/docs/weight-adjustment.md}{weight-adjustment guide};
#'   \code{FALSE} reproduces the published size scaling exactly.  Each drug
#'   file's header says what the switch changes for that model; for cefazolin
#'   it also sets the weight the creatinine-clearance estimate uses.
#' @param creatinine the patient's serum creatinine in mg/dL, or NULL for the
#'   assumed normal value for the patient's age and sex (R/renalFunction.R)
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   creatinine clearance the renal term reads by 1.5, the rise in glomerular
#'   filtration of pregnancy (Ansari 2016; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cefazolin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  # Size scaling (see docs/weight-adjustment.md and the header).  The
  # published volumes and Qu carry no size term, so with the switch off they
  # are used as published (legacyVolume = 1).  pkW is the weight the renal
  # covariate sees: the pharmacokinetic weight with the switch on, total
  # weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl <- creatinineClearanceCG(pkW, age, sex,       # mL/min
                                adultEquivalentCreatinine(creatinine, age, sex))
  # Pregnancy (see the header): the renal term alone.
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- crcl * gfrPregnancy

  # Komatsu 2024, unbound: CLu 29.3 (CrCL/70)^0.586 L/h, Vcu 36.6 L,
  # Qu 54.4 L/h, Vpu 42.2 L
  cl1 <- 29.3 * (crcl / 70)^0.586 / 60   # L/min, renal covariate carries size
  v1  <- 36.6 * size$volume
  cl2 <- 54.4 / 60 * size$clearance
  v2  <- 42.2 * size$volume
  v3  <- 1                                # no third compartment
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

  # No effect site: an antibiotic's effect is its plasma exposure relative to
  # the MIC, and the engine has nothing to equilibrate it into.
  tPeak <- 0
  MEAC  <- 0

  # Band: free drug at the expected MBC of the common pathogens, placed on
  # the plotted curve (R/antibioticMbc.R).
  band         <- antibioticMbcBand("cefazolin")$plotted
  typical      <- band[["typical"]]
  upperTypical <- band[["upper"]]
  lowerTypical <- band[["lower"]]

  reference <- paste0(
    "Komatsu T et al., Antimicrob Agents Chemother 2024;68:e00267-24. ",
    "Unbound two-compartment model; the plotted concentration is UNBOUND ",
    "cefazolin. Creatinine clearance is from the entered creatinine or an ",
    "assumed normal one. https://doi.org/10.1128/aac.00267-24"
  )
  if (gfrPregnancy != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR)))
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
