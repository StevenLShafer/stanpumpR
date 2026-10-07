# -----------------------------------------------------------------------------
# Clindamycin: one-compartment joint intravenous/oral model (Bouazza 2012)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.
#
# DISPOSITION
# ===========
# Bouazza et al. fitted total plasma clindamycin from 50 adults with
# osteomyelitis treated intravenously and orally.  The FINAL TABLE of that
# paper, not its abstract, gives: CL 15.2 x (BW/70)^0.497 L/h, V 66.2 L,
# ka 0.967 /h, oral F 0.876.  The abstract prints a different, earlier vector
# (CL 16.2, V 70.2, ka 0.92) that must not be mixed with these.
#
# Because the fit used both routes, 15.2 L/h is systemic clearance and 0.876
# is absolute bioavailability.  F is applied once, to oral doses only.
#
# The intravenous product is clindamycin PHOSPHATE, an inactive prodrug with
# a hydrolysis half-time of about six minutes.  Treating the labelled
# clindamycin-equivalent dose as direct parent input is the source model's
# approximation, and the only place it is visibly wrong is the first few
# minutes after a fast infusion.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source scaled clearance with total weight to the power 0.497 and left
# the volume fixed.  With the fat-free-mass switch on, the same 0.497 exponent
# is applied to the FAT-FREE-MASS ratio instead, and the volume follows the
# library convention (x FFM / FFM_ref).  With it off, the model is exactly
# the published one: CL x (weight/70)^0.497, V unscaled.
#
# NOT MODELLED
# ============
# Protein binding (saturable, to alpha-1 acid glycoprotein: see below), the
# active N-demethyl and sulfoxide metabolites, and the prodrug step.  The
# curve is total parent clindamycin.
#
# TIME UNTIL THRESHOLD: FREE DRUG AT THE MIC
# ==========================================
# The default threshold (endCe in drugDefaults_global.csv) is the TOTAL
# concentration at which FREE clindamycin equals the MIC of 0.5 mg/L for
# staphylococci: the CLSI/FDA susceptible breakpoint for Staphylococcus spp.
# (S <= 0.5, I 1-2, R >= 4 mg/L), also the EUCAST breakpoint for streptococci
# of groups A, B, C and G.  It is the conservative end of the defensible range:
# the EUCAST staphylococcal breakpoint and the S. aureus ECOFF are 0.25 mg/L.
# Clindamycin is the beta-lactam-allergy prophylaxis agent for skin flora.
#
# Binding to alpha-1 acid glycoprotein is saturable, so the free fraction is
# lowest at the low levels where free drug crosses the MIC.  Wulkersdorfer
# 2021 (900 mg IV, healthy adults, intravascular microdialysis and
# ultrafiltration) fitted a single saturable site, Kd 0.85 mg/L in vivo
# (1.16 in vitro):
#     C_total = C_free + Bmax C_free / (Kd + C_free).
# Bmax is not in the abstract; 12.7 mg/L reproduces its in vivo AUC ratio of
# 0.147 (14.9 mg/L its in vitro ratio of 0.139).  Free 0.5 mg/L is then
# 0.5 + 12.7 x 0.5 / 1.35 = 5.2 mg/L total (free fraction about 0.10; 5.0
# with the in vitro set).  Son 1998's human binding equation gives 5.9.  The
# constant 0.15 this header used to quote is the average over a whole 900 mg
# profile and overstates free drug at the crossing (it would give 3.3); older
# ex vivo ultrafiltration studies put the free fraction near 0.2 (2.5), but
# conflict with the in vivo microdialysis.  Raised AAG -- after surgery, in
# inflammation -- binds more, so the same free level needs more total drug.
# See R/antibioticThresholds.R.  (Free fraction about 0.10, threshold
# 5.2 mg/L, chosen by Steven L. Shafer, 2026-10-07.)
#
# References
# ----------
# Bouazza N et al., Br J Clin Pharmacol 2012;74:971-977.
#   https://doi.org/10.1111/j.1365-2125.2012.04292.x
# Wulkersdorfer B et al., J Antimicrob Chemother 2021;76:2106-2113.
#   https://doi.org/10.1093/jac/dkab140
# Son DS et al., J Vet Pharmacol Ther 1998;21:34-40 (human plasma binding).
#   https://doi.org/10.1046/j.1365-2885.1998.00111.x
# Diekema DJ et al., Open Forum Infect Dis 2019;6(Suppl 1):S47-S53 (MSSA 96%
#   susceptible to clindamycin).  https://doi.org/10.1093/ofid/ofy270
# -----------------------------------------------------------------------------

#' Clindamycin pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale the volume to the patient's fat-free mass and the
#'   clearance to that ratio to the published 0.497 power; when \code{FALSE},
#'   leave the volume unscaled and apply the 0.497 power to total body weight. No
#'   renal-function estimate.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
clindamycin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header).  Volume: library convention, unscaled when
  # the switch is off.  Clearance: the published 0.497 exponent, on the
  # fat-free-mass ratio when the switch is on and on weight/70 when it is off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  clSize <- if (isTRUE(adjustToFFM)) size$volume^0.497 else (weight / 70)^0.497

  # Bouazza 2012, final table
  v1  <- 66.2 * size$volume
  cl1 <- 15.2 * clSize / 60            # L/min
  v2  <- 1                             # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  # Oral: first-order absorption as published; the source bioavailability is
  # applied once, here.
  ka_PO              <- 0.967 / 60     # 1/min
  bioavailability_PO <- 0.876
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

  # Band, total mg/L: the source's working trough criterion of 2 mg/L, with
  # 1 and 4 mg/L either side.  Orientation only; binding is saturable (see the
  # header), free fraction about 0.1 at these levels.
  typical      <- 2
  upperTypical <- 4
  lowerTypical <- 1

  reference <- paste0(
    "Bouazza N et al., Br J Clin Pharmacol 2012;74:971-977. ",
    "Final-table one-compartment model, total plasma; oral F 0.876. ",
    "https://doi.org/10.1111/j.1365-2125.2012.04292.x"
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
