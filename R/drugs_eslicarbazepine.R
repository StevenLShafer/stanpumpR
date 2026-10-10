# -----------------------------------------------------------------------------
# Eslicarbazepine: one compartment, apparent oral (from eslicarbazepine acetate)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Eslicarbazepine acetate (Aptiom) is a prodrug: hydrolysed almost completely
# on first pass to eslicarbazepine, the active moiety (systemic prodrug is
# about 0.01% of exposure).  The curve is eslicarbazepine.
#
# Falcao A et al. (CNS Drugs 2012;26:79-91) fitted 641 phase III patients,
# one compartment, first-order absorption:
#
#     CL/F = (2.36 + ... co-medication ...) x (WT/70)^0.75 L/h
#     V/F, ka from the Simulations Plus AAPS 2013 poster (P3.243): V/F 61.3 L,
#     absorption half-life 0.296 h, i.e. ka 2.34 /h, with CL/F 2.43 L/h at
#     the base.
#
# The handoff said enzyme inducers add 1.24 L/h to CL/F; the peer-reviewed
# value is +1.41 L/h (Falcao).  Neither is applied: the app has no
# co-medication field, so the curve is eslicarbazepine without inducers.
#
# DOSE BASIS
# ==========
# The dose is entered as mg of eslicarbazepine ACETATE (the product strength).
# Eslicarbazepine is 0.858 of that by molecular weight (254.3 / 296.3); since
# the apparent parameters were fitted to eslicarbazepine-equivalent dosing,
# the acetate dose is multiplied by 0.858 through bioavailability_PO.  That is
# a mass conversion on an apparent scale, recorded in the registry, not a
# measured bioavailability.
#
# CHECKS: half-life 17 h (label 13-20 h); once-daily dosing gives a trough of
# about 7.4 mg/L at 800 mg and 11 mg/L at 1200 mg (Sunkaraneni 2018), which
# the model reproduces with the 0.858 factor.
#
# RENAL: clearance falls in renal impairment (eslicarbazepine is renally
# cleared), but Falcao found no creatinine-clearance term in this cohort, so
# none is applied.
#
# BODY SIZE: the model's own allometric weight, at the pharmacokinetic weight
# with the switch on.
#
# BAND: 3-26 mcg/mL (the eslicarbazepine-specific range, Patsalos 2018),
# typical 15.
#
# References
# ----------
# Falcao A et al., CNS Drugs 2012;26:79-91.
#   https://doi.org/10.2165/11596290-000000000-00000
# Sunkaraneni S et al., J Pharmacokinet Pharmacodyn 2018;45:265-276.
#   https://doi.org/10.1007/s10928-018-9596-7
# Patsalos PN et al., Ther Drug Monit 2018;40:526-548.
#   https://doi.org/10.1097/FTD.0000000000000546
# -----------------------------------------------------------------------------

#' Mass of eslicarbazepine in a unit mass of eslicarbazepine acetate
ESLICARBAZEPINE_ACETATE_FRACTION <- 254.3 / 296.3

#' Eslicarbazepine pharmacokinetics (oral, from eslicarbazepine acetate)
#'
#' Falcao et al. (2012) with the Simulations Plus poster absorption; see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
eslicarbazepine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = 61.3 * (w / 70), v2 = 1, v3 = 1,
    cl1 = 2.43 * (w / 70)^0.75 / 60, cl2 = 0, cl3 = 0,
    ka_PO = 2.34 / 60,
    bioavailability_PO = ESLICARBAZEPINE_ACETATE_FRACTION,   # acetate -> moiety
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 15, upperTypical = 26, lowerTypical = 3,
    reference = paste0(
      "Falcao A et al., CNS Drugs 2012;26:79-91, with absorption from the ",
      "Simulations Plus AAPS 2013 poster: one compartment, apparent oral, the ",
      "active eslicarbazepine from eslicarbazepine acetate, CL/F 2.43 L/h ",
      "(no inducers), V/F 61.3 L, ka 2.34/h. ",
      "https://doi.org/10.2165/11596290-000000000-00000"
    )
  )
}
