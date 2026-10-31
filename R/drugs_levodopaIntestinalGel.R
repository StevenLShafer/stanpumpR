# -----------------------------------------------------------------------------
# Levodopa-carbidopa intestinal gel (Duopa, Duodopa): the levodopa
# disposition of R/drugs_levodopa.R, pumped into the jejunum and absorbed
# through one transit compartment
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of levodopa.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer.
#
# The source, the disposition, the effect site and what is not modelled are in
# the header of R/drugs_levodopa.R: Othman & Dutta 2014 (Br J Clin Pharmacol
# 78:94), confirmed in the FDA Clinical Pharmacology Review of NDA 203952,
# with Mao 2013's motor effect site.  For the gel the transit rate constant is
# 9.2 /h (four times the tablets' 2.4 /h: the gel is delivered past the
# stomach, whose emptying limits a tablet) and the bioavailability relative
# to tablets 0.97 (95% bootstrap interval 0.95-0.98).
#
# HOW IT IS GIVEN
# ===============
# Through a PEG-J tube from a portable pump: a morning dose (5-10 mL of the
# 20 mg/mL gel, 100-200 mg of levodopa, over 10-30 minutes), a continuous
# rate over the waking day (typically 2-6 mL/h, 40-120 mg/h, for 16 hours),
# and extra doses as needed.  The doses are "mg PO" and the continuous rate
# "mg/hr PO" (enteralRateUnits, R/constants.R), both into the gut, both
# absorbed through the transit compartment: the drug declares
# enteralInfusion and a transit chain of two compartments, and the
# matrix-exponential engine takes the doses and the rate into the head of the
# chain, exactly (R/enteral.R, R/advanceTransitChain.R).  A morning dose over
# 15 minutes is entered as a rate for those 15 minutes, or as a single
# "mg PO" dose.
#
# CHECKS: the paper's typical peak after a 200 mg morning dose is at 45
# minutes; the engine's, for 200 mg given over 15 minutes, is at 39 minutes
# (29 for an instantaneous dose, 50 for one over 30 minutes; the paper does
# not say how long its simulated morning dose took).  A continuous rate
# approaches rate x 0.97 / CL, 2.35 mcg/mL at 60 mg/h, slowly at the end,
# because the poorly estimated peripheral volume gives a terminal half-life
# near 10 h: 2.06 mcg/mL 8 hours after a 200 mg morning dose over 15
# minutes and 60 mg/h from then on (tests).  The label's
# median time to peak after starting the 16-hour infusion is 2.5 h.
#
# References
# ----------
# Othman AA, Dutta S. Br J Clin Pharmacol 2014;78:94-105.
#   https://doi.org/10.1111/bcp.12324
# FDA CDER, Clinical Pharmacology Review, NDA 203952 (Duopa).
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2015/203952Orig1s000ClinPharmR.pdf
# -----------------------------------------------------------------------------

#' Levodopa-carbidopa intestinal gel
#'
#' The disposition of \code{levodopa()}, absorbed from the jejunum through
#' one transit compartment at 9.2/h with relative bioavailability 0.97
#' (Othman & Dutta 2014), given as doses ("mg PO") and a continuous
#' enteral rate ("mg/hr PO").  See the header of
#' \code{R/drugs_levodopaIntestinalGel.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
levodopaIntestinalGel <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  default <- c(levodopaDisposition(weight, height, age, sex, adjustToFFM), list(
    ka_PO = LEVODOPA_KTR_GEL / 60,
    bioavailability_PO = LEVODOPA_FREL_GEL,   # relative to tablets
    tlag_PO = 0
  ))
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, ke0 = LEVODOPA_KE0 / 60, MEAC = 0,
    typical = 2.25, upperTypical = 3.5, lowerTypical = 1.0,
    transitChain = list(n = 2L, ktr = LEVODOPA_KTR_GEL / 60, kMetabolic = 0),
    enteralInfusion = TRUE,
    reference = paste0(
      "Othman & Dutta, Br J Clin Pharmacol 2014;78:94-105: the levodopa ",
      "disposition (CL/F 24.8 L/h, Vc/F 58.5 L x weight/70, Q/F 6.8 L/h, Vp/F ",
      "72.9 L), the gel absorbed from the jejunum through one transit ",
      "compartment at 9.2/h with bioavailability 0.97 relative to tablets; ",
      "motor effect site ke0 1.80/h (Mao 2013). https://doi.org/10.1111/bcp.12324"
    )
  )
}
