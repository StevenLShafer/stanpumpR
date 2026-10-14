# -----------------------------------------------------------------------------
# Tiagabine: one compartment, apparent oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.  Drafted by Claude Code, 2026-10-10,
# at the request of Steven L. Shafer; see the antiseizure registry,
# inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Ingwersen SH et al. (Eur J Pharm Sci 2000;11:247-254): 130 patients on
# tiagabine monotherapy (children to the elderly), 593 concentrations; one
# compartment, first-order absorption, NONMEM.  The final model (Table 4,
# Results and Discussion), for a subject of 170 cm:
#
#     CL/F = 6.10 x (height / 170)^2.13 L/h
#     V/F  = 52.0 x (height / 170)^3.44 L
#     ka   = 1.25 /h
#
# The abstract prints V/F as 62.0 L, a typographical error: with 6.10 L/h it
# gives a half-life of 7.0 h, while 52 L gives 5.9 h, the paper's own
# "population half-life 5.72 h".  Until 2026-10-14 this file carried the
# abstract's 62 L and, for want of the height exponents, the library's
# fat-free-mass scaling in place of the height terms.  Both covariates are
# now the paper's, so the model is used as published in both positions of
# the fat-free-mass switch: height is its own size covariate and nothing is
# left for the library's scaling to act on.  Population half-life 5.7 h.
#
# ENZYME INDUCTION NOT APPLIED: Ingwersen's was a monotherapy population.
# Samara 1998 found clearance about 67% higher with enzyme-inducing drugs
# (the app has no co-medication field).
#
# CHECKS (170 cm adult): half-life ln 2 x 52 / 6.10 = 5.9 h (paper 5.72 h;
# label 7-9 h without inducers); a single 4 mg dose peaks at about 60 ng/mL
# at 2.0 h, inside the label's fasted-to-fed Tmax range of about 45 min to
# 2.5 h.  (The figure this header gave until 2026-10-14, 25 ng/mL at 1.3 h,
# was never what the model produced.)
#
# BAND: 20-200 ng/mL (ILAE, Patsalos 2008), typical 50.  Tiagabine is 96%
# bound; the range is total drug.
#
# References
# ----------
# Ingwersen SH et al., Eur J Pharm Sci 2000;11:247-254.
#   https://doi.org/10.1016/s0928-0987(00)00109-3
# Samara EE et al., Epilepsia 1998;39:868-873.
#   https://doi.org/10.1111/j.1528-1157.1998.tb01182.x
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# -----------------------------------------------------------------------------

#' The height, cm, at which Ingwersen 2000 reports tiagabine's parameters
#' @keywords internal
TIAGABINE_REFERENCE_HEIGHT <- 170

#' Tiagabine pharmacokinetics (oral)
#'
#' Ingwersen et al. (2000) monotherapy model, with its height covariates on
#' both parameters; see the header of the file.  The fat-free-mass switch
#' does not act on it.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tiagabine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  h <- height / TIAGABINE_REFERENCE_HEIGHT

  default <- list(
    v1  = 52.0 * h^3.44, v2 = 1, v3 = 1,
    cl1 = 6.10 * h^2.13 / 60, cl2 = 0, cl3 = 0,
    ka_PO = 1.25 / 60,
    bioavailability_PO = 1,     # apparent
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 50, upperTypical = 200, lowerTypical = 20,
    reference = paste0(
      "Ingwersen SH et al., Eur J Pharm Sci 2000;11:247-254: one compartment, ",
      "apparent oral, monotherapy, CL/F 6.10 x (height/170)^2.13 L/h and V/F ",
      "52.0 x (height/170)^3.44 L (the abstract's 62 L is a misprint), ka 1.25/h. ",
      "https://doi.org/10.1016/s0928-0987(00)00109-3"
    )
  )
}
