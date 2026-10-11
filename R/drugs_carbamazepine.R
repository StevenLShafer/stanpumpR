# -----------------------------------------------------------------------------
# Carbamazepine: one compartment, apparent oral, chronic (autoinduced) state
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Graves NM et al. (Pharmacotherapy 1998;18:273-281): 829 adults on
# maintenance carbamazepine; one compartment, first-order absorption, mixed-
# effect modelling.  From the abstract:
#
#     CL/F = (0.0134 x TBW + 3.58) L/h, x 0.749 if age >= 70
#     V/F  = 1.97 L/kg x TBW
#     ka   = 0.441 /h
#
# at 70 kg, CL/F 4.52 L/h (0.065 L/h/kg) and a half-life of 21 h.
#
# THE CHRONIC (AUTOINDUCED) STATE ONLY
# ====================================
# Carbamazepine induces its own metabolism: clearance roughly doubles over the
# first few weeks (the induction half-life is about 70 h, Magnusson 2007, so
# induction is about 90% complete in 10 days).  stanpumpR's engine is time-
# invariant, so a single run cannot cross that transition.  This model is the
# INDUCED, steady-state kinetics a maintenance population shows, which is the
# clinically useful case.  A single dose in a naive patient is cleared far
# more slowly (half-life 25-65 h); that curve is not produced, and the help
# page says so.  Co-medication (phenytoin raises clearance about 40%) is not
# applied.
#
# THE EPOXIDE IS NOT PLOTTED
# ==========================
# Carbamazepine-10,11-epoxide is active and accumulates (about 10-15% of the
# parent).  A joint parent-epoxide model exists (PICME, Br J Clin Pharmacol
# 2021), but the epoxide is not a drug in the library and the engine's single
# metabolite link is not built here; the curve is the parent only.
#
# FORMULATIONS: suspension, immediate-release and chewable tablets share these
# parameters; the extended-release tablet (Tegretol-XR) is given a slower
# absorption and a relative bioavailability of 0.89 (the XR label, against the
# suspension), as "mg PO ER".  Absolute oral bioavailability is about 0.80
# (Marino 2012, intravenous stable-label), so the apparent clearance is about
# 0.8 of the true; the curve is oral, where the factor cancels.
#
# CHECKS (70 kg adult): half-life 21 h; 200 mg twice daily averages about 6.6
# mg/L at steady state, inside the 4-12 mg/L range; Marino 2012 measured an
# absolute clearance of about 0.04-0.05 L/h/kg at steady state.
#
# BODY SIZE: the model's own weight covariate (per-kilogram volume, clearance
# linear in weight), at the pharmacokinetic weight with the switch on.
#
# BAND: 4-12 mcg/mL (ILAE, Patsalos 2008), typical 8.
#
# References
# ----------
# Graves NM et al., Pharmacotherapy 1998;18:273-281.
#   (PMID 9545146)
# Magnusson MO et al., Clin Pharmacol Ther 2008;84:52-62.
#   https://doi.org/10.1038/sj.clpt.6100431
# Marino SE et al., Clin Pharmacol Ther 2012;91:483-491.
#   https://doi.org/10.1038/clpt.2011.251
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# -----------------------------------------------------------------------------

#' Carbamazepine pharmacokinetics (oral, chronic autoinduced state)
#'
#' Graves et al. (1998) maintenance model; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
carbamazepine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = 1.97 * w, v2 = 1, v3 = 1,
    cl1 = (0.0134 * w + 3.58) * (if (age >= 70) 0.749 else 1) / 60,
    cl2 = 0, cl3 = 0,
    ka_PO = 0.441 / 60,
    bioavailability_PO = 1,     # apparent
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 8, upperTypical = 12, lowerTypical = 4,
    reference = paste0(
      "Graves NM et al., Pharmacotherapy 1998;18:273-281: one compartment, ",
      "apparent oral, chronic (autoinduced) maintenance state, ",
      "CL/F = (0.0134 x weight + 3.58) L/h, V/F 1.97 L/kg, ka 0.441/h; ",
      "single-dose naive kinetics and the epoxide not modelled. PMID 9545146"
    ),
    oralFormulations = list(
      ER = list(ka_PO = 0.1 / 60, bioavailability_PO = 0.89, tlag_PO = 0)
    )
  )
}
