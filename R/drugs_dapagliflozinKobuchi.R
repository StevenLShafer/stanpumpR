# -----------------------------------------------------------------------------
# Dapagliflozin (Kobuchi 2025): one compartment, oral (Farxiga)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: the source's HbA1c model is
# not plotted.  Drafted by Claude Code, 2026-10-17, at the request of Steven
# L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Kobuchi S, Sakai S, Terada R, Kato K-I, Hayakawa T, Sakaeda T.  Population
# pharmacokinetic-pharmacodynamic analysis of dapagliflozin in Japanese
# patients with type 2 diabetes.  Int J Med Sci 2025;22:2333-2341.  85
# outpatients on 5 mg once daily after breakfast, all also on metformin and
# a DPP-4 inhibitor, 415 trough concentrations.  One compartment, first-order
# absorption; Table 2 and Equation 6:
#
#     CL/F = 229.3 L/day x (BW / 77.0)^0.41
#     V/F  = 73.9 L, FIXED       ka = 57.4 /day, FIXED
#
# V/F and ka were fixed from Melin et al. (Br J Clin Pharmacol
# 2024;90:606-612) because "all plasma dapagliflozin concentrations were
# measured at trough time points".  BW is the body weight at sampling.
#
# TROUGHS ONLY.  Such data fix the average and trough concentrations at
# steady state (dose rate over clearance), not the shape within a day.  With
# the fixed volume the half-life is 5.4 h, against the label's 12.9 h; the
# peak and the fall after it are the fixed parameters', not this
# population's.  The help page says so.
#
# BODY SIZE: the model's own weight covariate, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off;
# V/F, which has no weight term, is scaled by the library factor
# (legacyVolume = 1).  Neither renal function nor sex affected CL/F.
#
# A SECOND MODEL.  `dapagliflozin` is the sponsor's model (Melin 2023, 2744
# patients), the source of this model's fixed V/F and ka; this one is kept
# as its own drug for the Japanese troughs it was fitted to, whose CL/F,
# 9.6 L/h at 77 kg, is 2.4 times lower than Melin's 23.0 L/h.
#
# References
# ----------
# Kobuchi S et al., Int J Med Sci 2025;22:2333-2341.
#   https://doi.org/10.7150/ijms.111519
# Melin J et al., Br J Clin Pharmacol 2024;90:606-612 (Kobuchi's reference 11).
# -----------------------------------------------------------------------------

#' Dapagliflozin pharmacokinetics (Kobuchi 2025, oral)
#'
#' One compartment, first-order absorption (Kobuchi 2025); see the header of
#' the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dapagliflozinKobuchi <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = 73.9 * size$volume, v2 = 1, v3 = 1,
    cl1 = 229.3 * (pkW / 77.0)^0.41 / MINS_PER_DAY, cl2 = 0, cl3 = 0,
    ka_PO = 57.4 / MINS_PER_DAY,
    bioavailability_PO = 1,   # apparent CL/F and V/F
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Kobuchi 2025 (85 Japanese adults with type 2 diabetes, troughs on 5 mg ",
      "daily): one compartment, CL/F 229.3 L/day x (weight/77)^0.41, V/F 73.9 L ",
      "and ka 57.4/day fixed; only the steady-state trough is fitted; ",
      "concentration only. https://doi.org/10.7150/ijms.111519"
    )
  )
}
