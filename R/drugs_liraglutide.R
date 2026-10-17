# -----------------------------------------------------------------------------
# Liraglutide: one compartment, subcutaneous once daily (Victoza)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (1 nmol/L = 3.751 ng/mL; molecular weight
# 3751.2 g/mol).  Concentration only: the source's integrated meal
# glucose-insulin model is not plotted.  Drafted by Claude Code, 2026-10-17,
# at the request of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Roge RM, Klim S, Ingwersen SH, Kjellsson MC, Kristensen NR.  The effects of
# a GLP-1 analog on glucose homeostasis in type 2 diabetes mellitus quantified
# by an integrated glucose insulin model.  CPT Pharmacometrics Syst Pharmacol
# 2015;4:e00011 (PMC4369758).  29 people with type 2 diabetes, 0.6, 1.2 and
# 1.8 mg once daily.  "A one-compartment model with first-order absorption and
# first-order elimination"; a zero- then first-order input was tried and did
# not improve the fit.  Table 1, with clearance and volume "assumed to be
# proportional to weight":
#
#     V/F  = 0.112 L/kg
#     CL/F = 1.83e-4 L/min/kg   (0.77 L/h at 70 kg)
#     ka   = 9.41e-4 /min       (0.056 /h)
#
# The mantissas were read from the paper's table; its powers of ten were lost
# in the available text and are the ones the specification gives, which the
# label's 13-hour half-life confirms (the model: elimination half-life 7.1 h,
# absorption half-time 12.3 h, so a flip-flop 12 h apparent half-life, and a
# peak 13 h after a dose).
#
# WATSON 2010.  The pooled healthy-volunteer and diabetes fit of Watson et al.
# (J Clin Pharmacol 2010;50:886-894; sequential zero- and first-order
# absorption) is the other published model.  Its parameters could not be read
# from the available abstract, so it stays in the registry; the engine can
# carry its sequential input (zeroOrderSC, into = "depot", R/subcutaneous.R)
# once they are transcribed.
#
# BODY SIZE: per kilogram, evaluated at the pharmacokinetic weight with the
# switch on and at total weight with it off.
#
# References
# ----------
# Roge RM et al., CPT Pharmacometrics Syst Pharmacol 2015;4:e00011.
#   https://doi.org/10.1002/psp4.11
# Watson E et al., J Clin Pharmacol 2010;50:886-894.
#   https://doi.org/10.1177/0091270009354996
# -----------------------------------------------------------------------------

#' Liraglutide pharmacokinetics (subcutaneous, once daily)
#'
#' One compartment with first-order absorption (Roge 2015); see the header of
#' the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
liraglutide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = 0.112 * pkW, v2 = 1, v3 = 1,
    cl1 = 1.83e-4 * pkW, cl2 = 0, cl3 = 0,
    ka_SC = 9.41e-4,
    bioavailability_SC = 1,   # apparent CL/F and V/F
    tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Roge 2015 (29 adults with type 2 diabetes): one compartment, V/F 0.112 L/kg, ",
      "CL/F 1.83e-4 L/min/kg, ka 9.41e-4/min; concentration only. ",
      "https://doi.org/10.1002/psp4.11"
    )
  )
}
