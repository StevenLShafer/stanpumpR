# -----------------------------------------------------------------------------
# Quinine, oral: Kloprogge 2014, pregnant women with malaria (the adults'
# candidate of the quinine group, R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg
# of quinine sulfate as dosed, concentrations in mcg/mL (= mg/L) of total
# quinine (base) in plasma.  Concentration only.  Drafted by Claude Code,
# 2026-11-28, at the request of Steven L. Shafer (docs/library-expansion/
# plan.csv, Antimalarials; "Yes to all drugs in group 1", and "Build it, say
# so", 2026-11-28, on the [decision] recorded on the card).  The candidate came
# through the nlmixr2lib cross-check (docs/nlmixr2lib/); nlmixr2lib's files were
# not used, and every number below was read on the PDF.
#
# SOURCE
# ======
# Kloprogge F, Jullien V, Piola P, Dhorda M, Muwanga S, Nosten F, Day NPJ,
# White NJ, Guerin PJ, Tarning J.  Population pharmacokinetics of quinine in
# pregnant women with uncomplicated Plasmodium falciparum malaria in Uganda.  J
# Antimicrob Chemother 2014;69:3033-3040 (PMID 24970740; card
# papers/digests/PMID_24970740.md; read on the PDF, Tables 1 and 2).  22 women
# in the second or third trimester (13-37 weeks), 18-37 years (median 21),
# 44-71 kg (median 56.5), given oral quinine sulfate 10 mg salt/kg three times
# daily for 7 days; venous plasma to 184 h.  The doses were converted to
# quinine base (MW 324.42 from the sulfate's 782.96), so the parameters are
# in base.
#
# Table 2 (apparent oral parameters; F fixed at 100%):
#
#     ka   = 0.817 /h
#     CL/F = 10.4 L/h  x (WT/56.5)^(2/3)
#     Vc/F = 174 L     x WT/56.5
#     Q/F  = 10.7 L/h  x (WT/56.5)^(2/3)
#     Vp/F = 54.3 L    x WT/56.5
#
# The paper does not print the weight the allometric terms are divided by; the
# cohort's median, 56.5 kg, is used (its simulations take a "typical patient
# weighing 56 kg"; the difference is 0.6% on the clearances).
#
# POPULATION.  Every woman was pregnant and had malaria.  This is the only
# adult population model of quinine in the archive or in nlmixr2lib, so the
# quinine group gives it to every adult (Shafer, 2026-11-28: "Build it, say
# so"); the help page says so.  Two controlled studies of intravenous quinine
# in Sudanese women with malaria support that: Abdelrahim 2007 (8 pregnant
# against 8 non-pregnant) found no difference in Cmax, half-life or AUC, and
# Mirghani 2010 found the pregnant women's AUC 17% lower, not significantly
# (inst/extdata/pregnancyRegistry.csv).
#
# MALARIA COVARIATES, held at the reference (Shafer, 2026-11-28).
#   * Parasitaemia on F: +38.9% per log10 parasites/uL, time-varying.  The
#     term's centring is not printed; F is held at the fixed 100% to which
#     Table 2's parameters belong.
#   * Admission temperature on CL/F, exponential, -0.243 per degree (51.8%
#     lower at 39 than 36 C): held at the cohort median, 37.2 C, a factor of 1.
#   So the curve is a typical patient's of this cohort at its reference
#   covariates, not one whose fever or parasite load is entered.
#
# CHECKS (56.5 kg woman, switch off, 10 mg sulfate/kg three times daily, the
# trial's regimen).  The model's terminal half-life is 16.2 h; the paper's
# post hoc median is 15.3 h (10.4-30.8).  Its other post hoc medians fall on
# either side of the curve: the first dose's AUC(0-8) is 13.2 mg.h/L against
# the paper's 26.6 (16.0-53.2), and the peak after the last dose on day 7 is
# 6.2 mg/L against 4.06 (2.40-7.92).  Both are what a bioavailability that
# rises with parasitaemia gives: high in the acute phase, lower once the
# parasites are gone.  So F = 100% lies between the acute phase and recovery,
# and no single curve is the paper's typical patient; the help page says so.
#
# DOSE BASIS.  The US product is quinine sulfate (capsules of 324 mg), and its
# doses are in mg of the salt; the paper's parameters are in quinine base, and
# its text says so.  So a dose is entered as the sulfate and converted to base
# by 2 x 324.42 / 782.96 = 0.8287 (QUININE_BASE_PER_SULFATE), carried as the
# oral bioavailability (AGENTS.md, Dose basis).
#
# BODY SIZE.  The model's own allometric weight terms are kept as fitted and
# evaluated at the pharmacokinetic weight with the switch on and at total
# weight with it off.  ka is not a size quantity.
#
# References
# ----------
# Kloprogge F et al., J Antimicrob Chemother 2014;69:3033-3040.
#   https://doi.org/10.1093/jac/dku228
# -----------------------------------------------------------------------------
QUININE_BASE_PER_SULFATE <- 2 * 324.42 / 782.96   # mg base per mg sulfate dihydrate
KLOPROGGE_WT_REF <- 56.5     # kg, the cohort's median
KLOPROGGE_KA     <- 0.817    # /h
KLOPROGGE_CL     <- 10.4     # L/h
KLOPROGGE_VC     <- 174      # L
KLOPROGGE_Q      <- 10.7     # L/h
KLOPROGGE_VP     <- 54.3     # L

#' Quinine, oral (Kloprogge 2014, pregnant women with malaria)
#'
#' The adults' candidate of the quinine group; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
quinineKloprogge <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- (if (isTRUE(adjustToFFM)) size$pkWeight else weight) / KLOPROGGE_WT_REF
  wcl <- w^(2 / 3)

  default <- list(
    v1  = KLOPROGGE_VC * w,
    v2  = KLOPROGGE_VP * w,
    v3  = 1,
    cl1 = KLOPROGGE_CL * wcl / 60,
    cl2 = KLOPROGGE_Q * wcl / 60,
    cl3 = 0,
    ka_PO = KLOPROGGE_KA / 60,
    bioavailability_PO = QUININE_BASE_PER_SULFATE,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Kloprogge F et al., J Antimicrob Chemother 2014;69:3033-3040, Table 2 (22 pregnant ",
      "women with uncomplicated falciparum malaria): two compartments, ka 0.817 /h, CL/F ",
      "10.4 and Q/F 10.7 L/h x (WT/56.5)^(2/3), Vc/F 174 and Vp/F 54.3 L x WT/56.5; ",
      "parasitaemia and temperature held at the reference; doses as quinine sulfate, the ",
      "model in base (x 0.8287); total plasma quinine. https://doi.org/10.1093/jac/dku228")
  )
}
