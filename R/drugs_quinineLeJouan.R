# -----------------------------------------------------------------------------
# Quinine, oral: Le Jouan 2005, children with malaria (the children's
# candidate of the quinine group, R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg
# of quinine sulfate as dosed, concentrations in mcg/mL (= mg/L) of total
# quinine (base) in plasma.  Concentration only.  Drafted by Claude Code,
# 2026-11-28, at the request of Steven L. Shafer (docs/library-expansion/
# plan.csv, Antimalarials; 2026-11-28).  The candidate came through the
# nlmixr2lib cross-check (docs/nlmixr2lib/); nlmixr2lib's files were not used,
# and every number below was read on the PDF.
#
# SOURCE
# ======
# Le Jouan M, Jullien V, Tetanye E, Tran A, Rey E, Treluyer J-M, Tod M, Pons G.
# Quinine pharmacokinetics and pharmacodynamics in children with malaria
# caused by Plasmodium falciparum.  Antimicrob Agents Chemother
# 2005;49:3658-3662 (PMID 16127036; card papers/digests/PMID_16127036.md; read
# on the PDF, Table 2 and the Results).  30 children in Cameroon, 0.55-6.7
# years (2.8 +/- 1.7), 13.6 +/- 3.8 kg, with uncomplicated falciparum malaria,
# given oral quinine 8.3 mg/kg as base every 8 h for 5 days (a quinine
# formiate syrup); 5-9 venous samples each to 56 h.
#
# One compartment with first-order absorption, no lag.  Both apparent
# parameters scale with the unbound fraction fu, which rises as the
# inflammatory proteins of acute malaria fall (Table 2 and Methods):
#
#     CL/F = fu x (theta1 + theta5 x BW),  theta1 0 (fixed), theta5 0.53 L/h/kg
#     V/F  = fu x (theta2 + theta6 x BW),  theta2 57 L, theta6 3.8 L/kg
#     ka   = 0.934 /h
#     fu   = 0.001 /h x (t - 36) + 0.15, from 0 to 72 h of treatment
#
# THE UNBOUND FRACTION (Shafer, 2026-11-28, on the card's [decision]): the
# engine has no time-since-treatment covariate, so fu is fixed at its 36-hour
# value, 0.15, the intercept of the paper's equation.  For a 15 kg child that
# gives CL/F 1.19 L/h and V/F 17.1 L (the paper: 0.91 to 1.48 L/h and 13 to 21
# L between the start and 72 h).  The half-life, which fu does not change, is
# 9.9 h at 15 kg; the paper prints 9.1 h, which its own typical values (0.693 x
# 13 / 0.91) do not give.
#
# DOSE BASIS.  The paper's doses were quinine base; the US product is quinine
# sulfate, dosed as the salt, so a dose is converted to base by
# QUININE_BASE_PER_SULFATE (0.8287; R/drugs_quinineKloprogge.R), carried as the
# oral bioavailability.
#
# BODY SIZE.  The model's own linear weight terms are kept as fitted and
# evaluated at the pharmacokinetic weight with the switch on and at total
# weight with it off.  They are not held to the studied weights: a child
# outside them is extrapolated by the paper's own straight line.
#
# References
# ----------
# Le Jouan M et al., Antimicrob Agents Chemother 2005;49:3658-3662.
#   https://doi.org/10.1128/AAC.49.9.3658-3662.2005
# -----------------------------------------------------------------------------
LEJOUAN_FU     <- 0.15     # unbound fraction at 36 h, the equation's intercept
LEJOUAN_CL_KG  <- 0.53     # L/h/kg, unbound (theta5)
LEJOUAN_V0     <- 57       # L, unbound (theta2)
LEJOUAN_V_KG   <- 3.8      # L/kg, unbound (theta6)
LEJOUAN_KA     <- 0.934    # /h

#' Quinine, oral (Le Jouan 2005, children with malaria)
#'
#' The children's candidate of the quinine group; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
quinineLeJouan <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = LEJOUAN_FU * (LEJOUAN_V0 + LEJOUAN_V_KG * w),
    v2  = 1,
    v3  = 1,
    cl1 = LEJOUAN_FU * LEJOUAN_CL_KG * w / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = LEJOUAN_KA / 60,
    bioavailability_PO = QUININE_BASE_PER_SULFATE,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Le Jouan M et al., Antimicrob Agents Chemother 2005;49:3658-3662, Table 2 (30 ",
      "children 0.55-6.7 years with uncomplicated falciparum malaria): one compartment, ",
      "ka 0.934 /h, CL/F 0.15 x 0.53 L/h/kg x BW, V/F 0.15 x (57 L + 3.8 L/kg x BW), the ",
      "unbound fraction fixed at its 36-hour value 0.15; doses as quinine sulfate, the ",
      "model in base (x 0.8287); total plasma quinine. ",
      "https://doi.org/10.1128/AAC.49.9.3658-3662.2005")
  )
}
