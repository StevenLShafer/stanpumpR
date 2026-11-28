# -----------------------------------------------------------------------------
# Tecovirimat (TPOXX): oral, two compartments with a lag (Leeds 2013)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg,
# concentrations in ng/mL of tecovirimat in plasma.  Concentration only: no
# antiviral effect is plotted.  Drafted by Claude Code, 2026-11-28, at the
# request of Steven L. Shafer (docs/library-expansion/plan.csv, Antivirals;
# "Yes to all drugs in group 1", 2026-11-28).  The candidate came through the
# nlmixr2lib cross-check (docs/nlmixr2lib/); nlmixr2lib's files were not used,
# and every number below was read on the PDF.
#
# SOURCE
# ======
# Leeds JM, Fenneteau F, Gosselin NH, Mouksassi MS, Kassir N, Marier JF, Chen Y,
# Grosenbach D, Frimm AE, Honeychurch KM, Chinsangaram J, Tyavanagimatt SR,
# Hruby DE, Jordan R.  Pharmacokinetic and pharmacodynamic modeling to determine
# the dose of ST-246 to protect against smallpox in humans.  Antimicrob Agents
# Chemother 2013;57:1136-1143 (PMID 23254433; card
# papers/digests/PMID_23254433.md; read on the PDF, Table 1).  The human model
# was fitted to one placebo-controlled trial: 107 healthy volunteers aged 19 to
# 74, about half women, mean weight 78.4 kg, given 400 or 600 mg once daily
# for 14 days, not fasted, sampled richly on days 1 and 14 (1,395
# concentrations).  The same paper fits non-human primates; only the human
# model is used.
#
# Table 1, humans (apparent oral parameters):
#
#     ka    = 1.06 /h
#     Tlag  = 1.46 h
#     CL/F  = 41.15 L/h x (WT/78.4)^0.75
#     Vc/F  = 281.51 L (women), 217.44 L (men) x (WT/78.4)
#     Q/F   = 36.79 L/h x (WT/78.4)^0.75
#     Vp/F  = 413.53 L x (WT/78.4)
#
# DOSE BASIS.  The paper doses "ST-246" in mg without naming a salt; the US
# product's capsule holds tecovirimat monohydrate equivalent to 200 mg of
# tecovirimat (label, section 11), and its doses are in mg of tecovirimat.  So
# doses are entered as labelled, with no conversion (AGENTS.md, Dose basis).
#
# BODY SIZE.  The model carries its own allometric weight terms on every
# disposition parameter (exponents fixed at 0.75 and 1), kept as fitted and
# evaluated at the pharmacokinetic weight with the switch on and at total
# weight with it off, as for every model with its own weight covariate; the
# sex term on Vc/F is the paper's.  ka and the lag are not size quantities.
#
# ROUTE.  Oral only.  The intravenous product (TPOXX injection) exists, but
# these parameters are apparent oral ones and no intravenous model is in the
# archive, so no intravenous route is derived from them (anakinra's rule).
#
# RENAL AND HEPATIC FUNCTION: the label finds no clinically significant
# difference with renal or hepatic impairment for the capsule (12.3); the
# model has no term for either.
#
# PREGNANCY: no pharmacokinetic study (inst/extdata/pregnancyRegistry.csv).
#
# CHECKS (the published values, switch off)
# =========================================
# The label (12.3, not fitted here): at 600 mg every 12 hours in healthy adults
# under 120 kg, a steady-state AUC(0-24) of 29,816 ng.h/mL, Cmax 2159 and
# trough 845 ng/mL; and in 34 adults over 120 kg on the same dose, 19,500
# ng.h/mL, 1300 and 585.  The model's steady-state AUC(0-24) is 1200 mg / CL/F:
# 29,160 at 78.4 kg (2% below the label), and 19,960 at 130 kg (2% above).  At
# 78.4 kg (a man) the model's day-14 peak is 2155 ng/mL (label 2159), 2.9 h
# after the dose (label: median 6 h, range 2-24, with food), and its trough 610
# ng/mL, 28% below the label's 845; at 130 kg, 1403 and 438 (label 1300 and
# 585).  The troughs low by a quarter at both weights say the model's
# elimination at the end of a dosing interval is a little fast.  The model's
# terminal half-lives are the paper's own, 16.7 h in a man and 17.4 h in a
# woman of 78.4 kg; the label gives 19 h (CV 29%) and a CL/F of 31 L/h after a
# single 600 mg dose.
#
# References
# ----------
# Leeds JM et al., Antimicrob Agents Chemother 2013;57:1136-1143.
#   https://doi.org/10.1128/AAC.00959-12
# TPOXX (tecovirimat) capsules and injection, prescribing information, SIGA
#   Technologies, NDA 208627 and 214518, revised 1/2026 (DailyMed setid
#   fce826ab-4d6a-4139-a2ee-a304a913a253).
# -----------------------------------------------------------------------------
TECOVIRIMAT_WT_REF <- 78.4     # kg, the trial's mean weight
TECOVIRIMAT_KA     <- 1.06     # /h
TECOVIRIMAT_TLAG   <- 1.46     # h
TECOVIRIMAT_CL     <- 41.15    # L/h
TECOVIRIMAT_VC_F   <- 281.51   # L, women
TECOVIRIMAT_VC_M   <- 217.44   # L, men
TECOVIRIMAT_Q      <- 36.79    # L/h
TECOVIRIMAT_VP     <- 413.53   # L

#' Tecovirimat pharmacokinetics (oral)
#'
#' Two compartments with first-order absorption after a lag (Leeds 2013,
#' healthy volunteers); see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tecovirimat <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  wcl <- (w / TECOVIRIMAT_WT_REF)^0.75
  wv  <- w / TECOVIRIMAT_WT_REF
  vc  <- if (sex == SEX_FEMALE) TECOVIRIMAT_VC_F else TECOVIRIMAT_VC_M

  default <- list(
    v1  = vc * wv,
    v2  = TECOVIRIMAT_VP * wv,
    v3  = 1,
    cl1 = TECOVIRIMAT_CL * wcl / 60,
    cl2 = TECOVIRIMAT_Q * wcl / 60,
    cl3 = 0,
    ka_PO = TECOVIRIMAT_KA / 60,
    bioavailability_PO = 1,     # apparent oral parameters
    tlag_PO = TECOVIRIMAT_TLAG * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Leeds JM et al., Antimicrob Agents Chemother 2013;57:1136-1143, Table 1 (107 ",
      "healthy volunteers, 400 or 600 mg once daily): two compartments, ka 1.06 /h, lag ",
      "1.46 h, CL/F 41.15 and Q/F 36.79 L/h x (WT/78.4)^0.75, Vc/F 281.51 L (women) or ",
      "217.44 L (men) and Vp/F 413.53 L x WT/78.4; doses in mg of tecovirimat as ",
      "labelled; plasma, concentration only. https://doi.org/10.1128/AAC.00959-12")
  )
}
