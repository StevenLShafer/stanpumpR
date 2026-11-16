# -----------------------------------------------------------------------------
# Linezolid (Boak 2014): one compartment, three absorption lag compartments,
# renal and nonrenal clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mg/L (mcg/mL).
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked, and on 2026-11-13 approved the trough band below as an exception to
# the antibiotics' MBC rule.  The numbers below were read from the full text
# of the source (the PubMed Central page of PMC4023770: Methods, equations 1-5
# and 12-13, Table 2 and its footnotes) and from the Zyvox label (NDA 021130,
# 2026), read in full.
#
# THE SOURCE
# ==========
# Boak LM, Rayner CR, Grayson ML, et al.  Clinical population
# pharmacokinetics and toxicodynamics of linezolid.  Antimicrob Agents
# Chemother 2014;58:2334-2343 (doi 10.1128/AAC.01885-13).  41 adults (42
# courses) at three hospitals, 600 mg intravenously or orally every 12 hours
# for 5-54 days, plasma before and 2, 4 and 8 h after one dose (161
# concentrations), HPLC; S-ADAPT.  "A one-compartment disposition model with
# first-order elimination and three absorption lag compartments"; a
# peripheral compartment and saturable elimination did not improve the fit.
# "Oral bioavailability was initially estimated and eventually fixed at
# 100%".  Table 2, at a standard weight of 65 kg and creatinine clearance of
# 120 mL/min:
#
#     Tlag 69.2 min, klag = 3 / Tlag      Tabs1/2 10.3 min, ka = ln 2 / Tabs1/2
#     CLNR 4.55 L/h    CLR 2.17 L/h       Vc 44.3 L
#
#     GFR = (140 - age) / SCR x 65 / 72 x FSex (FSex 0.85 in women; SCR mg/dL)
#     RF  = GFR / 120
#     CL  = FSize,CL x (RF x CLR + CLNR),  FSize,CL = (WT / 65)^0.75
#     V   = Vc x WT / 65
#
# with WT "the lower value of ideal body weight (IBW) (27) and total body
# weight", reference 27 being Devine 1974.  Cockcroft-Gault is evaluated at a
# fixed 65 kg, as printed.
#
# THE ABSORPTION
# ==============
# Three lag compartments emptying at klag into the gut, which empties at ka,
# is a chain of four: `transitChain` with n = 4, ktr = klag and the last at
# ka_PO, run exactly by R/advanceTransitChain.R.  Intravenous doses enter the
# central compartment.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own size term, at WT = min(ideal body weight, the
# pharmacokinetic weight with the fat-free-mass switch on or total weight
# with it off).  Devine's ideal body weight is not defined below 5 feet and
# goes negative in a small child, so below 152.4 cm, or under 18 years (the
# source studied adults), WT is the pharmacokinetic (or total) weight.
#
# CHECKED AGAINST THE LABEL (not used in the fit)
# ===============================================
# Zyvox Table 8, 600 mg intravenously every 12 hours: AUC0-12 89.7 mg.h/L,
# half-life 4.8 h, clearance 123 mL/min (7.4 L/h).  This model at the
# reference man: clearance 6.5 L/h, half-life 4.7 h, AUC0-12 92.  The label
# says the pharmacokinetics are "not altered in patients with any degree of
# renal impairment"; Boak, Meagher 2003 and Crass 2019 (31% lower clearance
# at an eGFR of 60 or less) all found a renal term, and the label's own Table
# 10 shows the single-dose AUC rising from 110 to 128 mg.h/L as creatinine
# clearance falls below 80, so the source's renal term is kept (AGENTS.md: a
# label is frozen at approval).
#
# RENAL FUNCTION AND PREGNANCY
# ============================
# A blank serum creatinine is an assumed normal value for age and sex
# (assumedCreatinine()), put on the adult scale for a child
# (adultEquivalentCreatinine()).  In pregnancy the GFR takes the 50% rise in
# filtration of late pregnancy when the creatinine is blank
# (pregnancyGfrFactor()); one case report (van Kampenhout 2017, quoted by
# Groen 2023) found exposure lower in pregnancy, which is recorded only.
#
# THE MIC LINE AND THE BAND
# =========================
# The MIC line is Staphylococcus aureus, including MRSA, at 2 mg/L, free
# fraction 0.69 (antibioticMicTable()).  The band is NOT an MBC band:
# linezolid is bacteriostatic against staphylococci and enterococci, so the
# band is the ESICM/ESCMID trough range, 2-7 mg/L (antibioticTroughBands(),
# R/antibioticMbc.R).
# -----------------------------------------------------------------------------

# Boak 2014, Table 2
LINEZOLID_TLAG     <- 69.2    # min, mean lag time; klag = 3 / Tlag
LINEZOLID_TABS     <- 10.3    # min, absorption half-life
LINEZOLID_CL_NR    <- 4.55    # L/h at 65 kg
LINEZOLID_CL_R     <- 2.17    # L/h at 65 kg and a GFR of 120 mL/min
LINEZOLID_V        <- 44.3    # L at 65 kg
LINEZOLID_WT_STD   <- 65      # kg
LINEZOLID_GFR_STD  <- 120     # mL/min

#' Linezolid (Boak 2014)
#'
#' One compartment, three absorption lag compartments, renal and nonrenal
#' clearance; see the header of \code{R/drugs_linezolid.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant third trimester (TRUE) or not
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
linezolid <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  wt <- if (age < 18 || height < 152.4) pkW else
    min(idealBodyWeightDevine(height, sex), pkW)
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  gfr <- (140 - age) / scr * LINEZOLID_WT_STD / 72 *
    (if (sex == SEX_FEMALE) 0.85 else 1) * gfrPregnancy
  cl <- (wt / LINEZOLID_WT_STD)^0.75 *
    (gfr / LINEZOLID_GFR_STD * LINEZOLID_CL_R + LINEZOLID_CL_NR)
  klag <- 3 / LINEZOLID_TLAG                     # 1/min

  default <- list(
    v1 = LINEZOLID_V * wt / LINEZOLID_WT_STD, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0,
    ka_PO = log(2) / LINEZOLID_TABS,
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL") else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Boak LM et al., Antimicrob Agents Chemother 2014;58:2334-2343: one ",
    "compartment, three absorption lag compartments (Tlag 69.2 min, Tabs1/2 ",
    "10.3 min), CL (WT/65)^0.75 x (4.55 + 2.17 x GFR/120) L/h, V 44.3 x WT/65 ",
    "L, WT ", sprintf("%.1f", wt), " kg; GFR ", sprintf("%.0f", gfr),
    " mL/min at 65 kg, ", creatinineNote, ". https://doi.org/10.1128/AAC.01885-13"
  )
  if (isTRUE(pregnant) && gfrPregnancy != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("GFR x ", PREGNANCY_GFR_FACTOR, " (physiological rise in filtration)")))
  }

  # Band: the trough range, not an MBC band (antibioticTroughBands())
  band <- antibioticTroughBands()
  band <- band[band$Drug == "linezolid", ]
  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = band$Typical,
    upperTypical = band$Upper,
    lowerTypical = band$Lower,
    reference = reference,
    transitChain = list(n = 4L, ktr = klag, kMetabolic = 0)
  )
}
