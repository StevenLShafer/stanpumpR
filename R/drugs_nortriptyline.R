# -----------------------------------------------------------------------------
# Nortriptyline (Vos 2025, Kvist 2001's model 2): one compartment, oral,
# well-stirred hepatic clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL.
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  The numbers below were read in the source's main text (PubMed
# Central, PMC12185566) and its Electronic Supplementary Material 1
# (equations 1-7 and Table A), and in the Pamelor label (NDA 018013, 2019)
# and Hart 2026 Tables 5 and 6, read in full.
#
# THE SOURCE
# ==========
# Vos CF, Coenen MJH, ter Hark SE, et al.  Optimizing nortriptyline dosing: a
# comparison between pharmacogenetics-based, phenotype-based, and standard
# dosing.  Clin Pharmacokinet 2025 (doi 10.1007/s40262-025-01528-x).  85
# adults (18-65 years) with major depression from the Dutch PITA trial,
# samples 8-24 h after the first dose and 10-16 h after the dose at steady
# state (none in the absorption phase), NONMEM.  "A linear two-compartment
# pharmacokinetic model ... based on model 2 described by Kvist et al." (a
# gut depot and one systemic compartment):
#
#     CL = Q CLint / (Q + CLint),  F = Q / (Q + CLint),  Q = 36 L/h (hepatic
#     plasma flow)
#     ka 0.533 /h (fixed, Kvist 2001)   CLint 42.3 L/h   V 1080 L
#
# so CL 19.45 L/h, F 0.460, CL/F 42.3 L/h, half-life 38.5 h.  No covariate
# was retained; there is no CYP2D6 term, although nortriptyline is cleared by
# CYP2D6 (the source's subset: 5 poor, 20 intermediate, 21 normal and 2
# ultrarapid metabolisers), so the typical patient is a mixed population.
# The Patient Profile's CYP 2D6 field is not read: Kvist 2001, which had a
# CYP2D6 term, was not reachable.
#
# CHECKED (not used in the fit)
# =============================
# Hart 2026 (AGNP update, Table 6): concentration per daily dose 24 h after a
# once-daily dose, 0.804 (0.578-1.319) ng/mL per mg; this model, 0.815.
# Jensen 2012 (Br J Clin Pharmacol 73:619, read), 25 mg in 17 healthy
# Caucasian normal metabolisers: peak 9.4-9.5 ng/mL (this model 9.45), but
# AUC 350-364 ng.h/mL and half-life 24 h against this model's 591 and 38.5
# h, so a healthy normal metaboliser clears nortriptyline faster than this
# typical depressed patient.
#
# DOSE BASIS
# ==========
# Pamelor: "nortriptyline HCl ... equivalent to 10 mg, 25 mg, 50 mg, and 75 mg
# base", so US strengths are base and a dose is entered as prescribed.  The
# source does not state its basis; Hart's concentration per dose, in base,
# agrees with it to 1.4%.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No weight term (Jerling 1994 also found none), so the values are written
# for the library's reference man and scaled by fat-free mass with the switch
# on, the clearances (CLint and the hepatic flow, so F is unchanged) to the
# 0.75 power and V in proportion; fixed with the switch off.
#
# THE BAND
# ========
# Hart 2026 (AGNP consensus update 2026, Table 5, read in full):
# nortriptyline 70-170 ng/mL for depression, 12 h after the dose; laboratory
# alert level 300 ng/mL; level of recommendation 1.
#
# PREGNANCY
# =========
# Not applied (unquantified): Wisner 1993, quoted by Yue 2023, found that six
# women needed about 1.6 times their prepregnancy dose in the third
# trimester, a secondary quotation mixing medians and means.
# -----------------------------------------------------------------------------

# Vos 2025, ESM 1 (equations 1-7, Table A)
NORTRIPTYLINE_Q     <- 36       # L/h, hepatic plasma flow
NORTRIPTYLINE_CLINT <- 42.3     # L/h
NORTRIPTYLINE_V     <- 1080     # L
NORTRIPTYLINE_KA    <- 0.533    # 1/h, fixed (Kvist 2001)

#' Nortriptyline (Vos 2025)
#'
#' One compartment, oral, well-stirred hepatic clearance; see the header of
#' \code{R/drugs_nortriptyline.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
nortriptyline <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  q     <- NORTRIPTYLINE_Q * size$clearance
  clint <- NORTRIPTYLINE_CLINT * size$clearance

  default <- list(
    v1 = NORTRIPTYLINE_V * size$volume, v2 = 1, v3 = 1,
    cl1 = q * clint / (q + clint) / 60, cl2 = 0, cl3 = 0,
    ka_PO = NORTRIPTYLINE_KA / 60,
    bioavailability_PO = q / (q + clint),
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Vos CF et al., Clin Pharmacokinet 2025 (Kvist 2001's model 2): one ",
    "compartment, well-stirred liver, Q 36 L/h, CLint 42.3 L/h (CL 19.5 L/h, ",
    "F 0.46), V 1080 L, ka 0.533 /h; CYP2D6 mixed, not applied. ",
    "https://doi.org/10.1007/s40262-025-01528-x"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 120,
    upperTypical = 170,
    lowerTypical = 70,
    reference = reference
  )
}
