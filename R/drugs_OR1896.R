# -----------------------------------------------------------------------------
# OR-1896, levosimendan's long-acting active metabolite (Bertin 2026)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, rate constants per minute, amounts
# in mcg and concentrations in ng/mL (= mcg/L).
#
# Drafted by Claude Code, 2026-11-27, on Steven L. Shafer's decisions of
# 2026-11-26 ("Yes on OR-1896": Bertin's adult chain, Antila 2004 as its
# external check, OR-1855 not drawn) and 2026-11-27 (the transit-chain engine
# extended to carry it; card papers/digests/PMID_41288922.md).  Every number
# below was read on the PDF of the source and its supplement.
#
# THE SOURCE
# ==========
# Bertin S, Guidi M, Haefliger D, Thoueille P, Bardinet C, Decosterd LA,
# Perez MH, Giraud R, Assouline B, Schneider A, Buclin T, Livio F.
# Population pharmacokinetics of levosimendan and its metabolites OR-1855 and
# OR-1896 in critically ill adults, neonates and infants on veno-arterial
# ECMO.  Clin Pharmacokinet 2026;65:241-255 (doi 10.1007/s40262-025-01591-4;
# PMID 41288922).  21 patients (15 adults: 12 men, 62 years, 18-75; 79 kg,
# 52-125; Table 1), arterial plasma, UHPLC-MS/MS; NONMEM, amounts in umol.
# The full-precision estimates are the supplement's (PMID_41288922_ESM1, PDF
# p22), whose control stream (p20) writes the parent's elimination as
# K10 = CL/V1 - K13, so the transit outflow K13 is part of CL:
#
#   levosimendan -K13-> transit -ktr-> OR-1855 -kM2-> OR-1896
#                                        |  <-kM2-M1-   |
#                                       keM1           keM2
#
#   K13 = ktr = 0.0127 /h; keM1 0.01 /h (fixed) x (BW/70)^-0.611; kM2 0.0722 /h
#   in adults (x (1 - 0.732) in neonates and infants); keM2 0.01 /h (fixed);
#   kM2-M1 0.012 /h (fixed); V3 = V4 = V1 = 15.9 L x (BW/70)^0.574.
#
# HOW IT JOINS THE LIBRARY'S LEVOSIMENDAN
# =======================================
# The parent is Jonsson 2003 (R/drugs_levosimendan.R).  OR-1855 is formed
# from the share of the parent's elimination Bertin's model sends through the
# transit, K13 / (CL/V1) = 1.45% at 70 kg, evaluated at the patient's weight
# on Bertin's own size terms and applied to Jonsson's k10.  That share
# carries Bertin's fit into the join: concentrations identify only the ratio
# of a metabolite's formation to its clearance, not the share on its own, so
# the share is used together with Bertin's own metabolite parameters, which
# keeps that ratio as Bertin fitted it.  The
# transit-chain engine (R/advanceTransitChain.R) carries the whole system
# exactly: an intravenous parent with no absorption chain (n = 0), one
# formation transit, OR-1855 in the metabolite's central compartment, and
# OR-1896 in its "peripheral" one with an elimination of its own; OR-1896 is
# the curve drawn, and OR-1855 is not.  Amounts are molar in the source, so
# OR-1896 is drawn in its own mass, x 245.28 / 280.29 (molecular weights of
# OR-1896, C13H15N3O2, and levosimendan, C14H12N6O).
#
# THE CHECK (tests/testthat/test-drugs-OR1896.R)
# ==============================================
# Antila 2004 (Br J Clin Pharmacol 57:412-415, PMID 15025738; noncompartmental
# only, so a check and not a source), 24 patients with NYHA III-IV heart
# failure given 0.05 or 0.1 mcg/kg/min for 7 days, sampled 10-15 days after:
# OR-1896 Cmax 9.9 and 17.1 ng/mL, AUC 2370 and 3890 ng.h/mL, half-life 81 h,
# median tmax 180 and 170 h.  At 84 kg (the weight implied by its Table 1,
# CL in L/h over CL in L/h/kg) this model gives Cmax 8.2 and 16.4, AUC 2390
# and 4780, half-life 89 h, tmax 217 h: the peaks 17% and 4% low, the areas
# 1% and 23% high.  A linear model cannot match both areas, because the
# measured ones are not proportional to the dose (3890 at twice the rate of
# 2370, 18% short of double), and the peaks are (17.1 against 2 x 9.9), so
# the shortfall is in the study's tails, not in the model's structure; the
# test bounds the areas at 30%.  These are with the size switch on; with it
# off (the parent at total weight) Cmax 8.2 and 16.4, AUC 2330 and 4670,
# half-life 85 h, the numbers on the card.
# A second check, Kivikko 2002 (Int J Clin Pharmacol Ther 40:465-471, PMID
# 12395979, Table 2 read on the PDF image 2026-11-27; noncompartmental):
# 12 patients, 78 kg, 173 cm, 62 years, 0.2 mcg/kg/min for 24 h (the text
# layer's "mg/kg/min" is the Symbol font's mu, which the figure captions
# print).  Measured OR-1896 Cmax 5.5 +/- 2.2 ng/mL, Tmax 77 +/- 26 h (range
# 48-120), AUC 948 +/- 535, half-life 77 +/- 31 h; this model 5.2, 120 h,
# 1330 and 92 h, the parent at 24 h 68 against 62.6.  Both studies say the
# same: the peak is right, but OR-1896 rises and falls more slowly than
# measured (peak about 2 days late, areas high, half-life 10-20% long).  The
# fixed literature values Bertin used (keM1, keM2, kM2-M1, and the transit
# rate) are the likely place; they are kept as published, the bias stated
# rather than fitted away (a model is not refitted to its checks).  OR-1855 is about 3x low against the
# same study (Cmax 2.4 against 7.8), which is why it is not drawn; NAT2
# acetylator status, untyped in both, is the likely reason (Bertin's kM2
# varies 91% between patients).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight terms (keM1, and the metabolite volumes through V1)
# are kept as fitted, evaluated at the pharmacokinetic weight with the switch
# on and at total weight with it off.  The rate constants without a size term
# (K13, ktr, kM2, keM2, kM2-M1) take the fat-free-mass clearance factor over the
# volume factor, since a rate constant is a clearance over a volume.
# BODY SIZE ANCHOR: Bertin's Table 1 prints the adults' sex, age and weight
# but no height, BMI or BSA, so the rule's step 3 needs NCD-RisC's height for
# Switzerland, which this session could not reach; those rate constants are
# left at the reference anchor until it is supplied (papers/pending-work.csv).
# The cost, from cohortSizeFactors() for Bertin's adults (12 men and 3 women,
# 79 kg, 62 years) with men 172-178 cm and women 13.5 cm shorter: the
# anchored rate constants would be 0.6-1.4% lower than these.
#
# NOT MODELLED: the children's kM2 (x 0.268), Bertin's ECMO population
# itself, any effect site (the haemodynamic effect is not plotted), NAT2.
#
# PREGNANCY: none studied (inst/extdata/pregnancyRegistry.csv, levosimendan).
# -----------------------------------------------------------------------------

OR1896_K13       <- 0.0127   # /h, Bertin ESM p22 (K13 = ktr)
OR1896_KTR       <- 0.0127   # /h
OR1896_KEM1      <- 0.01     # /h, fixed, x (BW/70)^-0.611
OR1896_KEM1_EXP  <- -0.611
OR1896_KM2       <- 0.0722   # /h, adults
OR1896_KEM2      <- 0.01     # /h, fixed
OR1896_KM2M1     <- 0.012    # /h, fixed
OR1896_BERTIN_CL <- 13.9     # L/h at 70 kg, x (BW/70)^0.75
OR1896_BERTIN_V1 <- 15.9     # L at 70 kg, x (BW/70)^0.574
OR1896_V1_EXP    <- 0.574
OR1896_MW        <- 245.28   # g/mol, C13H15N3O2
LEVOSIMENDAN_MW  <- 280.29   # g/mol, C14H12N6O

#' Levosimendan's metabolite chain to OR-1896 (Bertin 2026)
#'
#' The one definition the parent's transit chain and the metabolite's own
#' model both read; see the header of \code{R/drugs_OR1896.R}.
#' @inheritParams cefazolin
#' @returns a list: `fm`, the share of the parent's elimination that forms
#'   OR-1855; `ktr`, `kel` (OR-1855), `k12` (to OR-1896), `k21` (back),
#'   `kelPeripheral` (OR-1896), all per minute; `v` and `vPeripheral` (L);
#'   and `mwRatio`
#' @keywords internal
levosimendanOR1896Chain <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  rate <- size$clearance / size$volume       # 1 with the switch off
  v <- OR1896_BERTIN_V1 * (w / 70)^OR1896_V1_EXP
  bertinK10 <- OR1896_BERTIN_CL * (w / 70)^0.75 / v
  list(
    fm  = OR1896_K13 * rate / bertinK10,
    ktr = OR1896_KTR * rate / 60,
    v   = v,
    kel = OR1896_KEM1 * (w / 70)^OR1896_KEM1_EXP / 60,
    k12 = OR1896_KM2 * rate / 60,
    k21 = OR1896_KM2M1 * rate / 60,
    kelPeripheral = OR1896_KEM2 * rate / 60,
    vPeripheral = v,
    mwRatio = OR1896_MW / LEVOSIMENDAN_MW
  )
}

#' OR-1896, levosimendan's active metabolite (Bertin 2026)
#'
#' Formed from levosimendan and not dosed directly; drawn by the parent's
#' transit chain.  Its own disposition, as one compartment losing drug by
#' elimination and by conversion back to OR-1855; see the header of
#' \code{R/drugs_OR1896.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
OR1896 <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  ch <- levosimendanOR1896Chain(weight, height, age, sex, adjustToFFM)
  default <- list(
    v1  = ch$vPeripheral,
    v2  = 1,
    v3  = 1,
    cl1 = (ch$kelPeripheral + ch$k21) * ch$vPeripheral,
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,          # the haemodynamic effect is not plotted
    MEAC = 0,
    typical = 0,        # no established therapeutic plasma range
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Bertin S, Guidi M, Haefliger D, et al. Population pharmacokinetics of ",
      "levosimendan and its metabolites OR-1855 and OR-1896 in critically ill ",
      "adults, neonates and infants on veno-arterial ECMO. Clin Pharmacokinet ",
      "2026;65:241-255 (supplement's full-precision estimates). THE ACTIVE ",
      "METABOLITE OF LEVOSIMENDAN, formed and not dosed: 1.45% of the parent's ",
      "elimination passes a transit (0.0127 /h) to OR-1855 (keM1 0.01 /h), ",
      "acetylated at 0.0722 /h to OR-1896 (keM2 0.01 /h, back to OR-1855 at ",
      "0.012 /h), both in 15.9 L x (BW/70)^0.574; adults. Checked against Antila ",
      "2004's 7-day and Kivikko 2002's 24-hour infusions (OR-1896 peaks within 17%; ",
      "the peak about 2 days late and the areas up to 40% high). ",
      "https://doi.org/10.1007/s40262-025-01591-4")
  )
}
