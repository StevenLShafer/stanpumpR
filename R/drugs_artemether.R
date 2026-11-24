# -----------------------------------------------------------------------------
# Artemether, plotted with its active metabolite dihydroartemisinin: oral, a
# chain of transit compartments, two compartments for artemether and one for
# dihydroartemisinin, and a clearance that rises with each dose of the course
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentrations are ng/mL (= mcg/L) of artemether and of
# dihydroartemisinin in plasma.
#
# Drafted by Claude Code, 2026-11-24, at the request of Steven L. Shafer, as
# one of the two components of Coartem (R/combination-products.R): "For
# Coartem, both components are active. Both should appear." and "Yes, draw
# both. Build it with Coartem." (2026-11-24).  Artemether is given in the
# United States only in Coartem, so this model has no dose units of its own
# and no Category: a Coartem dose reaches it as mg of artemether, 20 mg a
# tablet (docs/library-expansion/combination-only.csv).  Every number below
# was read on the PDF of the source.
#
# SOURCE: DING 2026, TABLE 2
# ==========================
# Ding J, Hoglund RM, van der Pluijm RW, Callery JJ, Peto TJ, Tripura R, et
# al.  Population pharmacokinetics of artemether-lumefantrine plus
# amodiaquine in patients with uncomplicated Plasmodium falciparum malaria.
# Br J Clin Pharmacol 2026;92(2):589-605.  doi 10.1002/bcp.70301 (PMID
# 41052792, PMC12850606).  626 artemether and dihydroartemisinin samples
# (LC-MS/MS) from the 79 patients of the TRACII (Bangladesh) and TACT-CV
# (Vietnam) trials who had dense sampling (1, 2, 4, 6, 8, 12, 24 and 64 h,
# and 52 h in TRACII), children under 20 kg excluded; NONMEM 7.5, FOCE-I,
# the samples below the limit discarded (M1).  The two arms (with and
# without amodiaquine) did not differ, so the model is the same for both.
#
# Table 2, for a "typical" adult patient weighing 45 kg:
#
#   artemether          MTT 1.55 h, two transit compartments, F 1 (fixed)
#                       CL/F 79.3 L/h, VC/F 141 L, Q/F 21.7 L/h, VP/F 283 L
#                       time dependency on CL 0.551, as CL x (1 + 0.551 (OCC - 1)),
#                       OCC the dose occasion from 1 to 6
#   dihydroartemisinin  CL/F 255 L/h, VC/F 64.1 L, one compartment
#
# Weight acts on every clearance and volume with exponents FIXED at 0.75 and
# 1 (Methods 2.7), normalised to the 45 kg of the table's footnote.  No other
# covariate was retained.  "Parent drugs were assumed to be completely
# metabolized to their metabolites" (Methods 2.6), so the whole of
# artemether's elimination forms dihydroartemisinin (kMetabolic = k10).
#
# THE TRANSIT CHAIN
# =================
# "two transit compartments, with an identical rate constant between
# compartments" (Results 3.1.1).  The Mahidol-Oxford group writes its transit
# models with the dose in a depot that empties through the chain into the
# central compartment, every step at KTR = (N + 1) / MTT, the absorption rate
# being the same constant (Savic 2007; R/drugs_enalapril.R's
# transitRateConstant()).  So the engine's chain is three compartments, each
# emptying at 3 / 1.55 h = 1.94 /h.  The paper prints no equation, and the
# other reading, two compartments at 2 / 1.55 h, has the same mean absorption
# time; the three-compartment reading gives the typical first-dose peaks
# 240 ng/mL (artemether) and 128 ng/mL (dihydroartemisinin) at 2.0 h, against
# Table 2's medians of 256 and 135 ng/mL and the label's "about 2 hours",
# where the two-compartment reading gives 220 and 116 (measured in this
# session's own simulation, 2026-11-24).
#
# THE CLEARANCE THAT RISES WITH EACH DOSE
# =======================================
# The label reads the fall in artemether's exposure over the course as enzyme
# induction (artemether/DHA AUC ratio 1.2 after one dose, 0.3 after six).
# Ding models it as a step in clearance at each dose, which the transit-chain
# engine now carries exactly (doseOccasionClearance, R/advanceTransitChain.R):
# by the sixth dose the clearance is 3.76 times the first.  Ding: "this
# empirical time-dependent clearance model is not suitable for extrapolation
# to treatment durations beyond the standard 3 days", so the engine holds the
# sixth occasion's clearance for any later dose rather than raising it
# further.  A second course weeks later starts at that clearance, not the
# first's; nothing in the source says how fast the induction wears off.
#
# DIHYDROARTEMISININ: MASS, NOT MOLES
# ===================================
# Ding does not say whether the conversion was written in mass or in moles
# (artemether 298.4 g/mol, dihydroartemisinin 284.4 g/mol).  With complete
# conversion the metabolite's area is the dose over its own clearance: 480 mg
# over 255 L/h is 1,882 h.ng/mL in mass, 1,794 in moles, against Table 2's
# median of 1,870 for the 45 kg patient.  Mass it is, as the engine carries
# it (no molecular-weight ratio; R/advanceTransitChain.R explains why none is
# applied on top of a fitted volume).
#
# ARTEMETHER'S TERMINAL HALF-LIFE  [decided by Steven L. Shafer, 2026-11-23]
# ==============================
# Ding's two compartments give a terminal half-life of 12.1 h (Table 2).
# Hoglund 2015 (PMID 25297720), from the same group, rejected its own
# two-compartment fit with a 12.2 h half-life as unstable and kept one
# compartment; the label says about 2 h.  Shafer chose Ding: "Typically later
# studies have more sensitive assays. More sensitive assays give longer
# half-lives."  The 2 h phase still carries nearly all of each dose; the 12 h
# phase sits at a few ng/mL between doses.
#
# NOT APPLIED
# ===========
# * Food.  Every patient took Coartem with food or milk, as the label
#   directs; fasting lowers artemether's bioavailability 2- to 3-fold (label
#   12.3).  The curve is the fed patient's.
# * Interactions.  CYP3A4 inducers and inhibitors (rifampin, efavirenz,
#   lopinavir/ritonavir, ketoconazole) change artemether's exposure severalfold
#   (label 7); none is modelled.
# * Pregnancy.  Ding studied no pregnant women; see the pregnancy registry.
#
# CHECKS (tests/testthat/test-drugs-artemether.R)
# ===============================================
# * Table 2's typical values, both switch positions.
# * The typical 45 kg patient on the label's six doses of 80 mg: artemether
#   AUC 2,990 and dihydroartemisinin AUC 1,878 h.ng/mL, against Table 2's
#   medians of 2,850 and 1,870 (post hoc, with variability), and the peaks
#   above.
# * The engine against an independent Runge-Kutta solution of the same
#   equations, dose occasions included.
# -----------------------------------------------------------------------------

ARTEMETHER_MTT       <- 1.55    # h, mean transit time
ARTEMETHER_TRANSITS  <- 2L      # transit compartments, besides the dose depot
ARTEMETHER_CL        <- 79.3    # L/h, CL/F at 45 kg, first dose occasion
ARTEMETHER_VC        <- 141     # L, VC/F at 45 kg
ARTEMETHER_Q         <- 21.7    # L/h, Q/F at 45 kg
ARTEMETHER_VP        <- 283     # L, VP/F at 45 kg
ARTEMETHER_OCC_SLOPE <- 0.551   # CL x (1 + 0.551 (OCC - 1))
ARTEMETHER_OCCASIONS <- 6L      # the standard course; held there after
ARTEMETHER_WT_REF    <- 45      # kg, Table 2's typical patient
ARTEMETHER_CL_EXP    <- 0.75    # fixed
ARTEMETHER_V_EXP     <- 1       # fixed

#' Ding 2026's weight factors on clearance and volume
#'
#' Shared by artemether and dihydroartemisinin, whose parameters are both
#' normalised to the 45 kg patient of Table 2 with fixed exponents.
#'
#' @inheritParams cefazolin
#' @returns a list of `clearance` and `volume` multipliers
#' @keywords internal
dingWeightFactors <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  list(clearance = (w / ARTEMETHER_WT_REF)^ARTEMETHER_CL_EXP,
       volume    = (w / ARTEMETHER_WT_REF)^ARTEMETHER_V_EXP)
}


#' Artemether pharmacokinetics, with its metabolite dihydroartemisinin
#'
#' A component of Coartem: see R/combination-products.R.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
artemether <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  f <- dingWeightFactors(weight, height, age, sex, adjustToFFM)
  ktr <- transitRateConstant(ARTEMETHER_MTT, ARTEMETHER_TRANSITS)   # /h

  v1 <- ARTEMETHER_VC * f$volume
  default <- list(
    v1  = v1,
    v2  = ARTEMETHER_VP * f$volume,
    v3  = 1,
    # The first dose occasion's clearance; the engine raises it at each dose.
    cl1 = ARTEMETHER_CL * f$clearance / 60,
    cl2 = ARTEMETHER_Q * f$clearance / 60,
    cl3 = 0,
    # The last compartment of the chain empties at the same rate as the
    # others (see the header).
    ka_PO = ktr / 60,
    bioavailability_PO = 1,      # apparent parameters, F fixed at 1
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  dha <- dihydroartemisininSet(weight, height, age, sex, adjustToFFM)

  list(
    PK = PK,
    tPeak = 0,          # no concentration-effect relation is plotted
    MEAC = 0,
    typical = 0,        # no established therapeutic plasma range
    upperTypical = 0,
    lowerTypical = 0,
    transitChain = list(
      # The dose depot and the two transit compartments, every one at ktr.
      n   = ARTEMETHER_TRANSITS + 1L,
      ktr = ktr / 60,
      # The whole elimination forms dihydroartemisinin (Methods 2.6).
      kMetabolic = default$cl1 / default$v1,
      doseOccasionClearance = list(slope = ARTEMETHER_OCC_SLOPE,
                                   occasions = ARTEMETHER_OCCASIONS),
      metabolite = list(
        name = "dihydroartemisinin",
        n    = 0L,                 # formed straight into its central volume
        v    = dha$v1,
        kel  = dha$cl1 / dha$v1
      )
    ),
    reference = paste0(
      "Ding J, Hoglund RM, van der Pluijm RW, et al. Population pharmacokinetics ",
      "of artemether-lumefantrine plus amodiaquine in patients with uncomplicated ",
      "Plasmodium falciparum malaria. Br J Clin Pharmacol 2026;92:589-605, Table 2 ",
      "(79 patients with dense sampling, Bangladesh and Vietnam). A COMPONENT OF ",
      "COARTEM, plotted with its active metabolite dihydroartemisinin. Two transit ",
      "compartments behind the dose depot (MTT 1.55 h, each step 3/MTT), then two ",
      "compartments: CL/F 79.3 L/h and Q/F 21.7 L/h x (WT/45)^0.75, VC/F 141 L and ",
      "VP/F 283 L x WT/45; the clearance rises with each dose as CL x (1 + 0.551 ",
      "(OCC - 1)), held at the sixth dose's after it. All of the elimination forms ",
      "dihydroartemisinin (one compartment, CL/F 255 L/h, V/F 64.1 L, same weight ",
      "scaling). Fed patients; no interaction is modelled. ",
      "https://doi.org/10.1002/bcp.70301")
  )
}
