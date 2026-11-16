# -----------------------------------------------------------------------------
# Cladribine (Lindemalm 2005): plasma cladribine after intravenous,
# subcutaneous and oral doses in adults with lymphoid malignancies
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-05, at the request of Steven L. Shafer, as
# the first of the chemotherapy drugs (docs/chemotherapy.md,
# inst/extdata/chemotherapyRegistry.csv, source N04).  Every number below was
# read from the full text in PubMed Central (PMC1079880): Table 3 and the
# Results; Shafer supplied the PDF (2026-11-06), whose Table 3 confirms every
# value (ka in /h) and whose Results confirm F fixed at 1 subcutaneously and
# no oral lag.
#
# THE SOURCE
# ==========
# Lindemalm S, Savic RM, Karlsson MO, Juliusson G, Liliemark J, Friberg LE.
# Application of population pharmacokinetics to cladribine.  BMC Pharmacol
# 2005;5:4 (doi 10.1186/1471-2210-5-4).  Pooled data from five Swedish
# centres and one in the UK, 1990-1996: 1102 plasma concentrations from 161
# patients (168 courses; 129 men and 34 women of the 163 first included; mean
# age 60, range 22-89 years; mean weight 76 kg, 48-118; mean height 174 cm;
# BSA 1.9 m2), mostly hairy cell (84) and chronic lymphocytic (63) leukaemia.
# Doses: a 2-hour intravenous infusion of 5 mg/m2 or 0.12 mg/kg, continuous
# infusions of 4.0-5.7 mg/m2 per 24 h in nine, an oral solution of 10 mg/m2
# or 0.24 mg/kg in saline after an overnight fast, and 5 mg/m2 injected under
# the skin of the abdomen.  Venous plasma, reversed-phase HPLC, limit of
# quantitation 1 nM.  NONMEM FOCE-I, all three routes fitted together.
#
# THE MODEL (Table 3)
# ===================
# Three compartments, first-order absorption with no lag by both
# extravascular routes:
#
#     CL 39.3 L/h   V1 71.7 L   Q2 51.1 L/h   V2 475 L   Q3 105 L/h   V3 73.6 L
#     oral ka 1.31 /h, F 0.353;  subcutaneous ka 2.48 /h, F fixed at 1
#
# which gives half-lives of 0.19, 1.28 and 16.4 h against the text's 0.2,
# 1.3 and 16 h (the test checks them).  No covariate was retained: age,
# weight, height, sex, body surface area, diagnosis, creatinine clearance,
# liver tests and lymphocyte count were all tested and none was significant.
#
# WHAT IT DOES NOT COVER
# ======================
# * Renal impairment.  Cladribine is substantially excreted unchanged (about
#   half its clearance is renal in Kearns 1994, registry source N05), yet no
#   renal term was found here because no patient had severe impairment
#   (creatinine clearance mean 81 mL/min, range 5-162, with five outliers).
#   The curve is therefore that of a patient with near-normal renal function,
#   whatever creatinine is entered.
# * Children.  Fitted in adults 22-89 years; Kearns 1994 is the paediatric
#   source still to be read.  A child gets the adult model scaled to fat-free
#   mass (below), which is an extrapolation.
# * The tablet.  The oral data are an oral SOLUTION after an overnight fast;
#   the multiple-sclerosis tablet (Mavenclad) is a different product, modelled
#   by Savic 2017 (registry source C37b), and is not this route.
# * The active species.  Cladribine acts as its intracellular triphosphate,
#   which is not modelled; nor is 2-chloroadenine, its metabolite.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate was significant, and the authors conclude that a fixed
# dose does as well as dosing by weight or body surface area.  The library's
# rule still applies: the population values are taken as the 70-kg
# reference man's (the cohort's mean was 76 kg, 79% men) and scaled to
# fat-free mass with the switch on; with it off they are fixed, as published
# (legacyVolume = legacyClearance = 1).
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No human pharmacokinetics of cladribine in pregnancy were found (PubMed,
# 2026-11-05); the only pregnancy studies are of its teratogenicity in mice
# and rats.  The registry records it as unknown, and nothing is applied.
#
# NO EFFECT SITE, NO BAND
# =======================
# No plasma concentration-effect relation or therapeutic range is
# established.  Plasma only: tPeak, MEAC and the band are zero.
# -----------------------------------------------------------------------------

# Lindemalm 2005, Table 3: clearances in L/h, volumes in L, ka in /h
CLADRIBINE_LINDEMALM <- c(
  CL = 39.3, V1 = 71.7, Q2 = 51.1, V2 = 475, Q3 = 105, V3 = 73.6,
  kaPO = 1.31, FPO = 0.353, kaSC = 2.48, FSC = 1
)

#' Cladribine (Lindemalm 2005)
#'
#' Three compartments in adults with lymphoid malignancies, intravenous,
#' subcutaneous and oral; see the header of \code{R/drugs_cladribine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cladribine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CLADRIBINE_LINDEMALM
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)

  default <- list(
    v1  = p[["V1"]] * size$volume,
    v2  = p[["V2"]] * size$volume,
    v3  = p[["V3"]] * size$volume,
    cl1 = p[["CL"]] / 60 * size$clearance,
    cl2 = p[["Q2"]] / 60 * size$clearance,
    cl3 = p[["Q3"]] / 60 * size$clearance,
    ka_PO = p[["kaPO"]] / 60,
    bioavailability_PO = p[["FPO"]],
    tlag_PO = 0,
    ka_SC = p[["kaSC"]] / 60,
    bioavailability_SC = p[["FSC"]],
    tlag_SC = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Lindemalm S, Savic RM, Karlsson MO, Juliusson G, Liliemark J, Friberg LE. ",
    "Application of population pharmacokinetics to cladribine. BMC Pharmacol ",
    "2005;5:4. Three compartments in 161 adults with lymphoid malignancies: CL ",
    "39.3 L/h, V1 71.7 L, Q2 51.1 L/h, V2 475 L, Q3 105 L/h, V3 73.6 L; oral ",
    "solution ka 1.31/h, F 0.353; subcutaneous ka 2.48/h, F 1. No covariates; ",
    "no patient with severe renal impairment. Plasma cladribine only. ",
    "https://doi.org/10.1186/1471-2210-5-4"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
