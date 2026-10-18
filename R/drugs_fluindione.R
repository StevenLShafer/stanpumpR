# -----------------------------------------------------------------------------
# Fluindione: apparent two-compartment PK driving the INR through an
# indirect-response model (Verstuyft 2012)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, fluindione
# concentration in mg/L, and the plotted curve is the INR.  Concentration.Units
# "INR" (R/inr.R).
#
# SOURCE
# ======
# Verstuyft C, Delavenne X, Rousseau A, Robert A, Tod M, Diquet B, Lebot M,
# Jaillon P, Becquemont L.  A pharmacokinetic-pharmacodynamic model for
# predicting the impact of CYP2C9 and VKORC1 polymorphisms on fluindione and
# acenocoumarol during induction therapy.  Clin Pharmacokinet 2012;51(1):41-53.
# 24 White healthy subjects, open-label two-period crossover, a single 20 mg
# dose of fluindione; Monolix.  Tables II (PK/PD parameters) and IV (steady-
# state INRmax by covariate), and Equation 1 (the indirect response model), read
# from the full text on 2026-10-18.
#
# Fluindione is an indane-dione vitamin-K antagonist, the most prescribed oral
# anticoagulant in France.  A two-compartment, first-order oral input model with
# no lag time describes the (apparent, /F) disposition; the anticoagulant effect
# is an indirect-response model (R/inr.R, type "indirect") in which fluindione
# inhibits the production of 1/INR:
#
#     E      = C^gamma / (C^gamma + C50^gamma)      (maximal effect fixed at 1)
#     d(1/INR)/dt = kin (1 - E) - kout (1/INR),     1/INR_0 = kin / kout
#
# PARAMETERS (Table II, typical 69 kg subject, CYP1A2 at the median)
# ==================================================================
#     ka  = 1.83 /h
#     CL  = 0.17 L/h  x (WT/69)^0.75  x (PAX_CAF/0.42)^0.617  x genotype
#     V1  = 5.19 L    x (WT/69)
#     Q   = 0.173 L/h     V2 = 2.77 L
#     C50 = 3.63 mg/L x VKORC1     kin = 0.0385 /h   kout = 0.0391 /h   gamma = 2.32
#
# COVARIATES
# ==========
# These were fitted in Monolix as a multiplicative effect, parameter x (1 + beta)
# per category (the table writes the beta with its sign; the signed value below
# is what reproduces the paper's Table IV INRmax and the expected direction):
#   * CYP2C9: a *3 allele carrier (any of *1/*3, *2/*3, *3/*3) has CL x (1 - 0.363)
#     = x 0.637.  Only *3 was a predictor; *1/*2 and *2/*2 behave as the wild type
#     here, so they are treated as *1/*1.  (beta_CL = -0.363, Table II theta5.)
#   * VKORC1: a carrier of the less-sensitive allele has C50 x (1 + 0.3) = x 1.30,
#     so the sensitive genotype has the lower C50 and the higher INR, as Table IV
#     shows (TT INRmax 5.4 vs CT/CC 3.7 at *1/*3).  (beta_C50 = +0.3, theta11.)
#   * CYP1A2 (PAX/CAF ratio) and body weight act on CL; CYP1A2 and smoking also
#     shift the response.  There is no CYP1A2 / smoking field in the Patient
#     Profile, so the ratio is fixed at the population median 0.42, i.e. the term
#     drops out; the app therefore does not show the smoking effect on fluindione.
#     Body weight is the model's OWN covariate, used in both positions of the
#     adjustToFFM switch, as warfarin's fat-free mass is.
#
# VKORC1 GENOTYPE: 1173C>T vs the -1639G>A FIELD
# ==============================================
# The source genotyped VKORC1 at 1173C>T (rs9934438), CC/CT/TT; the Patient
# Profile field is the -1639G>A SNP (rs9923231), GG/GA/AA.  The two are in strong
# linkage disequilibrium and tag the same haplotype groups: -1639G = 1173C (group
# B, wild type, less sensitive) and -1639A = 1173T (group A, lower VKORC1
# expression, more sensitive, lower dose).  So AA maps to the source's TT (the
# sensitive, base C50) and GG or GA to its "CC or CT" (the less-sensitive,
# x(1 + 0.3) C50).  The same field drives the warfarin models (R/drugs_warfarin.R).
#
# THRESHOLDS
# ==========
# No effect site, no MEAC, no band and no time until threshold: the plotted INR
# is a turnover endpoint.  This is a research and teaching model; a simulated INR
# is not a dose recommendation, a bleeding risk or a procedure-safe time; see
# inst/help/drugs/fluindione.md.
#
# (Claude Code, 2026-10-18, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' Fluindione pharmacokinetics and the INR (Verstuyft 2012)
#'
#' Apparent two-compartment disposition and the indirect-response model that
#' gives the INR; see the header of \code{R/drugs_fluindione.R}.
#'
#' @param weight weight in kg
#' @param height height in cm (not used; accepted for the common signature)
#' @param age age in years (not used; the model has no age covariate)
#' @param sex sex as a string (not used)
#' @param adjustToFFM accepted for a common signature but not used: fluindione
#'   carries its own body-weight covariate, applied in both switch positions
#' @param cyp2c9 CYP2C9 diplotype, one of \code{CYP2C9_VALUES}. A \code{*3}
#'   carrier has reduced clearance; every other diplotype takes the wild-type
#'   clearance (the app passes this field).
#' @param vkorc1 VKORC1 -1639G>A genotype, one of \code{VKORC1_VALUES};
#'   \code{"GG"} or \code{"GA"} raises the C50 (less sensitive), \code{"AA"} is
#'   the sensitive reference (the app passes this field)
#' @returns a list in the shape \code{getDrugPK()} expects, with an indirect
#'   \code{inr} block
#' @export
fluindione <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       cyp2c9 = CYP2C9_DEFAULT, vkorc1 = VKORC1_DEFAULT)
{
  if (length(cyp2c9) != 1 || !cyp2c9 %in% CYP2C9_VALUES)
    stop("Invalid cyp2c9: ", paste(cyp2c9, collapse = ", "), ". Must be one of: ",
         paste(CYP2C9_VALUES, collapse = ", "))
  if (length(vkorc1) != 1 || !vkorc1 %in% VKORC1_VALUES)
    stop("Invalid vkorc1: ", paste(vkorc1, collapse = ", "), ". Must be one of: ",
         paste(VKORC1_VALUES, collapse = ", "))

  cyp2c9Factor <- if (grepl("\\*3", cyp2c9)) 1 - 0.363 else 1   # beta_CL = -0.363
  CL <- 0.17 * (weight / 69)^0.75 * cyp2c9Factor                # L/h (PAX/CAF median)
  V1 <- 5.19 * (weight / 69)                                    # L
  Q  <- 0.173                                                   # L/h
  V2 <- 2.77                                                    # L
  ka <- 1.83 / 60                                               # 1/min

  sSet <- list(
    v1 = V1, v2 = V2, v3 = 1,
    cl1 = CL / 60, cl2 = Q / 60, cl3 = 0,
    ka_PO = ka, tlag_PO = 0, bioavailability_PO = 1
  )

  c50 <- 3.63 * (if (vkorc1 %in% c("GG", "GA")) 1 + 0.3 else 1)  # beta_C50 = +0.3

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(sSet))

  list(
    PK = PK,
    inr = list(type = "indirect", c50 = c50, gamma = 2.32,
               kin = 0.0385 / 60, kout = 0.0391 / 60),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Verstuyft C et al., Clin Pharmacokinet 2012;51(1):41-53 (apparent ",
      "two-compartment PK and an indirect-response INR model with CYP2C9 and ",
      "VKORC1; 24 healthy subjects; CYP1A2 at the median and the wild-type ",
      "CYP2C9 unless *3 is set). https://doi.org/10.2165/11595560-000000000-00000"
    )
  )
}
