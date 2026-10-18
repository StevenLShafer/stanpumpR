# -----------------------------------------------------------------------------
# Warfarin: S- and R-enantiomer pharmacokinetics driving the INR through
# prothrombin complex activity (Xue 2017)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, enantiomer
# concentrations in mg/L, and the plotted curve is the INR (international
# normalized ratio).  Concentration.Units "INR" (R/inr.R).
#
# SOURCE
# ======
# Xue L, Holford N, Ding X-L, et al.  Theory-based pharmacokinetics and
# pharmacodynamics of S- and R-warfarin and effects on international
# normalized ratio: influence of body size, composition and genotype in
# cardiac surgery patients.  Br J Clin Pharmacol 2017;83:823-835, with the
# erratum (2017;83:1602).  264 Chinese cardiac-surgery patients (148 men, 116
# women; weight 40-86 kg, median age ~55 y; 73% mechanical and 22%
# bioprosthetic valves), 2858 S- and R-warfarin concentrations (UPLC-MS/MS)
# and 2163 INR values over a median 8.8 weeks; NONMEM.  Table 3 (PK) and
# Table 5 (PD and turnover), read from the full text on 2026-10-17.
#
# A racemic dose of D mg adds D/2 mg to each enantiomer's absorption depot
# (F = 1; the drug is given as equal amounts of S and R).  Each enantiomer is
# one-compartment with first-order absorption:
#
#     CL_S = 0.224 L/h   V_S = 24.0 L      (POP_CLS, POP_VS)
#     CL_R = 0.124 L/h   V_R = 16.2 L      (POP_CLR, POP_VR)
#     ka   = ln 2 / 0.385 h = 1.80 /h, fixed, both enantiomers (POP_TABS)
#
# The enantiomers are not added.  S-warfarin inhibits the synthesis of the
# prothrombin complex (activity PCA, a fraction of normal), R-warfarin
# competes with it, and the INR is 1 / PCA; see the turnover model in
# R/inr.R.  Table 5:
#
#     PCA0 = 0.979   C50_S = 0.378 mg/L   Hill = 2.720   (Emax fixed at 1)
#     T2PCA = 11.4 h (PCA turnover half-life)   RIC50 = 2.387 mg/L
#
# COVARIATES (Table 3 and Table 5)
# ================================
# Multiplicative, on the named parameter only, applied when the genotype or
# treatment is present:
#     amiodarone          x0.760 CL_S, x0.872 CL_R
#     fluconazole         x0.549 CL_S
#     CYP2C9 *1/*3        x0.830 CL_S, x1.230 CL_R  (the source estimated
#                           only *1/*3 against *1/*1; every other diplotype
#                           takes the *1/*1 reference, since *3/*3 and the *2
#                           diplotypes were absent or not extrapolated)
#     bioprosthetic valve x0.775 CL_S and CL_R
#     female              x1.33 V_S, x1.22 V_R
#     VKORC1 AA           x0.725 C50_S  (vs GA/GG)
#     CYP4F2 CC or CT     x0.770 C50_S  (vs TT)
# The Patient Profile has none of these inputs except sex, so in the app the
# patient is a CYP2C9 *1/*1, VKORC1 non-AA, CYP4F2 TT patient on neither
# interacting drug and with a mechanical valve, with the female volume terms
# applied by sex.  The other covariates are function arguments for scripts.
#
# THE R-WARFARIN ANTAGONISM, AND THE ERRATUM
# ==========================================
# R-warfarin raises the apparent C50 of S-warfarin:
#     C50_app = C50_S x (1 + C_R / RIC50)
# (R/inr.R).  This is the competitive antagonism of the paper's Equation 2 as
# the erratum corrects it; the erratum text could not be read here, but this
# form reproduces the paper's worked example (apparent C50 0.530 mg/L against
# 0.378, a 36% rise, at a maintenance dose giving C_R near 0.86 mg/L), which
# test-drugs-warfarin.R pins.  The authors found the R effect minor.
#
# BODY SIZE: FAT-FREE MASS, THE MODEL'S OWN
# =========================================
# CL scales with fat-free mass as FFM^0.75 and V as FFM^1 (the paper's
# normal-fat-mass model with the fat fraction fixed at zero, so NFM = FFM).
# This is the model's OWN covariate, so it is used in both positions of the
# adjustToFFM switch, as meloxicam's lean-body-mass volume term is.  The
# paper used the Janmahasatian fat-free mass, which this file computes
# directly (warfarinFFM()) rather than the library's Al-Sallami, so that the
# published numbers are reproduced; the reference is the Janmahasatian FFM of
# the paper's standard subject, a 70 kg, 176 cm man, which is 56.1 kg.  The
# paper's prose gives this reference as 56.6 kg but its Figure 3 caption and
# its own equation give 56.1 kg (the value a 70 kg, 176 cm man returns); 56.1
# is used, the self-consistent one.
#
# THRESHOLDS
# ==========
# No effect site, no MEAC, no band and no time until threshold: the plotted
# INR is a turnover endpoint, not an effect-site concentration, and it falls
# after warfarin is stopped at the pace of clotting-factor synthesis.  A
# simulated INR is not a dosing instruction or a procedure-safe time; see
# inst/help/drugs/warfarin.md.
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

# VKORC1 genotypes are the shared VKORC1_VALUES (R/constants.R), read from the
# Patient Profile; CYP4F2 is warfarin's own (no Patient Profile field yet).
WARFARIN_CYP4F2_VALUES <- c("TT", "CT", "CC")
# Janmahasatian fat-free mass of the paper's standard subject (70 kg, 176 cm
# male): 9270 x 70 / (6680 + 216 x 22.60) = 56.1 kg.
WARFARIN_FFM_STD <- 9270 * 70 / (6680 + 216 * 70 / 1.76^2)

#' Janmahasatian fat-free mass, the equation Xue 2017 used
#' @keywords internal
warfarinFFM <- function(weight, height, sex)
{
  bmi <- weight / (height / 100)^2
  if (sex == SEX_MALE) 9270 * weight / (6680 + 216 * bmi)
  else                 9270 * weight / (8780 + 244 * bmi)
}

#' Warfarin pharmacokinetics and the INR (Xue 2017)
#'
#' S- and R-warfarin disposition and the prothrombin-complex turnover that
#' gives the INR; see the header of \code{R/drugs_warfarin.R}.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string, "female" or "male"
#' @param adjustToFFM accepted for a common signature but not used: warfarin
#'   carries its own fat-free-mass covariate (clearance as FFM^0.75, volume as
#'   FFM), which the source fitted as the only body-size term, so it is applied
#'   in both positions of the switch, as meloxicam's lean-body-mass volume is
#' @param cyp2c9 CYP2C9 diplotype, one of \code{CYP2C9_VALUES}. Only
#'   \code{"*1/*3"} was estimated in the source; every other diplotype takes
#'   the \code{"*1/*1"} reference clearance (the app passes this field).
#' @param vkorc1 VKORC1 -1639 genotype, one of \code{"GG"}, \code{"GA"},
#'   \code{"AA"}; \code{"AA"} lowers the S-warfarin C50
#' @param cyp4f2 CYP4F2 genotype, one of \code{"TT"}, \code{"CT"}, \code{"CC"};
#'   \code{"CT"} or \code{"CC"} lowers the S-warfarin C50
#' @param amiodarone,fluconazole is the patient taking it? (enzyme inhibition)
#' @param bioprosthetic does the patient have a bioprosthetic valve?
#' @returns a list in the shape \code{getDrugPK()} expects, with R-warfarin as
#'   a parallel system and an \code{inr} block
#' @export
warfarin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     cyp2c9 = "*1/*1", vkorc1 = VKORC1_DEFAULT, cyp4f2 = "TT",
                     amiodarone = FALSE, fluconazole = FALSE,
                     bioprosthetic = FALSE)
{
  if (length(cyp2c9) != 1 || !cyp2c9 %in% CYP2C9_VALUES)
    stop("Invalid cyp2c9: ", paste(cyp2c9, collapse = ", "), ". Must be one of: ",
         paste(CYP2C9_VALUES, collapse = ", "))
  if (length(vkorc1) != 1 || !vkorc1 %in% VKORC1_VALUES)
    stop("Invalid vkorc1: ", paste(vkorc1, collapse = ", "), ". Must be one of: ",
         paste(VKORC1_VALUES, collapse = ", "))
  if (length(cyp4f2) != 1 || !cyp4f2 %in% WARFARIN_CYP4F2_VALUES)
    stop("Invalid cyp4f2: ", paste(cyp4f2, collapse = ", "), ". Must be one of: ",
         paste(WARFARIN_CYP4F2_VALUES, collapse = ", "))

  female <- sex == SEX_FEMALE
  sizeCL <- (warfarinFFM(weight, height, sex) / WARFARIN_FFM_STD)^0.75
  sizeV  <- warfarinFFM(weight, height, sex) / WARFARIN_FFM_STD

  valve <- if (bioprosthetic) 0.775 else 1
  clS <- 0.224 * sizeCL * valve *
    (if (amiodarone) 0.760 else 1) * (if (fluconazole) 0.549 else 1) *
    (if (cyp2c9 == "*1/*3") 0.830 else 1)
  clR <- 0.124 * sizeCL * valve *
    (if (amiodarone) 0.872 else 1) *
    (if (cyp2c9 == "*1/*3") 1.230 else 1)

  ka <- log(2) / 0.385 / 60                               # 1/min

  sSet <- list(
    v1 = 24.0 * sizeV * (if (female) 1.33 else 1), v2 = 1, v3 = 1,
    cl1 = clS / 60, cl2 = 0, cl3 = 0,
    ka_PO = ka, tlag_PO = 0, bioavailability_PO = 1
  )
  rSet <- list(
    v1 = 16.2 * sizeV * (if (female) 1.22 else 1), v2 = 1, v3 = 1,
    cl1 = clR / 60, cl2 = 0, cl3 = 0,
    ka_PO = ka, tlag_PO = 0, bioavailability_PO = 1
  )

  c50 <- 0.378 *
    (if (vkorc1 == "AA") 0.725 else 1) *
    (if (cyp4f2 %in% c("CC", "CT")) 0.770 else 1)

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(sSet))

  list(
    PK = PK,
    doseFraction = 0.5,                                   # S gets half the dose
    parallelSystems = list(
      list(name = "R-warfarin", doseFraction = 0.5,
           PK = list(default = rSet))
    ),
    inr = list(pca0 = 0.979, c50 = c50, hill = 2.720, ric50 = 2.387,
               halfLife = 11.4 * 60),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Xue L et al., Br J Clin Pharmacol 2017;83:823-835, with the erratum ",
      "(2017;83:1602) (S- and R-warfarin one-compartment PK and a prothrombin ",
      "complex turnover model giving the INR; 264 cardiac-surgery patients; ",
      "CYP2C9 *1/*1, VKORC1 non-AA, CYP4F2 TT, no interacting drug and a ",
      "mechanical valve unless set). https://doi.org/10.1111/bcp.13157"
    )
  )
}
