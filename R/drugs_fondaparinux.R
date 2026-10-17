# -----------------------------------------------------------------------------
# Fondaparinux: subcutaneous two-compartment model after orthopaedic surgery
# (Delavenne 2010)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), plasma fondaparinux.  Doses are entered in
# mg subcutaneously; the model is apparent (CL/F, V/F).
#
# SOURCE
# ======
# Delavenne X, Zufferey P, Baylot D, et al. (GETHCAM / POP-A-RIX).  Population
# pharmacokinetics of fondaparinux administered at prophylactic doses after
# major orthopaedic surgery in everyday practice.  Thromb Haemost
# 2010;104:252-260.  Patients on 2.5 mg once daily after hip replacement, hip
# fracture repair or knee replacement.  NONMEM; a two-compartment model with
# first-order absorption.  Table 2 and Eqs 4-5, final model, read from the full
# text on 2026-10-17:
#
#     Ka = 0.716 /h
#     CL/F (L/h) = 0.278 x (WT/68)^0.534 x (CLcr/87)^0.532 x (Age/74)^-0.461
#     V2/F (central, L) = 8.14 x (WT/68)^0.522
#     Q/F = 0.082 L/h (fixed)     V3/F (peripheral, L) = 15.6
#
# with the paper's V2/F the central volume and V3/F the peripheral, so here
# v1 = V2/F and v2 = V3/F.  CLcr is the simplified MDRD estimate (egfrMDRD(),
# R/renalFunction.R), which the paper found more accurate than Cockcroft-Gault,
# centred on 87 mL/min; weight on 68 kg and age on 74 y.  The terminal
# half-life is about 17-21 h.
#
# WHAT THIS REPRESENTS
# ====================
# Patients after major orthopaedic surgery on the 2.5 mg prophylactic dose.
# Clearance falls with age and with renal impairment (the paper: about 15%
# lower in moderate and 43% in severe renal impairment).  Extrapolation to the
# treatment dose or to a non-surgical population is outside the data.
# Bioavailability is 1: the apparent parameters already carry the subcutaneous
# absorbed fraction (fondaparinux is essentially completely absorbed).
#
# CREATININE CLEARANCE
# ====================
# Simplified MDRD (egfrMDRD()) in mL/min/1.73 m^2, from the Patient Profile's
# serum creatinine on the adult scale (adultEquivalentCreatinine()); with the
# field blank it is the ASSUMED NORMAL creatinine for age and sex, so renal
# impairment is then not represented.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance and the central volume carry the model's own weight covariate,
# evaluated at the pharmacokinetic weight with the switch on (size$pkWeight) and
# at total body weight with it off, as for cefazolin.  Q and the peripheral
# volume have no size term in the source and take the library's fat-free-mass
# factors with the switch on; with it off they are as published
# (legacyVolume = 1).
#
# EFFECT SITE AND THRESHOLDS
# ==========================
# None: tPeak = 0 and the plotted concentration is plasma.  No band, no MEAC
# and no time until threshold: a simulated concentration is not an anti-Xa
# activity, a bleeding or clotting risk, or a procedure-safe time.
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

FONDAPARINUX_WT_REF   <- 68   # kg, reference weight
FONDAPARINUX_CLCR_REF <- 87   # mL/min, reference MDRD creatinine clearance
FONDAPARINUX_AGE_REF  <- 74   # y, reference age

#' Fondaparinux pharmacokinetics (subcutaneous, orthopaedic prophylaxis)
#'
#' Delavenne 2010; see the header of \code{R/drugs_fondaparinux.R}.
#'
#' @inheritParams apixaban
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
fondaparinux <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl <- egfrMDRD(age, sex, adultEquivalentCreatinine(creatinine, age, sex))

  cl <- 0.278 * (pkW / FONDAPARINUX_WT_REF)^0.534 *
    (crcl / FONDAPARINUX_CLCR_REF)^0.532 *
    (age / FONDAPARINUX_AGE_REF)^-0.461                        # L/h

  default <- list(
    v1  = 8.14 * (pkW / FONDAPARINUX_WT_REF)^0.522,            # central (V2/F)
    v2  = 15.6 * size$volume,                                  # peripheral (V3/F)
    v3  = 1,                                                   # two compartments
    cl1 = cl / 60,
    cl2 = 0.082 / 60 * size$clearance,                         # Q/F
    cl3 = 0,
    ka_SC = 0.716 / 60,                                        # 1/min
    tlag_SC = 0,
    bioavailability_SC = 1                                     # apparent scale
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
      "Delavenne X et al., Thromb Haemost 2010;104:252-260 (subcutaneous ",
      "two-compartment model after major orthopaedic surgery; clearance on ",
      "weight, simplified-MDRD creatinine clearance and age, central volume on ",
      "weight; apparent parameters, from the entered creatinine or an assumed ",
      "normal value for age and sex). https://doi.org/10.1160/TH10-02-0127"
    )
  )
}
