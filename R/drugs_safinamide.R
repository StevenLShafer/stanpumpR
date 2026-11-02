# -----------------------------------------------------------------------------
# Safinamide (Xadago): one compartment, apparent oral, with allometric weight
# and a power of age on clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer
# (inst/help/models/parkinsons.md, inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# FDA Clinical Pharmacology Review of NDA 207145 (Xadago), Pharmacometrics
# Review, Table 1 (the sponsor's Model 136, from Table 11-15 of its report
# safinamide-pop-pk-pd.pdf), read in full from the page image: Parkinson's
# disease patients of the three phase 3 studies (016, 446 patients; MOTION,
# 452; SETTLE, 273), one compartment with first-order absorption and
# elimination, no lag:
#
#     CL/F 4.54 L/h, Vd/F 168 L, KA 1.19 /h        (70 kg Asian patient)
#     CL/F (WGT) 0.75 FIX, Vd/F (WGT) 1 FIX, CL/F~AGE -0.199
#     F~Whites 1.04, F~Others 1.16                   (relative to Asian)
#
# "For a typical 70-kg, 60-year old Asian patient ...".  The review prints the
# coefficients but not the equations; the power forms
#
#     CL/F = 4.54 (WT/70)^0.75 (AGE/60)^-0.199 / Frel
#     V/F  = 168  (WT/70)              / Frel
#
# are the ones its own statements imply, and reproduce them: over the
# analysis's 35-134 kg CL/F "ranged from 41% lower to 63% higher, and Vd/F
# ... from 50% lower to 91% higher" ((35/70)^0.75 = 0.59, (134/70)^0.75 =
# 1.63; 0.50 and 1.91), and "Over the age range of 30 to 80 years, there was
# an 18% decrease" ((80/30)^-0.199 = 0.82).  The age reference of 60 is read
# from "60-year old"; the 18% does not depend on it.  Frel is a relative
# BIOAVAILABILITY, so it divides both apparent parameters and scales the
# concentration, leaving the half-life alone.  Sex, renal function and
# levodopa had no effect.
#
# WHAT IS HELD FIXED
# ==================
# Race has no Patient Profile field.  White (Frel 1.04) is used, the larger
# share of the US and European trial population; an Asian patient's
# concentrations are 4% lower than drawn.
#
# DOSE BASIS: tablets are labelled as safinamide BASE ("131.76 mg of
# safinamide mesylate, equivalent to 100 mg"), and the phase 3 doses were the
# base, so a dose is entered as labelled.  Loprete 2016 (Pharmacol Res
# Perspect 4:e00251, PMC5045937, read in full) fitted the same study 016 and
# an earlier study dosed as the mesylate, and found the 30% difference between
# them that the salt would explain; its model is the registry's alternative,
# not implemented (its ka puts the peak at 5 h, against the label's 2-3 h).
#
# AGE: a negative power of age is infinite at 0, so the term sees
# max(age, ADULT_AGE_TERM_FLOOR) (R/constants.R).
#
# BODY SIZE (docs/weight-adjustment.md): the model carries its own allometric
# size covariate, evaluated at the pharmacokinetic weight with the
# fat-free-mass switch on and at total body weight with it off.
#
# CHECKS (White, 70 kg, 60 y): CL/F 4.37 L/h, V/F 161.5 L, half-life 25.6 h
# (label 20-26 h), time to peak after one tablet 3.2 h (label 2-3 h), and at
# 100 mg daily an AUC over the interval of about 22,900 ng.h/mL (healthy
# Chinese adults, Jing 2023: 23,100).
#
# NO EFFECT SITE, NO BAND
# =======================
# Safinamide inhibits MAO-B reversibly (IC50 9.3 nM in platelet-rich plasma;
# over 90% inhibition above 20 mg), but platelet MAO-B stays inhibited for
# days after the plasma concentration falls (still significant at 120 h; FDA
# review), the trials found no exposure-response (Loprete 2016), and no
# concentration range is published.  The help page says this.
#
# References
# ----------
# FDA CDER, Clinical Pharmacology Review, NDA 207145 (Xadago).
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2017/207145Orig1s000ClinPharmR.pdf
# Xadago label: https://www.accessdata.fda.gov/drugsatfda_docs/label/2021/207145s006lbl.pdf
# Loprete L et al. Pharmacol Res Perspect 2016;4:e00251.
#   https://doi.org/10.1002/prp2.251
# -----------------------------------------------------------------------------

# FDA review of NDA 207145, Pharmacometrics Review, Table 1 (Model 136)
SAFINAMIDE_CL       <- 4.54    # L/h, 70 kg, 60 y, Asian
SAFINAMIDE_V        <- 168     # L
SAFINAMIDE_KA       <- 1.19    # /h
SAFINAMIDE_CL_WT    <- 0.75    # fixed
SAFINAMIDE_V_WT     <- 1       # fixed
SAFINAMIDE_CL_AGE   <- -0.199
SAFINAMIDE_AGE_REF  <- 60      # years, from "60-year old"
SAFINAMIDE_F_WHITE  <- 1.04    # relative to Asian patients

#' Safinamide pharmacokinetics (oral)
#'
#' One compartment, apparent (/F), from the sponsor's Model 136 in the FDA
#' review of NDA 207145; see the header of \code{R/drugs_safinamide.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
safinamide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  pkW <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  frel <- SAFINAMIDE_F_WHITE
  cl <- SAFINAMIDE_CL * (pkW / 70)^SAFINAMIDE_CL_WT *
    (max(age, ADULT_AGE_TERM_FLOOR) / SAFINAMIDE_AGE_REF)^SAFINAMIDE_CL_AGE / frel
  v  <- SAFINAMIDE_V * (pkW / 70)^SAFINAMIDE_V_WT / frel

  default <- list(
    v1 = v, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0,
    ka_PO = SAFINAMIDE_KA / 60,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "FDA Pharmacometrics Review of NDA 207145 (Xadago), Table 1, Model 136 ",
      "(phase 3 patients with Parkinson's disease): one compartment, CL/F ",
      "4.54 L/h x (weight/70)^0.75 x (age/60)^-0.199, V/F 168 L x weight/70, ",
      "ka 1.19/h, both divided by the relative bioavailability of White ",
      "patients, 1.04; doses as safinamide base; concentration only. ",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2017/207145Orig1s000ClinPharmR.pdf"
    )
  )
}
