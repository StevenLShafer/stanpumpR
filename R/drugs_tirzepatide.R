# -----------------------------------------------------------------------------
# Tirzepatide: two compartments, subcutaneous once weekly (Mounjaro)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: the FDA's linked fasting
# glucose / HbA1c model is not plotted.  Drafted by Claude Code, 2026-10-17,
# at the request of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Schneck K, Urva S.  Population pharmacokinetics of the GIP/GLP receptor
# agonist tirzepatide.  CPT Pharmacometrics Syst Pharmacol 2024;13:494-503
# (PMC10962491).  5802 participants, 19 studies, healthy volunteers and type 2
# diabetes, an intravenous study among them, so the parameters are on the TRUE
# scale with F estimated against it.  Table 3, read in full:
#
#     ka = 0.0373 /h            CL = 0.0329 L/h/70 kg    Q  = 0.126 L/h/70 kg
#     Vc = 2.47 L/70 kg         Vp = 3.98 L/70 kg        F  = 0.8 FIXED
#     CL and Q x (BW / 70)^0.8 (exponent fixed)
#     Vc and Vp x (FFM + 0.482 x FM) / 70 (exponent fixed at 1)
#     relative effect on F between studies -0.181
#
# FFM is Janmahasatian's fat-free mass (named in the paper; male
# 9270 W / (6680 + 216 BMI), female 9270 W / (8780 + 244 BMI)) and FM = W - FFM.
#
# BIOAVAILABILITY.  The paper writes F = 0.8 x (1 + fraction), with fraction
# -0.181 in some studies, without listing which.  With F = 0.8 x 0.819 =
# 0.655 a 90 kg patient's apparent clearance is 0.0329 x (90/70)^0.8 / 0.655
# = 0.061 L/h, exactly the apparent clearance the paper reports for patients
# with type 2 diabetes, so the model uses F = 0.655 for the commercial
# product in diabetes: a DERIVED value, recorded as such.  The 0.8 is the
# absolute bioavailability of the intravenous-comparison study.
#
# BODY SIZE: the model carries its own size covariates (total weight on the
# clearances, fat-weighted mass on the volumes, the paper having estimated
# how much fat counts), as the Eleveld models do, so adjustToFFM is ignored.
#
# CHECKS (90 kg, 170 cm woman)
# ============================
# CL/F 0.061 L/h, as the paper.  Half-life about 5 days (paper: mean 5.4 days
# in type 2 diabetes); peak 1-3 days after a dose (paper: tmax 8-72 h).
#
# References
# ----------
# Schneck K, Urva S, CPT Pharmacometrics Syst Pharmacol 2024;13:494-503.
#   https://doi.org/10.1002/psp4.13099
# Janmahasatian S et al., Clin Pharmacokinet 2005;44:1051-1065.
#   https://doi.org/10.2165/00003088-200544100-00004
# -----------------------------------------------------------------------------

TIRZEPATIDE_BIOAVAILABILITY <- 0.8 * (1 - 0.181)

#' Tirzepatide pharmacokinetics (subcutaneous, once weekly)
#'
#' Two compartments with first-order absorption (Schneck and Urva 2024); see
#' the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tirzepatide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # adjustToFFM is ignored: the model has its own size covariates.
  bmi <- weight / (height / 100)^2
  ffm <- if (sex == SEX_MALE) 9270 * weight / (6680 + 216 * bmi) else
    9270 * weight / (8780 + 244 * bmi)
  fm  <- weight - ffm
  clScale <- (weight / 70)^0.8
  vScale  <- (ffm + 0.482 * fm) / 70

  default <- list(
    v1  = 2.47 * vScale, v2 = 3.98 * vScale, v3 = 1,
    cl1 = 0.0329 * clScale / 60, cl2 = 0.126 * clScale / 60, cl3 = 0,
    ka_SC = 0.0373 / 60,
    bioavailability_SC = TIRZEPATIDE_BIOAVAILABILITY,
    tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Schneck and Urva 2024 (5802 participants): two compartments, ka 0.0373/h, ",
      "CL 0.0329 and Q 0.126 L/h x (weight/70)^0.8, Vc 2.47 and Vp 3.98 L x ",
      "(FFM + 0.482 FM)/70, F 0.8 x (1 - 0.181) in diabetes; concentration only. ",
      "https://doi.org/10.1002/psp4.13099"
    )
  )
}
