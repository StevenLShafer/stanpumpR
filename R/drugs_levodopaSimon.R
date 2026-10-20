# -----------------------------------------------------------------------------
# Levodopa, oral, with carbidopa (Simon 2016): one compartment, with the motor
# and dyskinesia effect site fitted to the same patients
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of levodopa.
#
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, who
# supplied the article and its electronic supplement (inst/help/models/
# parkinsons.md, inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# Simon N, Viallet F, Boulamery A, Eusebio A, Gayraud D, Azulay J-P.  A
# combined pharmacokinetic/pharmacodynamic model of levodopa motor response
# and dyskinesia in Parkinson's disease patients.  Eur J Clin Pharmacol
# 2016;72:423-430 (PMID 26936272), read in full; the supplement holds only
# goodness-of-fit figures.  30 patients in Marseille, Hoehn and Yahr stage 3
# (24) or 4 (6), every one with peak-dose dyskinesia (median weight 63 kg,
# 40-91; age 65, 51-78; disease 12 years), after a 12-hour washout, given a
# single dose of carbidopa-levodopa of 150% of their usual dose (mean 380 mg,
# 200-550), sampled to 5 h.  NONMEM 7.3, FOCE-I.
#
# Table 2, one compartment with first-order absorption (no lag):
#
#     KA   1.86 /h
#     CL/F 36.6 L/h x (WT/70)^0.75
#     V/F  42.9 L   x (WT/70)
#
# The weight terms were not estimated: "none of the tested covariates
# decreased ISV and the objective function.  However, for comparison with
# other values in the literature, a theory based allometry model was used".
#
# Table 3, model 2 (the final model: one effect compartment for both effects),
# sequentially on that plasma model:
#
#     KE0 1.37 /h (equilibration half-life 0.51 h)
#     UPDRS III: E = E0 (1 - Emax Ce^g / (EC50^g + Ce^g)),
#                E0 31.4, Emax 0.72, EC50 1410 ug/L, g 4.26
#     dyskinesia (Goetz): E = Emax Ce / (EC50 + Ce), Emax 17.9, EC50 6280 ug/L
#
# "Dyskinesia appeared for levodopa plasma concentrations 4.45 times higher
# than for a therapeutic motor response" (6280 / 1410), and the authors offer
# 6280 ug/L as "a cut off concentration for patients suffering from
# dyskinesia".
#
# WHY A SECOND LEVODOPA
# =====================
# R/drugs_levodopa.R (Othman & Dutta 2014's plasma, Mao 2013's effect site)
# puts a model fitted in one population on another's.  Simon's is one study,
# its effect site estimated on its own plasma model, and in a different
# patient: later, dyskinetic disease.  The two disagree in the direction the
# literature leads one to expect (Harder & Baas 1998, Contin 1998): Simon's
# patients need nearly twice the concentration for half the motor response
# (1.41 against 0.81 mcg/mL) and their response is far steeper (Hill 4.26
# against 2.5), close to all-or-none.  Its clearance is higher than Othman's
# (36.6 against 24.8 L/h) and its distribution is a single volume.
#
# WHERE TO BE CAREFUL
# ===================
# The authors' visual predictive check "indicated an underprediction of the
# peak concentration": observed peaks were at about 1 h, the model's is at
# 46 min.  The doses were large (150% of usual) and the sampling ended at 5 h,
# so the terminal phase is not characterised (Othman's slow peripheral
# compartment does not appear).  The variability of KA between patients was
# 122%.  Valid for carbidopa-levodopa; no sex, age or disease-duration
# covariate survived.
#
# BODY SIZE (docs/weight-adjustment.md): the model's own allometric weight
# terms, evaluated at the pharmacokinetic weight with the fat-free-mass
# switch on and at total body weight with it off.
#
# THRESHOLD AND BAND: endCe in drugDefaults_global.csv is the UPDRS EC50,
# 1.41 mcg/mL, so "time until threshold" measures when the motor effect falls
# below half its maximum; the band is the AGNP reference range, 0.9 to 2.0
# mcg/mL, as for levodopa.
#
# References
# ----------
# Simon N et al. Eur J Clin Pharmacol 2016;72:423-430.
#   https://doi.org/10.1007/s00228-016-2034-0
# -----------------------------------------------------------------------------

# Simon 2016, Table 2 and Table 3 (model 2)
LEVODOPA_SIMON_KA   <- 1.86    # /h
LEVODOPA_SIMON_CL   <- 36.6    # L/h at 70 kg
LEVODOPA_SIMON_V    <- 42.9    # L at 70 kg
LEVODOPA_SIMON_KE0  <- 1.37    # /h, UPDRS III and dyskinesia
LEVODOPA_SIMON_UPDRS_EC50 <- 1.41   # mcg/mL (1410 ug/L)
LEVODOPA_SIMON_UPDRS_HILL <- 4.26
LEVODOPA_SIMON_UPDRS_EMAX <- 0.72   # fractional improvement
LEVODOPA_SIMON_DYSK_EC50  <- 6.28   # mcg/mL (6280 ug/L)

#' Levodopa, oral, with carbidopa (Simon 2016)
#'
#' One compartment with allometric weight, and the motor and dyskinesia effect
#' site fitted to the same 30 patients with peak-dose dyskinesia; see the
#' header of \code{R/drugs_levodopaSimon.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
levodopaSimon <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  w <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight

  default <- list(
    v1  = LEVODOPA_SIMON_V * w / 70, v2 = 1, v3 = 1,
    cl1 = LEVODOPA_SIMON_CL * (w / 70)^0.75 / 60, cl2 = 0, cl3 = 0,
    ka_PO = LEVODOPA_SIMON_KA / 60,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, ke0 = LEVODOPA_SIMON_KE0 / 60, MEAC = 0,
    typical = 1.4, upperTypical = 2.0, lowerTypical = 0.9,
    reference = paste0(
      "Simon N et al., Eur J Clin Pharmacol 2016;72:423-430 (30 patients, Hoehn ",
      "and Yahr 3-4, with peak-dose dyskinesia): one compartment, ka 1.86/h, ",
      "CL/F 36.6 L/h x (weight/70)^0.75, V/F 42.9 L x weight/70; effect site ",
      "ke0 1.37/h, UPDRS III EC50 1.41 mcg/mL (Hill 4.26), dyskinesia EC50 ",
      "6.28 mcg/mL. Band 0.9-2.0 mcg/mL (AGNP). https://doi.org/10.1007/s00228-016-2034-0"
    )
  )
}
