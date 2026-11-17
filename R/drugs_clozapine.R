# -----------------------------------------------------------------------------
# Clozapine and the norclozapine it forms (Li 2012): one compartment each,
# apparent oral parameters
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentrations are ng/mL.
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  Every number below was read from the full text of the source in
# PubMed Central (PMC4011352: Methods, Results, Figure 1, Tables 1 and 2).
#
# THE SOURCE
# ==========
# Li LJ, Shang DW, Li WB, et al.  Population pharmacokinetics of clozapine
# and its primary metabolite norclozapine in Chinese patients with
# schizophrenia.  Acta Pharmacol Sin 2012;33:1409-1416 (doi
# 10.1038/aps.2012.71).  162 inpatients with refractory schizophrenia (74 men,
# 88 women; age 18-59), oral clozapine two or three times a day, 809
# clozapine and 808 norclozapine concentrations from therapeutic monitoring
# (20% 0.5-4.5 h after a dose, the rest 8.5-15.5 h), HPLC-UV, NONMEM (ADVAN5).
# One compartment for each moiety; "a proportion of clozapine was converted
# into norclozapine in the central compartment of clozapine through a
# first-order conversion mechanism and the rest was converted into other
# metabolites".  Table 2:
#
#     clozapine     CL/F 21.9 L/h   V/F 526 L   Ka 1.3 /h (fixed)
#     conversion    KF 0.66 (fixed), the fraction converted to norclozapine
#     norclozapine  CLM 32.7 L/h    VM 624 L
#     covariates    smoking: CL x 1.45, CLM x 1.543
#                   male:    CL x 1.208, CLM x 1.242
#
# for a female nonsmoker.  "Other covariates such as weight and age did not
# significantly influence the PK parameters".  The parameters are APPARENT
# (divided by the unknown oral bioavailability, which the formation and the
# metabolite's parameters inherit), so only the oral route is offered.  The
# fixed ka was taken from rich-sampling studies (the source's reference 12);
# with 80% of samples drawn late in the interval, the model understates the
# swing within a dose interval (the Clozaril label: peak 319, trough 122
# ng/mL at 100 mg twice daily; this model, about 380 and 240 in a man).
#
# THE CONVERSION
# ==============
# Li's concentrations are in umol/L, so KF is a molar fraction: of each mole
# of clozapine cleared, 0.66 mol becomes norclozapine.  The library's
# metabolite route (R/advanceClosedFormMetabolite.R) forms the metabolite at
# kFormation from the parent's central compartment, in the parent's mass
# times mwRatio, so kFormation = KF x CL / V and mwRatio = 312.80 / 326.83,
# norclozapine's molecular weight over clozapine's (C17H17ClN4, C18H19ClN4).
# Formation and metabolite disposition were fitted together, so the pair is
# `joint` in METABOLITE_FORMATION_BASIS (R/metaboliteCoefficients.R), and the
# norclozapine model (R/drugs_norclozapine.R) is this source's.
#
# BODY SIZE AND SEX (docs/weight-adjustment.md)
# =============================================
# The source reports no body weight, and weight was not a covariate, so the
# published values are written for the library's reference MAN, female
# nonsmoker x 1.208 (CL 26.46 L/h; CLM 40.61 L/h), and scaled by the library's
# fat-free-mass factors with the switch on.  Those factors carry sex
# themselves (a woman of the same size: clearances x 0.865), which stands in
# for the source's own sex term (1 / 1.208 = 0.828), so the sex term is not
# applied twice.  With the switch off the published values are fixed, and the
# source's sex terms are applied (legacyClearance).
#
# HELD AT THE REFERENCE
# =====================
# Smoking: the reference is a NONSMOKER; the app has no smoking field.
# Smoking induces CYP1A2 and raises clearance 45% (norclozapine 54%), so a
# smoker's concentration is about a third lower, and STOPPING smoking raises
# it, a recognised cause of clozapine toxicity.  CYP1A2 inhibitors
# (fluvoxamine, ciprofloxacin) and inflammation, which raises concentrations,
# are not modelled.
#
# THE BAND
# ========
# Hart 2026 (AGNP consensus update 2026, Pharmacopsychiatry 59:166-295, Table
# 5, read in full): clozapine 350-600 ng/mL for schizophrenia, 12 h after the
# dose; laboratory alert level 1,000 ng/mL; level of recommendation 1.
# Norclozapine has no range of its own.
#
# PREGNANCY
# =========
# Not applied (inst/extdata/pregnancyRegistry.csv, unquantified): Westin 2018
# (4 pregnancies) found dose-adjusted concentrations 23% lower at week 34
# (95% CI -46% to +9%), and case reports go both ways.
# -----------------------------------------------------------------------------

# Li 2012, Table 2 (female nonsmoker) and the male factors
CLOZAPINE_CL_FEMALE   <- 21.9    # L/h
CLOZAPINE_V           <- 526     # L
CLOZAPINE_KA          <- 1.3     # 1/h, fixed
CLOZAPINE_KF          <- 0.66    # fraction converted to norclozapine, molar
CLOZAPINE_MALE_CL     <- 1.208
NORCLOZAPINE_CL_FEMALE <- 32.7   # L/h
NORCLOZAPINE_V        <- 624     # L
NORCLOZAPINE_MALE_CL  <- 1.242
# Molecular weights (C 12.011, H 1.008, Cl 35.45, N 14.007)
CLOZAPINE_MW    <- 18 * 12.011 + 19 * 1.008 + 35.45 + 4 * 14.007
NORCLOZAPINE_MW <- 17 * 12.011 + 17 * 1.008 + 35.45 + 4 * 14.007

#' Clozapine (Li 2012)
#'
#' One compartment, apparent oral, forming norclozapine; see the header of
#' \code{R/drugs_clozapine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
clozapine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1,
                        legacyClearance = if (sex == SEX_FEMALE) 1 / CLOZAPINE_MALE_CL else 1)
  cl <- CLOZAPINE_CL_FEMALE * CLOZAPINE_MALE_CL / 60 * size$clearance
  v  <- CLOZAPINE_V * size$volume

  default <- list(
    v1 = v, v2 = 1, v3 = 1,
    cl1 = cl, cl2 = 0, cl3 = 0,
    ka_PO = CLOZAPINE_KA / 60,
    bioavailability_PO = 1,           # apparent parameters
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Li LJ et al., Acta Pharmacol Sin 2012;33:1409-1416: one compartment, ",
    "apparent oral, CL/F 21.9 L/h (female nonsmoker; x 1.208 in men), V/F 526 L, ",
    "ka 1.3 /h; 66% (molar) converted to norclozapine.  Nonsmoker. ",
    "https://doi.org/10.1038/aps.2012.71"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 475,
    upperTypical = 600,
    lowerTypical = 350,
    reference = reference,
    prodrug = FALSE,
    metabolite = list(
      name              = "norclozapine",
      kFormation        = CLOZAPINE_KF * cl / v,
      firstPassFraction = 0,
      mwRatio           = NORCLOZAPINE_MW / CLOZAPINE_MW
    )
  )
}
