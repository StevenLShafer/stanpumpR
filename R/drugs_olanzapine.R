# -----------------------------------------------------------------------------
# Olanzapine, oral only
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief.  Every Table 3 value used here was
# checked against the full text of Sun 2021, and so is the power form of the
# age term on Vc/F: the Methods write every continuous covariate as
# ln(TVP) = ln(theta) + theta_cov x ln(cov / reference), and the reference
# subject is 36 years old (also the median age).
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  The source reports L/h, 1/h and h.
#
# THE SOURCE
# ==========
# Sun et al., J Clin Pharmacol 2021;61:1430-1441: 9905 plasma concentrations
# from 601 healthy subjects and patients with schizophrenia, given olanzapine
# alone or with samidorphan.  Two compartments, first-order absorption after a
# lag.  Reference subject: a 70 kg, 36-year-old, non-Black, nonsmoking man,
# fasted, with normal hepatic and renal function.
#
#     CL/F = 15.5 L/h x (weight/70)^0.75 (exponent fixed)
#     Vc/F = 656 L    x (weight/70)      x (age/36)^0.356
#     Vp/F = 225 L,  Q/F = 6.15 L/h      (no covariates)
#     ka   = 0.861 /h,  tlag = 0.782 h (fixed)
#
# Of the categorical covariates on CL/F only sex is applied here (female x
# 0.862), because sex is the only one the Patient Profile carries.  Smoking
# (x 1.30), Black race (x 1.10), rifampin (x 1.80), moderate hepatic (x 0.875,
# fixed) and severe renal impairment (x 0.801) are not, so this is a
# nonsmoker's curve; a smoker clears olanzapine 30% faster.  The fed-state F
# multiplier (0.943) is not applied: this is the fasted reference.
#
# The parameters are APPARENT (/F), so olanzapine is offered orally only with
# bioavailability_PO = 1.  Zang 2021's one-compartment Chinese psychiatric
# model is a separate alternative and is not merged in.
#
# Body size: the model carries its own weight covariate, so it is evaluated at
# the pharmacokinetic weight with the fat-free-mass switch on (size$pkWeight)
# and at total weight with it off, as cefazolin does.  Vp and Q are not scaled,
# because the source does not scale them.
#
# EFFECT
# ======
# No effect site.  The brief pairs this with Kapur 1998's plasma D2 occupancy
# hyperbola (Emax 100%, EC50 10.3 ng/mL), but the Kapur abstract reports
# occupancy by dose only, and the EC50 has NOT been checked against the full
# text.  It is carried in antipsychoticProfiles() as unverified.  The band is
# the serum therapeutic reference range of the AGNP TDM task force's systematic
# review, 20 to 40 ng/mL 12 h after a dose (Wesner et al., J Clin Psychiatry
# 2023;84:22r14626; inst/extdata/therapeuticRanges.csv), which replaced AGNP
# 2017's 20 to 80 ng/mL.
# -----------------------------------------------------------------------------

OLANZAPINE_CL     <- 15.5    # L/h, apparent, 70 kg nonsmoking man
OLANZAPINE_VC     <- 656     # L, apparent, 70 kg, 36 years
OLANZAPINE_VP     <- 225     # L
OLANZAPINE_Q      <- 6.15    # L/h
OLANZAPINE_KA     <- 0.861   # 1/h
OLANZAPINE_TLAG   <- 0.782   # h, fixed in the source
OLANZAPINE_FEMALE <- 0.862   # multiplier on CL/F
OLANZAPINE_AGE_VC <- 0.356   # power of age/36 on Vc/F
# Youngest age the Vc/F age term sees.  Sun's subjects were adults (olanzapine
# and samidorphan are approved from 18), and the power term goes to zero at
# age 0, which the Patient Profile allows; so a younger patient's age term is
# held at 18, as buprenorphine and ondansetron hold theirs.  Weight still
# scales the child's volume and clearance.
OLANZAPINE_MIN_AGE <- 18     # years

#' Olanzapine pharmacokinetics (oral)
#'
#' Sun et al. (2021): two compartments with an absorption lag, apparent oral
#' parameters, with weight, age and sex covariates.  Plasma only.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
olanzapine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  wt <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  female <- if (sex == SEX_FEMALE) OLANZAPINE_FEMALE else 1
  ageVc  <- max(age, OLANZAPINE_MIN_AGE)   # see OLANZAPINE_MIN_AGE

  default <- list(
    v1 = OLANZAPINE_VC * (wt / 70) * (ageVc / 36)^OLANZAPINE_AGE_VC,
    v2 = OLANZAPINE_VP,
    v3 = 1,
    cl1 = OLANZAPINE_CL / 60 * (wt / 70)^0.75 * female,   # L/min
    cl2 = OLANZAPINE_Q / 60,
    cl3 = 0,
    ka_PO = OLANZAPINE_KA / 60,                           # 1/min
    bioavailability_PO = 1,                               # apparent (/F)
    tlag_PO = OLANZAPINE_TLAG * 60                        # 46.9 min
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Sun L et al., J Clin Pharmacol 2021;61:1430-1441. ",
    "https://doi.org/10.1002/jcph.1911 (apparent oral parameters; ",
    "nonsmoker, fasted, no interacting drugs)"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 30,        # Wesner 2023, ng/mL 12 h after a dose; mirrors the CSV
    upperTypical = 40,
    lowerTypical = 20,
    reference = reference
  )
}
