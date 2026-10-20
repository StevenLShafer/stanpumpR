# -----------------------------------------------------------------------------
# Haloperidol, oral only
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief.  The full text of Pilla Reddy 2013
# could NOT be read; the abstract confirms a two-compartment model in 122
# subjects.  The parameter values below are the brief's and are UNVERIFIED,
# although Li 2022 independently cites this model with CL/F 88 L/h and a total
# volume of 3169 L (= 669 + 2500), which matches them.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.
#
# THE SOURCE
# ==========
# Pilla Reddy V et al., J Clin Psychopharmacol 2013;33:731-739: oral
# haloperidol in patients with schizophrenia.  Two compartments, first-order
# absorption, no covariates:
#
#     CL/F 88 L/h, Q/F 233 L/h, Vc/F 669 L, Vp/F 2500 L, ka 0.236 /h
#
# Half-lives 1.3 h and 31 h.  APPARENT parameters, so oral only with
# bioavailability_PO = 1.  Intravenous haloperidol is the separate drug
# haloperidolIV (Li 2022), whose ABSOLUTE clearance is not this one times F.
#
# Body size: no weight covariate, so the published values are the 70 kg
# reference adult's and scale to fat-free mass (legacyVolume = 1).
#
# CYP2D6 (added 2026-10-11 from the full text of Park 2006)
# ==========================================================
# Park JY et al., J Clin Psychopharmacol 2006;26:135-142: single 5 mg oral
# doses in healthy Korean men, noncompartmental.  CL/F (Table 1, placebo
# phase) was 4.7 L/h/kg with CYP2D6*1/*1 (n = 8) and 3.6 with *10/*10
# (n = 7), a ratio of 0.77 (not significant on its own; significant, 0.56,
# with itraconazole).  Only that ratio is used: Park's absolute CL/F (about
# 300 L/h) and half-life (12.6 h) are biased by sampling to 72 h against a
# 31 h terminal half-life, and must not replace Pilla Reddy's.
#
#     normal 1, intermediate 0.77
#
# Poor and ultrarapid metabolisers were not studied: they take the nearest
# studied value (0.77 and 1).  Pilla Reddy's population genotype mix is not
# reported, so "normal" is read as their typical patient.
#
# Interactions not modelled, documented in the help: rifampin lowered
# haloperidol troughs to 30% of baseline over 1 to 2 weeks and stopping it
# raised them 3.3-fold over about 2 weeks (Kim YH 1996, J Clin
# Psychopharmacol 16:247-252); itraconazole lowered CL/F to 0.67 (Park 2006).
#
# EFFECT SITE (added 2026-10-11 from the full text of Lim 2013)
# ==============================================================
# Lim HS et al., Pharm Res 2013;30:683-693: [18F]fallypride PET and plasma
# in 8 healthy Korean men after 0.5, 1 or 3 mg daily for 7 days.  Effect
# compartment with full inhibition of binding potential, driven by plasma:
#
#     ke0 0.213 /h (putamen; 0.209 caudate), Ce50 0.791 ng/mL (putamen;
#     0.650 caudate)
#
# ke0 is supplied directly.  Lim's own PK model (one compartment with a
# saturable binding compartment, CL/F 75 L/h) cannot be run by this engine
# and is not used; the PD is transferred onto Pilla Reddy's PK, and
# antipsychoticProfiles() records the transfer.  D2 occupancy is
# 100 x Ce/(0.791 + Ce): a receptor biomarker, not a target.  Lim's
# simulations put 2 mg every 12 h near a 65-80% window.  The band is the
# serum therapeutic reference range of the AGNP update 2026, 1-5 ng/mL 12 h
# after a dose (Hart et al., Pharmacopsychiatry 2026;59:166-295, Table 5;
# level 1), narrowed from the 2017 update's 1-10.
#
# The same paper's PANSS model (Pilla Reddy) drives its drug term with each
# patient's AVERAGE STEADY-STATE concentration (Emax 0.31, EC50 3.58 ng/mL),
# inside a Weibull placebo and dropout model that has not been transcribed;
# it is recorded in antipsychoticProfiles() and not computed.
# -----------------------------------------------------------------------------

HALOPERIDOL_CL <- 88      # L/h, apparent
HALOPERIDOL_Q  <- 233     # L/h
HALOPERIDOL_VC <- 669     # L
HALOPERIDOL_VP <- 2500    # L
HALOPERIDOL_KA <- 0.236   # 1/h
HALOPERIDOL_KE0 <- 0.213  # 1/h, Lim 2013 (putamen); also used by haloperidolIV

# CL/F multiplier by CYP2D6 phenotype (Park 2006); see the header
HALOPERIDOL_CYP2D6 <- c(
  poor         = 0.77,
  intermediate = 0.77,
  normal       = 1,
  ultrarapid   = 1
)

#' Haloperidol pharmacokinetics (oral)
#'
#' Pilla Reddy et al. (2013): two compartments, apparent oral parameters,
#' with CYP2D6 on clearance (Park 2006) and a D2-occupancy effect site (Lim
#' 2013).
#'
#' @inheritParams cefazolin
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
haloperidol <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT,
                        adjustToFFM = TRUE)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  cyp <- unname(HALOPERIDOL_CYP2D6[[cyp2d6]])

  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = HALOPERIDOL_VC * size$volume,
    v2 = HALOPERIDOL_VP * size$volume,
    v3 = 1,
    cl1 = HALOPERIDOL_CL / 60 * size$clearance * cyp,   # L/min
    cl2 = HALOPERIDOL_Q  / 60 * size$clearance,
    cl3 = 0,
    ka_PO = HALOPERIDOL_KA / 60,                  # 1/min
    bioavailability_PO = 1,                       # apparent (/F)
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Pilla Reddy V et al., J Clin Psychopharmacol 2013;33:731-739. ",
    "https://doi.org/10.1097/JCP.0b013e3182a4ee2c (apparent oral parameters; ",
    "values not yet checked against the full text); CYP2D6: Park JY et al., ",
    "J Clin Psychopharmacol 2006;26:135-142; effect site: Lim HS et al., ",
    "Pharm Res 2013;30:683-693. https://doi.org/10.1007/s11095-012-0906-2"
  )

  list(
    PK = PK,
    tPeak = 0,                       # ke0 is supplied, not solved
    ke0 = HALOPERIDOL_KE0 / 60,      # 1/min
    MEAC = 0,
    typical = 3,                      # AGNP 2026, ng/mL; mirrors the CSV
    upperTypical = 5,
    lowerTypical = 1,
    reference = reference
  )
}
