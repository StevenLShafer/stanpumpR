# -----------------------------------------------------------------------------
# Cyclophosphamide, intravenous, in children (McCune 2009): plasma
# cyclophosphamide, with the autoinduction of its own metabolism
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-10, at the request of Steven L. Shafer, who
# asked for the autoinduction engine (R/advanceMichaelisMenten.R,
# AUTOINDUCTION) for this model and ifosfamide.  Every equation and number
# below was read on the pages of the PDF, which Shafer supplied (2026-11-10;
# papers archive PMID_18927240).  A candidate of the cyclophosphamide group
# (R/modelSelection.R): the program chooses it for children.
#
# THE SOURCE
# ==========
# McCune JS, Salinger DH, Vicini P, Oglesby C, Blough DK, Park JR.  Population
# pharmacokinetics of cyclophosphamide and metabolites in children with
# neuroblastoma: a report from the Children's Oncology Group.  J Clin
# Pharmacol 2009;49:88-102 (doi 10.1177/0091270008325928).  22 children (17
# boys), 1.30-9.37 years (median 3.16), 8.7-52.8 kg, BSA 0.45-1.47 m2;
# cyclophosphamide 400 mg/m2 (13.3 mg/kg at 12 kg or less) over 30 min daily
# for 5 days, each followed by topotecan; plasma before, at the end of the
# infusion and 3, 6 and 24 h after doses 1 and 4, 196 samples; LC-MS; NONMEM
# V, first-order method.
#
# THE MODEL (Figure 1, the four equations, Table II)
# ==================================================
#     dA1/dt = -(CL_NON / V) A1 - (CL_IND / V) A1 A2
#     dA2/dt = k_ENZ [1 + E_MAX (A1/V) / (EC50 + A1/V)] - k_ENZ A2,   A2(0) = 1
#
#     V 13.1 L/m2   CL_NON 1.83 L/h/m2   CL_IND 1.79 L/h/m2
#     k_ENZ 6.57e-3 /h (fixed-effect, half-life 105 h)
#     E_MAX 5 (fixed)   EC50 0.6 umol/L (fixed)
#
# The drug raises the formation of an enzyme (A2, relative to 1 before the
# first dose) toward 1 + E_MAX = 6 times its baseline, slowly (105-h half-life),
# so the inducible clearance grows over a 5-day course and the later doses
# are cleared faster (Figure 4).  With EC50 0.6 uM against peaks near 100 uM
# the induction is near its maximum whenever drug is present.  EC50 is
# converted with the paper's own molecular weight, 261.1 g/mol: 0.157 mg/L.
#
# Not carried: 4-hydroxycyclophosphamide and carboxyethylphosphoramide
# mustard, which the inducible route forms (their rate constants depend on a
# measured GFR, which the Patient Profile does not have, and their volumes
# were fixed from adults); topotecan, given with every dose, was not tested
# as a covariate.  No covariate on the parent but body-surface area.
#
# DOSE BASIS
# ==========
# Doses in mg of cyclophosphamide, converted to umol with 261.1 g/mol, the
# anhydrous molecular weight; the paper does not say whether its doses were
# the anhydrous or the monohydrate (279.1).  By Shafer's rule (2026-11-10:
# absent a statement in the paper, the parameters are in the units of the
# FDA-approved product), doses are entered as the product is labelled, with
# no conversion.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own covariate, body-surface area (Mosteller; the paper's
# formula followed each institution's guidelines and is not printed), from
# the pharmacokinetic weight with the switch on and total weight with it off.
#
# PREGNANCY: a children's model; see the cyclophosphamide row.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# McCune 2009, Table II (per m2 of body-surface area)
CYCLOPHOSPHAMIDE_MCCUNE <- list(
  v = 13.1,           # L/m2
  clNon = 1.83,       # L/h/m2, the non-inducible route
  clInd = 1.79,       # L/h/m2, the inducible route (times the enzyme amount)
  kEnz = 6.57e-3,     # /h
  emax = 5,           # fixed
  ec50uM = 0.6        # umol/L, fixed
)
CYCLOPHOSPHAMIDE_MW <- 261.1   # g/mol, the paper's

#' Cyclophosphamide, intravenous, in children (McCune 2009), with autoinduction
#'
#' One compartment, a non-inducible and an inducible clearance; see the
#' header of \code{R/drugs_cyclophosphamideMcCune.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cyclophosphamideMcCune <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CYCLOPHOSPHAMIDE_MCCUNE
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  clNon <- p$clNon * bsa / 60                  # L/min
  clInd <- p$clInd * bsa / 60

  default <- list(
    v1  = p$v * bsa,
    v2  = 1,                                 # placeholder: one compartment
    v3  = 1,
    cl1 = clNon + clInd,                     # before any induction (help tables)
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "McCune JS, Salinger DH, Vicini P, et al. Population pharmacokinetics of ",
    "cyclophosphamide and metabolites in children with neuroblastoma: a report from the ",
    "Children's Oncology Group. J Clin Pharmacol 2009;49:88-102. 22 children 1.3-9.4 ",
    "years: one compartment, V 13.1 L/m2, CL 1.83 + 1.79 x an enzyme amount L/h/m2, ",
    "the enzyme induced by the drug (Emax 5, EC50 0.6 uM, turnover 0.00657 /h); BSA ",
    "by Mosteller. https://doi.org/10.1177/0091270008325928"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    michaelisMenten = list(
      vmax = 0,
      km = 1,                                # unused: no saturable elimination
      clLinear = clNon,
      autoinduction = list(
        cl = clInd,
        kEnz = p$kEnz / 60,
        emax = p$emax,
        ec50 = p$ec50uM * CYCLOPHOSPHAMIDE_MW / 1000,   # mg/L
        imax = 0
      )
    )
  )
}
