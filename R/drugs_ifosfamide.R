# -----------------------------------------------------------------------------
# Ifosfamide, intravenous (Kerbusch 2000): plasma ifosfamide, with the
# autoinduction of its own metabolism
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-10, at the request of Steven L. Shafer, who
# asked for the autoinduction engine (R/advanceMichaelisMenten.R,
# AUTOINDUCTION) for this drug and cyclophosphamide.  Every equation and
# number below was read on the pages of the PDF, which Shafer supplied
# (2026-11-10; papers archive PMID_10848719).
#
# THE SOURCE
# ==========
# Kerbusch T, Huitema ADR, Ouwerkerk J, Keizer HJ, Mathot RAA, Schellens JHM,
# Beijnen JH.  Evaluation of the autoinduction of ifosfamide metabolism by a
# population pharmacokinetic approach using NONMEM.  Br J Clin Pharmacol
# 2000;49:555-561 (doi 10.1046/j.1365-2125.2000.00217.x).  15 adults (9 men,
# 6 women, 23-72 years, 49-82 kg) with soft-tissue sarcoma; 9 or 12 g/m2 as a
# 72-h continuous infusion every 4 weeks (10.3-23.5 g); plasma at 0-92 h,
# 176 concentrations; GC with nitrogen-phosphorus detection, validated
# 0.192-383 uM; NONMEM V, ADVAN6.
#
# THE MODEL (Methods, equations 1-4; Table 1)
# ===========================================
#     dA1/dt = R - CL A2 Cp,                       Cp = A1 / V
#     dA2/dt = Kenz,out [1 - A2 (1 - Cp / (Cp + IC50))],   A2(0) = 1
#
#     CL 2.94 L/h (initial)   V 43.5 L   Kenz,out 0.0546 /h (t1/2 12.7 h)
#     IC50 30.7 uM
#
# A2 is the relative amount of enzyme (CYP3A4 by the authors' reading): the
# drug inhibits its degradation, so it rises toward 1 + Cp/IC50 while drug is
# present (about a doubling in the first day of a 72-h infusion) and returns
# to 1 with a 12.7-h half-life afterwards.  There is no Imax or Hill term.
# The model ran in micromoles; the paper prints no molecular weight, and the
# IC50 is converted here with ifosfamide's 261.09 g/mol: 30.7 uM = 8.016 mg/L.
# Simulated, the model gives peaks of 108 and 204 uM for 10 and 20 g over
# 72 h, as Figure 6 shows (about 115 and 200); test-drugs-ifosfamide.R pins
# them.  Table 2's simulated AUCs are printed in "uM h" but are g h/L (1.65
# for 10 g, the table's 1.7), and its 20-g value (2.7, against 2.94) does not
# reproduce; the figure, not the table, is the check.
#
# Not modelled: the dechloroethylated metabolites and 4-hydroxyifosfamide
# (the authors' later work); covariates (none was tested).  Valid for the
# 72-h infusion it was fitted to; shorter infusions are an extrapolation of
# the same equations.
#
# DOSE BASIS: ifosfamide is approved and dosed as itself (no salt), and the
# paper worked in ifosfamide; doses in mg of ifosfamide.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate: the volume and both clearances scale by the library's
# factors with the switch on and are fixed with it off (the cohort's mean
# weight was 59 kg).  IC50 is a concentration and does not scale.
#
# PREGNANCY (R/pregnancy.R): no human pharmacokinetics in pregnancy; nothing
# applied.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Kerbusch 2000, Table 1
IFOSFAMIDE_KERBUSCH <- list(
  cl = 2.94,          # L/h, the initial (uninduced) clearance
  v = 43.5,           # L
  kEnzOut = 0.0546,   # /h
  ic50uM = 30.7       # umol/L of ifosfamide
)
IFOSFAMIDE_MW <- 261.09  # g/mol (C7H15Cl2N2O2P), not printed in the paper

#' Ifosfamide, intravenous (Kerbusch 2000), with autoinduction
#'
#' One compartment whose clearance is induced by the drug; see the header of
#' \code{R/drugs_ifosfamide.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ifosfamide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- IFOSFAMIDE_KERBUSCH
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  cl <- p$cl / 60 * size$clearance              # L/min, uninduced

  default <- list(
    v1  = p$v * size$volume,
    v2  = 1,                                 # placeholder: one compartment
    v3  = 1,
    cl1 = cl,                                # the uninduced clearance (help tables)
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Kerbusch T, Huitema ADR, Ouwerkerk J, et al. Evaluation of the autoinduction of ",
    "ifosfamide metabolism by a population pharmacokinetic approach using NONMEM. Br J ",
    "Clin Pharmacol 2000;49:555-561. 15 adults, 72-h infusions: one compartment, V 43.5 L, ",
    "CL 2.94 L/h x a relative enzyme amount whose degradation (0.0546 /h) the drug ",
    "inhibits (IC50 30.7 uM). https://doi.org/10.1046/j.1365-2125.2000.00217.x"
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
      clLinear = 0,
      autoinduction = list(
        cl = cl,
        kEnz = p$kEnzOut / 60,
        emax = 0,
        imax = 1,
        ic50 = p$ic50uM * IFOSFAMIDE_MW / 1000   # mg/L
      )
    )
  )
}
