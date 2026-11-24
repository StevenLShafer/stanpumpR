# -----------------------------------------------------------------------------
# Rocuronium: two-compartment model of Kleijn 2011, rocuronium alone
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (mg/L) of rocuronium.  Doses are rocuronium
# bromide as labelled, the dose basis of the source's trials (0.6-1.2 mg/kg).
#
# SOURCE
# ======
# Kleijn HJ, Zollinger DP, van den Heuvel MW, Kerbusch T.  Br J Clin
# Pharmacol 2011;72:415-433 (PMID 21535448; card papers/digests/
# PMID_21535448.md), read in full on the PDF, Table 4 on the page image
# (p. 423).  Rocuronium, sugammadex and their complex were fitted jointly in
# 446 subjects of 8 pooled trials (426 patients, 20 volunteers; aged 1-91
# years, median 43; 9.6-139 kg, median 74.5; creatinine clearance
# 4.30-239 mL/min, median 119; Table 2), total (free plus complexed)
# rocuronium by LC-MS/MS.  R/drugs_sugammadex.R is the same paper's
# sugammadex, so the library now draws the two drugs from one fit.
#
# The rocuronium arm of Table 4, AGE in years, BW the body weight in kg, CR
# the creatinine clearance in mL/min:
#
#     CL = 0.269 x [1 - 0.00678 (AGE - 43)] x (BW/70)^0.75      L/min
#     V1 = 4.73  x exp[-0.00143 (CR - 119)] x (BW/70)           L
#     Q2 = 0.279 x Q2RAC x (BW/70)^0.75                         L/min
#     V2 = 6.76  x exp[0.00613 (AGE - 43)]  x (BW/70)           L
#
# Q2RAC is 1 for the source's non-Asian category and 0.788 (1 - 0.212) for
# its Asian category.  stanpumpR has no such covariate, so it is held at 1,
# the reference.  "No effect of gender on any of the PK parameters was
# found" (p. 423), so sex enters only through the fat-free mass (below).
#
# At the library's reference man (70 kg, 170 cm, 35 years, assumed
# creatinine, Cockcroft-Gault 113.4 mL/min) this gives CL 0.284 L/min
# (4.05 mL/kg/min), V1 4.77 L, Q2 0.279 L/min and V2 6.44 L.
#
# THE INTERACTION IS NOT MODELLED
# ===============================
# Kleijn's complex (association k1, dissociation k2 = exp(-3.38) /min, kd
# 0.0559 uM fixed) and the extra loss of rocuronium from the effect
# compartment that free sugammadex causes (ks) need coupled, mass-conserving
# states the closed-form engine does not have.  This row is therefore
# Kleijn's rocuronium WITHOUT sugammadex: giving sugammadex in the dose table
# does not change it, as before.  The covariate patient (the weight the
# equations see and the creatinine clearance) is computed for both drugs by
# kleijn2011Patient() in R/drugs_sugammadex.R, so the two rows always
# describe the same patient; they share no parameter.
#
# AGE
# ===
# The clearance term is linear in age and was fitted over 1-91 years, so
# by the library's age rule (AGENTS.md, *Age terms*) it floors at the
# youngest age the source studied, 1 year (ROCURONIUM_KLEIJN_MIN_AGE; the
# paper's "infants" were all aged 1), and the peripheral-volume term with
# it.  It is finite and positive for every legal age (0.68 at 90 years; it
# would reach zero only at 190).  As printed, the term RAISES clearance
# below 43 years (by 5.4% at 35, 28.5% at 1 year).  The Discussion (p. 430)
# says "an age-dependent linear decrease in clearance in elderly, but not in
# younger patients ... Bodyweight is the driving component in paediatrics",
# which could be read as a term held at 43 below 43; Table 4 prints no such
# break, and nlmixr2lib reads it linear too.  Decided by Steven L. Shafer,
# 2026-11-24: keep it as printed.  The term was found by PsN's stepwise
# search, which tests a linear form first, so its shape below 43 was never
# tested separately; in children it is entangled with the fixed 0.75 size
# exponent (the authors report overparameterisation when they estimated
# it), so the pair is used as fitted.  Only 52 of the 446 subjects were
# under 18 (4 aged 1, 21 aged 2-11, 27 aged 12-17).  Against Wierda 1997
# (Br J Anaesth 78:690, PMID 9215021, abstract; arterial, 5 + 5), children
# 2.3-8 years cleared 6.7 mL/kg/min, which the term as printed gives
# (6.2-7.3 at typical weights; 5.0-5.8 held at 43).  Infants 0.1-0.8 years
# cleared 4.2 mL/kg/min, about half of what either form gives (8.6 as
# printed at 0.5 years and 7.5 kg): below 1 year there are no data and the
# model, which has no maturation term, very likely overpredicts clearance.
#
# RENAL FUNCTION
# ==============
# Creatinine clearance enters only the central volume.  CR is Cockcroft-
# Gault at the patient's serum creatinine from the Patient Profile, or at an
# ASSUMED NORMAL creatinine for age and sex when none is entered
# (R/renalFunction.R; a child's creatinine, entered or assumed, is read on
# the adult scale, adultEquivalentCreatinine()), exactly as for sugammadex.
# The source used Cockcroft-Gault from 18 years and a BSA-denormalised
# Schwartz below.  CR is held within the range the source studied,
# 4.30-239 mL/min (KLEIJN_2011_CR_RANGE, R/drugs_sugammadex.R, shared with sugammadex's
# peripheral volume), because the exponential term
# is unbounded outside it: a small entered creatinine in a large patient
# gives a Cockcroft-Gault in the thousands and a central volume of a few
# millilitres.  Inside the range nothing is changed.  Renal function does
# not enter rocuronium's clearance in this model.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every parameter carries its own allometric weight term, so, as for
# sugammadex, the equations are evaluated at the pharmacokinetic weight
# (70 kg x FFM / FFM_ref) with the switch on and at total body weight with
# it off.  The reference man therefore receives Kleijn's values in both
# positions.  No cohort anchoring applies (every parameter has its own size
# term).
#
# EFFECT SITE
# ===========
# Time to peak effect at the adductor pollicis: the population median time
# to maximum block after 0.15 mg/kg in Cortinez LI et al., Br J Anaesth
# 2007;99(5):679-685 (https://pubmed.ncbi.nlm.nih.gov/17681967/), 4.5 min
# (range 2.0-7.7), whose ke0 by every method was 0.19-0.20 /min.  The time
# to peak is kept and ke0 re-solved from it, as the library does whenever a
# plasma model changes: on Kleijn's plasma model, whose central volume is
# larger (4.77 against Plaud's 3.92 L) and whose distribution is slower
# (k12 0.059 against 0.100 /min), the ke0 that peaks at 4.5 min is 0.40 /min
# at the reference man (test-drugs-rocuronium.R pins the range), twice
# Cortinez's.  Kleijn's own ke0 (Table 4, T4/T1 ratio, no sevoflurane) is
# 0.134 /min x (BW/70)^-0.25, which on this plasma model puts the
# effect-site peak at 8.6 min, beyond the range of Cortinez's patients
# (2.0-7.7 min).  With sevoflurane Kleijn's ke0 is 0.058 /min.  Decided by
# Steven L. Shafer, 2026-11-23: "keep the peak effect at 4.5 minutes and
# adjust the ke0 as needed", so ke0 is re-solved from Cortinez's 4.5 min.
#
# THE TERMINAL PHASE
# ==================
# Two compartments: the terminal half-life is 38.5 min at the reference man,
# against the label's 1.4-2.4 h; Kleijn found a third compartment unstable
# and the two-compartment model adequate "within the clinically relevant
# concentration range" (p. 421).  Late, low concentrations after long
# infusions or large doses fall faster here than they will in a patient.
#
# BAND
# ====
# Unchanged: 1 to 2.2 mcg/mL, typical 1.5, a library band of effect-site
# concentrations for surgical block (inst/extdata/therapeuticRanges.csv).
# Kleijn's EC50 for the T4/T1 ratio is 1.62 uM without sevoflurane and
# 0.98 uM with it (Table 4), not used.
#
# REPLACED SOURCE
# ===============
# Until 2026-11-23 rocuronium was Plaud B et al., Clin Pharmacol Ther
# 1995;58:185-191 (PMID 7648768): eight adults, a two-compartment fit per
# kilogram with fixed rate constants (V1 0.056 L/kg, k10 0.1746, k12
# 0.100381, k21 0.0245 /min) and no covariates; at 70 kg V1 3.92 L, V2 16.1
# L, CL 0.684 L/min and Q2 0.393 L/min.  Its clearance, 9.78 mL/kg/min,
# was 2.3 times Wierda 1991's 4.0 mL/kg/min and the label's 0.25 L/kg/h,
# plausibly from its 6-h sampling (the question its card put, now
# resolved).  Kleijn's 4.05 mL/kg/min at the reference man agrees with
# both.  Replaced on Steven
# L. Shafer's decision of 2026-11-23, after the nlmixr2lib audit found the
# library drawing rocuronium and sugammadex from different studies.
# -----------------------------------------------------------------------------

ROCURONIUM_KLEIJN_MIN_AGE <- 1                 # years; see AGE in the header

rocuronium <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  # The covariate patient, shared with sugammadex (R/drugs_sugammadex.R):
  # the weight the equations see and the Cockcroft-Gault creatinine clearance
  patient <- kleijn2011Patient(weight, height, age, sex, adjustToFFM, creatinine)
  W  <- patient$W
  A  <- max(age, ROCURONIUM_KLEIJN_MIN_AGE)
  CR <- min(max(patient$CR, KLEIJN_2011_CR_RANGE[1]),
            KLEIJN_2011_CR_RANGE[2])
  q2Race <- 1                                  # non-Asian, the reference

  cl1 <- 0.269 * (1 - 0.00678 * (A - 43)) * (W / 70)^0.75
  v1  <- 4.73 * exp(-0.00143 * (CR - 119)) * (W / 70)
  cl2 <- 0.279 * q2Race * (W / 70)^0.75
  v2  <- 6.76 * exp(0.00613 * (A - 43)) * (W / 70)
  v3  <- 1                                     # no third compartment
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Cortinez 2007's median time to maximum block (see EFFECT SITE)
  tPeak <- 4.5
  typical <- 1.5
  upperTypical <- 2.2
  lowerTypical <- 1
  MEAC <- 0
  reference <- paste0(
    "Kleijn HJ et al., Br J Clin Pharmacol 2011;72:415-433 (rocuronium arm ",
    "of the joint rocuronium-sugammadex fit; age on clearance and peripheral ",
    "volume, creatinine clearance from the entered creatinine or an assumed ",
    "normal one on the central volume; non-Asian; binding to sugammadex not ",
    "modelled). https://doi.org/10.1111/j.1365-2125.2011.04000.x; ",
    "time to peak effect from Cortinez LI et al., Br J Anaesth ",
    "2007;99:679-685. https://pubmed.ncbi.nlm.nih.gov/17681967/"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
