# -----------------------------------------------------------------------------
# Evolocumab (Repatha): one compartment, first-order subcutaneous absorption,
# parallel linear and saturable (target-mediated) elimination
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, amounts in mg, concentrations in
# mcg/mL (= mg/L) of UNBOUND evolocumab in serum, the analyte the source's
# ELISA measured.  Concentration only: no PCSK9 or LDL-cholesterol effect is
# plotted.  Drafted by Claude Code, 2026-11-19, at the request of Steven L.
# Shafer.
#
# SOURCE
# ======
# Kuchimanchi M, Grover A, Emery MG, Somaratne R, Wasserman SM, Gibbs JP,
# Doshi S.  Population pharmacokinetics and exposure-response modeling and
# simulation for evolocumab in healthy volunteers and patients with
# hypercholesterolemia.  J Pharmacokinet Pharmacodyn 2018;45:505-522
# (PMID 29736889, PMC5953983; read in full from Europe PMC's full-text XML,
# tables included, 2026-11-19).  3414 adults (18-80 years, 41-175 kg, half
# women) from 11 phase 1-3 studies, 7-420 mg IV and SC, 16,179 concentrations.
#
# Table 3 (final model, phase 1-3):
#
#     F    = 0.72           SC bioavailability, FIXED from phase 1a
#     ka   = 0.319 /day     FIXED from phase 1a
#     CL   = 0.105 L/day x (WT/median)^0.276
#     V    = 5.18 L       x (WT/median)^1.04 x 1.11 (women)
#     Vmax = 9.85 nM/day  x (WT/median)^0.145 x 1.13 (statin alone)
#                         x 1.20 (statin + ezetimibe) x (PCSK9/median)^0.194
#     km   = 27.3 nM        FIXED
#
# The covariate equations (Methods) are TVP x (cov/median)^theta for a
# continuous covariate and TVP x theta^indicator for a categorical one.
#
# UNITS OF VMAX AND KM.  The molecular weight is 144 kDa (Repatha label,
# section 11), so km 27.3 nM = 3.931 mg/L, the paper's "approximately 4
# ug/mL" (Discussion: linear below 0.4 ug/mL, "approximately 10-fold below
# the km").  Vmax is a CONCENTRATION rate: elimination is Vmax C / (km + C) in
# nM/day, i.e. Vmax x V in amount.  That reading is confirmed by the two
# half-lives the Discussion derives from Table 3: Vmax/km = 0.361 /day, a
# half-life of 1.92 days (paper "approximately 1.9 days"), and V/CL, 34.2
# days (paper "approximately 34 days").  With V 5.18 L, Vmax x V is 7.35
# mg/day; the Discussion says 7.7 mg/day, 5% more, which it does not derive.
#
# REFERENCE PATIENT.  The medians the covariate terms divide by are not
# printed (Table 2 gives means: 84.2 kg).  The paper's own reference patient
# is "an 84 kg male ... not taking other lipid-lowering medications and with
# baseline PCSK9 of 5.9 nM", so weight is centred at 84 kg (a [decision] in
# the rows note: if the unprinted median differs by 2 kg, V moves 2.5%).
# Statin, ezetimibe and baseline PCSK9 have no Patient Profile field and are
# held at that reference (no statin, PCSK9 at the median).  Most patients
# take a statin, which raises Vmax 13% (20% with ezetimibe) and lowers
# exposure, as the label's "approximately 20% decrease" with a high-intensity
# statin says.
#
# THE ENGINE.  Elimination saturates, so the drug runs on the Michaelis-Menten
# engine (R/advanceMichaelisMenten.R), with the linear clearance as clLinear.
# That engine takes a subcutaneous dose only through its saturableSC block
# (exenatide's zero-order-then-saturable input).  A plain first-order depot is
# the block's degenerate case, used here: fraction 1 (no zero-order share),
# bioavailability 1 inside the block, and a depot "half-saturation" amount
# kmKa (EVOLOCUMAB_DEPOT_KM, 1e12 mg) so large that the depot empties at
# kaMax S / (kmKa + S) = ka S to within S/kmKa, below 1e-9 for any dose the
# app accepts.  The block's zero-order phase has length EVOLOCUMAB_SC_DELAY
# (0.01 min) and carries nothing; the dose reaches the depot at its end.  The
# block passes everything in its depot to the circulation, so the 72%
# bioavailability is carried as the SC entry of saltFactor (the share of each
# dose that reaches the modelled pool).  This is EXACT first-order
# absorption, checked against an independent Runge-Kutta solution in the
# test, not an approximation of the source; a [decision] in the rows note
# asks whether the engine should gain a plain ka_SC depot instead.
#
# BODY SIZE.  The model carries its own weight terms on CL, V and Vmax,
# evaluated at the pharmacokinetic weight (size$pkWeight) with the switch on
# and at total body weight with it off, and its own sex term on V.  ka, F and
# km carry no size term and are not size quantities.  The weight terms are
# not held to the studied 41-175 kg: V ~ weight^1.04 extrapolates to a child
# as a plasma-sized volume should, where holding it at 41 kg would give a
# 20 kg child an adult's volume.  The label's paediatric troughs (10-17
# years, 420 mg monthly: 22.4 and 25.8 mcg/mL at weeks 12 and 24) are the
# check on that extrapolation; the model is not otherwise validated in
# children.
#
# CHECKS (84 kg man, switch off; independent RK4 in the test)
# ===========================================================
# The source's half-lives above, exactly.  NOT reproduced: the Discussion's
# "approximately 77% ... eliminated through this nonlinear pathway for a
# single 140 mg SC dose, and approximately 51% for a single 420 mg" -- Table
# 3 gives 87% and 73%, and no single Vmax reproduces both figures (2.9 mg/day
# gives 51% at 420 mg but 70% at 140 mg), so they are not a unit question;
# how they were computed is not said.  Against the label's single doses in
# healthy volunteers (Cmax 18.6 and 59.0 ug/mL, AUClast 188 and 924
# day.ug/mL), the model gives 10.4 and 39.5 ug/mL and 125 and 789 day.ug/mL,
# peaking at 5 and 7 days (label: median 3-4 days).  The paper says the model
# under-predicts some high concentrations, "particularly evident in the QM
# dosing regimens"; its outcomes of interest, the week 8-12 AUC and the week
# 12 trough, were well predicted.  At steady state the model gives troughs
# of 8.5 (140 mg every 2 weeks) and 16.5 ug/mL (420 mg monthly) at week 12
# without a statin; the label's phase 3 troughs, most patients on a statin,
# are 7.21 and 11.2.
#
# References
# ----------
# Kuchimanchi M et al., J Pharmacokinet Pharmacodyn 2018;45:505-522.
#   https://doi.org/10.1007/s10928-018-9592-y
# REPATHA (evolocumab) injection, prescribing information, Amgen, BLA 125522,
#   revised 10/2026 (DailyMed setid cd61e902-166d-4aa6-9f3c-a18c1008d07e).
# -----------------------------------------------------------------------------
EVOLOCUMAB_MW <- 144000              # g/mol (Repatha label, section 11)
EVOLOCUMAB_REFERENCE_WEIGHT <- 84    # kg, the paper's reference patient
EVOLOCUMAB_F_SC <- 0.72
EVOLOCUMAB_KA <- 0.319               # /day
EVOLOCUMAB_CL <- 0.105               # L/day
EVOLOCUMAB_CL_WT <- 0.276
EVOLOCUMAB_V <- 5.18                 # L
EVOLOCUMAB_V_WT <- 1.04
EVOLOCUMAB_V_FEMALE <- 1.11
EVOLOCUMAB_VMAX_NM <- 9.85           # nM/day, a concentration rate
EVOLOCUMAB_VMAX_WT <- 0.145
EVOLOCUMAB_KM_NM <- 27.3             # nM
# The degenerate saturableSC block (THE ENGINE, above)
EVOLOCUMAB_DEPOT_KM <- 1e12          # mg
EVOLOCUMAB_SC_DELAY <- 0.01          # min

#' Evolocumab pharmacokinetics (subcutaneous)
#'
#' One compartment with first-order subcutaneous absorption and parallel
#' linear and Michaelis-Menten elimination (Kuchimanchi 2018); see the header
#' of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
evolocumab <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Own weight terms on CL, V and Vmax: read at the pharmacokinetic weight
  # with the switch on, at total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w    <- pkW / EVOLOCUMAB_REFERENCE_WEIGHT

  nMtoMgL <- EVOLOCUMAB_MW * 1e-6          # nmol/L -> mg/L
  v  <- EVOLOCUMAB_V * w^EVOLOCUMAB_V_WT *
    (if (sex == SEX_FEMALE) EVOLOCUMAB_V_FEMALE else 1)                 # L
  clLinear <- EVOLOCUMAB_CL * w^EVOLOCUMAB_CL_WT / MINS_PER_DAY         # L/min
  km   <- EVOLOCUMAB_KM_NM * nMtoMgL                                    # mg/L
  vmax <- EVOLOCUMAB_VMAX_NM * w^EVOLOCUMAB_VMAX_WT * nMtoMgL * v /
    MINS_PER_DAY                                                        # mg/min
  ka   <- EVOLOCUMAB_KA / MINS_PER_DAY                                  # /min

  default <- list(
    v1 = v, v2 = 1, v3 = 1,
    # The low-concentration limit, linear plus Vmax / km, for the help tables
    # only: the simulation uses the michaelisMenten block.
    cl1 = clLinear + vmax / km, cl2 = 0, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Kuchimanchi 2018 (3414 adults, healthy volunteers and hypercholesterolemia, ",
      "7-420 mg IV and SC): one compartment, F 0.72, ka 0.319/day, linear CL 0.105 ",
      "L/day x (weight/84)^0.276, V 5.18 L x (weight/84)^1.04 x 1.11 (women), ",
      "Vmax 9.85 nM/day x (weight/84)^0.145, km 27.3 nM (MW 144 kDa); no statin, ",
      "ezetimibe or PCSK9 term applied (the paper's reference patient); unbound ",
      "evolocumab, concentration only. https://doi.org/10.1007/s10928-018-9592-y"
    ),
    michaelisMenten = list(
      vmax = vmax,
      km = km,
      clLinear = clLinear,
      # A first-order SC depot as the degenerate case of the saturable one
      # (THE ENGINE, header): kaMax / kmKa = ka, kmKa far above any depot.
      saturableSC = list(
        fraction = 1,
        duration = EVOLOCUMAB_SC_DELAY,
        kaMax = ka * EVOLOCUMAB_DEPOT_KM,
        kmKa = EVOLOCUMAB_DEPOT_KM,
        bioavailability = 1
      ),
      saltFactor = list(SC = EVOLOCUMAB_F_SC)
    )
  )
}
