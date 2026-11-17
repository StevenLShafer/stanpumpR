# -----------------------------------------------------------------------------
# Vincristine, intravenous, in children (Nijstad 2022): plasma vincristine,
# two compartments with saturable binding to a tissue (beta-tubulin) pool
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer,
# who decided that day to extend the Michaelis-Menten engine for it
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source P18).
# Every number below was read on the page images of the PDF (Table I p. 2490,
# Table II and Fig. 1 p. 2491, Eq. 1 p. 2489); the supplement's Table S1
# gives the ODEs.  Named for its population, as methotrexatePediatric is, so
# it cannot become the adult default (vincristine, Igarashi 2021).
#
# THE SOURCE
# ==========
# Nijstad AL, Chu W-Y, de Vos-Kerkhof E, et al.  A population pharmacokinetic
# modelling approach to unravel the complex pharmacokinetics of vincristine in
# children.  Pharm Res 2022;39:2487-2495 (doi 10.1007/s11095-022-03364-1).
# 206 patients, 253 occasions, 1297 plasma samples; age 0.04-33.9 (median 8.3)
# years, 25 under a year; weight 2.9-126 (median 27.1) kg; Dutch and British
# cohorts.  1.4 (0.4-2.5) mg/m2, capped at 2 mg, by bolus (214 occasions) or
# 15-113 min infusion (39).  Plasma vincristine by LC-MS(/MS), LLOQ 0.1-0.5
# ng/mL.  NONMEM 7.3, FOCE-I.
#
# THE MODEL (Table II, Eq. 1, Table S1)
# =====================================
# Central (Vc) and linear peripheral (Vp) compartments, elimination CL from
# the central, and a saturable binding compartment (beta-tubulin) fed from the
# central AMOUNT:
#
#   dA(Vc)/dt = R - CL/Vc A(Vc) - kon A(Vc) (1 - A(bound)/Bmax)
#                 + koff A(bound) - Q/Vc A(Vc) + Q/Vp A(Vp)
#   dA(Vp)/dt = Q/Vc A(Vc) - Q/Vp A(Vp)
#   dA(bound)/dt = kon A(Vc) (1 - A(bound)/Bmax) - koff A(bound)
#
#   CL = 30.6 L/h (WT/70)^0.75      Q = 63.2 L/h (WT/70)^0.75
#   Vc = 5.39 L WT/70               Vp = 400 L WT/70
#   Bmax = 0.525 mg (WT/70) (age/18)^-0.199
#   kon = 1300 /h (fixed)           koff = 11.5 /h
#
# The plasma concentration is the free central A(Vc)/Vc; the bound amount is
# a tissue store, not in the sample.  kon and koff are taken as NOT
# allometrically scaled: the footnote names the 70-kg parameters and kon,
# koff are not among them, though the Methods say allometry was "a priori
# included on all PK parameters" [decision, recorded in the registry].
#
# HOW THE ENGINE CARRIES IT: QUASI-EQUILIBRIUM
# ============================================
# kon is 1300 /h, about 22 /min, beyond the Runge-Kutta stability limit at
# the engine's 2-minute step (2.78 / 2 min), and integrating the full system
# at a step that is stable would cost some 100,000 steps a week of plot.  The
# binding is far faster than everything else (the next-fastest rate, (CL +
# Q)/Vc, is 17 /h), so the bound amount is held at its equilibrium with the
# free central amount,
#
#   A(bound) = Bmax A(Vc) / (KD + A(Vc)),    KD = Bmax koff / kon (an amount),
#
# which is advanceMichaelisMenten()'s rapidBinding with kd = KD/Vc and
# bmaxCentral = Bmax/Vc (concentrations; KD/Vc is 0.86 ng/mL x
# (age/18)^-0.199 at any weight), amaxPeripheral 0, vmax 0 and the linear
# clearance CL as clLinear, with the option plotted = "unbound" added for this
# drug.  test-drugs-vincristinePediatric.R measures the reduction, at the
# engine's own step, against a fine-step Runge-Kutta integration of the full
# three-state system, after a bolus in a child, an adult and an infant: within
# 1.1% at 5 min and 0.8% from 30 min (the infant the worst case each time;
# the child and the adult within 0.5%), 0.1% at 6 and 24 h and 0.4% at 72 h.
#
# CHECKS (typical patients, 1.6 mg bolus at 8.3 years and 27.1 kg; 2 mg at 18
# years and 70 kg): the full system gives 106 and 65.4 ng/mL at 5 min, 4.03
# and 2.31 at 1 h, 1.04 and 0.684 at 24 h, and an AUC equal to dose/CL (the
# binding is reversible, elimination central only).  The paper prints no
# concentration or AUC to reproduce; its Fig. 2 (CL and Bmax against age at
# WHO median weights) agrees with the equations to about 5%.  Binding
# lengthens the terminal phase (apparent half-life about 19 h in an adult
# against 13.5 h from the linear compartments alone).
#
# NOT COVERED: thrombocytes (tested, not retained); intrathecal use (never);
# no height, BSA or renal covariate exists in the model.  Age is valid from
# 0.04 years; the age term, a negative power, is floored at the youngest age
# the source studied, 0.04 years (VINCRISTINE_NIJSTAD_MIN_AGE; the rule in
# AGENTS.md permits a floor below 18 where the source fitted those ages).
# Sex is not a covariate.
#
# DOSE BASIS
# ==========
# Doses are entered as labelled, mg of vincristine sulfate; the paper reports
# doses as "vincristine" mg without converting, and the assay measures the
# base (0.894 of the sulfate), so the parameters map the dose as given to the
# base measured [decision, recorded in the registry].
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own size covariate, weight, on every parameter but
# kon and koff: with the switch on it is evaluated at the pharmacokinetic
# weight, with it off at total body weight.
#
# PREGNANCY (R/pregnancy.R): a paediatric model; not applicable.
# NO EFFECT SITE, NO BAND: neuropathy and exposure-response relations are not
# a therapeutic range.
# -----------------------------------------------------------------------------

# Nijstad 2022, Table II: at 70 kg (and Bmax at 18 years); hours and mg
VINCRISTINE_NIJSTAD <- list(
  cl = 30.6,          # L/h
  q = 63.2,           # L/h
  vc = 5.39,          # L
  vp = 400,           # L
  bmax = 0.525,       # mg
  bmaxAgeExponent = -0.199,
  ageReference = 18,  # years
  kon = 1300,         # /h, fixed in the source
  koff = 11.5,        # /h
  weightReference = 70
)
# The youngest patient the source studied (0.04 years, about two weeks); the
# negative age power on Bmax is floored there (AGENTS.md, "Age terms").
VINCRISTINE_NIJSTAD_MIN_AGE <- 0.04

#' Vincristine, intravenous, in children (Nijstad 2022)
#'
#' Two compartments allometric in weight with saturable binding to a tissue
#' pool whose capacity scales with weight and age; plotted as the unbound
#' plasma concentration, run by \code{advanceMichaelisMenten()}.  See the
#' header of \code{R/drugs_vincristinePediatric.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
vincristinePediatric <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- VINCRISTINE_NIJSTAD
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- (if (isTRUE(adjustToFFM)) size$pkWeight else weight) / p$weightReference
  ageTerm <- max(age, VINCRISTINE_NIJSTAD_MIN_AGE) / p$ageReference

  cl <- p$cl * w^0.75 / 60                              # L/min
  q  <- p$q * w^0.75 / 60                               # L/min
  vc <- p$vc * w                                        # L
  vp <- p$vp * w                                        # L
  bmax <- p$bmax * 1000 * w * ageTerm^p$bmaxAgeExponent # mcg
  kd <- bmax * p$koff / p$kon                           # mcg, an amount

  default <- list(
    v1 = vc,
    v2 = vp,
    v3 = 1,                                  # placeholder: two compartments
    # The clearance of the unbound drug, for the help tables; the simulation
    # uses the michaelisMenten block.
    cl1 = cl,
    cl2 = q,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Nijstad AL, Chu W-Y, de Vos-Kerkhof E, et al. A population pharmacokinetic ",
    "modelling approach to unravel the complex pharmacokinetics of vincristine in ",
    "children. Pharm Res 2022;39:2487-2495. 206 patients aged 0.04-34 years: CL 30.6 ",
    "L/h x (WT/70)^0.75, Q 63.2 L/h x (WT/70)^0.75, Vc 5.39 L x WT/70, Vp 400 L x WT/70, ",
    "saturable binding to beta-tubulin from the central amount (Bmax 0.525 mg x WT/70 x ",
    "(age/18)^-0.199, kon 1300 /h, koff 11.5 /h), carried at quasi-equilibrium (age ",
    "floored at 0.04 years). Doses as labelled (vincristine sulfate). Unbound plasma ",
    "vincristine in ng/mL. https://doi.org/10.1007/s11095-022-03364-1"
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
      clLinear = cl,
      rapidBinding = list(
        kd = kd / vc,                        # ng/mL
        bmaxCentral = bmax / vc,             # ng/mL
        amaxPeripheral = 0,
        plotted = "unbound"
      )
    )
  )
}
