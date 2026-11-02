# -----------------------------------------------------------------------------
# Phenytoin (and fosphenytoin): one compartment, Michaelis-Menten elimination
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, Vmax in mg/min, Km and
# concentrations in mcg/mL (= mg/L), TOTAL (bound plus unbound) phenytoin acid
# in serum.  Drafted by Claude Code, 2026-10-10, at the request of Steven L.
# Shafer; the antiseizure registry is inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Odani A et al. (Biol Pharm Bull 1996;19:444-448) fitted 531 steady-state
# serum concentrations from 116 Japanese patients with epilepsy, children and
# adults, given oral phenytoin, with a one-compartment model and Michaelis-
# Menten elimination (NONMEM).  Read from the abstract (the paper is not in
# PubMed Central):
#
#     V    = 1.23 L/kg     in a typical 42 kg patient
#     Vmax = 9.80 mg/day/kg
#     Km   = 9.19 mcg/mL   (total phenytoin; x 1.16 with zonisamide)
#     "the parameter of power function of weight to adjust V and Vmax was
#     estimated to be 0.463"
#     F    = 1, assumed
#
# The weight term is read as V = 1.23 x 42 x (WT/42)^0.463 L and Vmax = 9.80
# x 42 x (WT/42)^0.463 mg/day: both equal the per-kilogram values at the
# typical 42 kg, and V and Vmax scale together, so the steady state depends on
# weight only through Vmax.  The other reading the abstract allows, a power
# on the per-kilogram value, would make volume per kilogram RISE with weight,
# which no phenytoin study has found.  At 70 kg this gives V 65.5 L (0.94
# L/kg) and Vmax 522 mg/day (7.5 mg/kg/day).
#
# WHY THIS SOURCE, AND ITS LIMITS
# ===============================
# The ChatGPT handoff proposed a free-drug model (Jun H et al., Drugs R D
# 2020;20:343-358, PMC7691416): 37 adults, Vmax 5.36 mg/kg/day and Km 0.532
# mg/L on UNBOUND phenytoin, with a linear binding model on albumin, and ka
# (0.225 /h) and F (0.859) fixed.  Its parameters are on the unbound scale and
# need its binding model to give the total concentration that is measured and
# banded; Odani's are on the total scale and need nothing else.  Odani is
# used, and Jun's fixed ka is taken for the extended-release capsule below.
# (The paper prints no Vmax per kg: 5.36 mg/kg/day is derived from its
# 15.3 mg/h at 70 kg, a weight^2.04 term and a bilirubin term, divided by
# the mean weight of 68.5 kg; at 70 kg it is 5.25 mg/kg/day.)
# Alqahtani S et al. (Pharmacology 2019;104:60-66), 43 Saudi adults, found
# V 0.61 L/kg, Vmax 6.12 mg/kg/day and Km 5.33 mg/L: a cross-check, below.
#
# Odani's subjects were Japanese; the steady state rests on Vmax and Km, which
# were estimated from troughs alone, so the shape within a dosing interval and
# the absorption are not from this source.  The handoff's own wording applies:
# an illustrative historical total-concentration model, not a universal US
# default.  The registry says so.
#
# CHECKS (70 kg, 40 y man, normal CYP2C9)
# =======================================
#     daily dose of phenytoin acid   Css = Km R / (Vmax - R)
#       200 mg/day                    5.7 mg/L
#       300 mg/day                   12.4 mg/L
#       400 mg/day                   30.2 mg/L
# Typical adults reach 10-20 mg/L on 300-400 mg/day with a disproportionate
# rise between them, the textbook observation.  Alqahtani's adults (65 kg, 330
# mg/day) had a mean trough of 11.2 mg/L; this model gives 15.4 mg/L at their
# mean dose and weight (their own parameters give 10.5).  An intravenous load
# of 18 mg/kg of phenytoin sodium gives 17.6 mg/L at the end of the load
# (1159 mg x 0.92 / 65.5 L, less what is cleared in the half hour).
#
# DOSE BASIS: ONE CONVERSION, AT INPUT
# ====================================
# Concentrations are phenytoin acid.  Each unit carries its own basis, applied
# once by the engine (saltFactor, R/advanceMichaelisMenten.R):
#
#   "mg", "mg/kg", "mg/min"     phenytoin SODIUM injection      x 0.92
#   "mg PO ER"                  phenytoin SODIUM extended       x 0.92
#                               capsules (Dilantin Kapseals)
#   "mg PO liquid"              phenytoin ACID suspension       x 1
#   "mg PO tablet"              phenytoin ACID chewable tablet  x 1
#                               (Infatabs)
#   "mg PE", "mg/kg PE",        FOSPHENYTOIN, prescribed in     x 0.92
#   "mg PE/min", "mg PE IM"     phenytoin sodium equivalents
#
# 0.92 is the ratio of molecular weights, 252.27 / 274.25 = 0.9199.  A dose of
# fosphenytoin is entered in PE, never as mg of fosphenytoin (1.5 mg of
# fosphenytoin sodium is 1 mg PE).
#
# ORAL ABSORPTION
# ===============
# Odani assumed F = 1 and estimated no absorption rate (steady-state troughs).
#   - ER capsule: ka 0.225 /h, FIXED by Jun 2020.  A single 300 mg dose
#     then peaks at about 11 h in the reference patient, inside the label's 4
#     to 12 h for the extended capsule.  Bioavailability 1 (on Odani's scale).
#   - Suspension and chewable tablet: no population estimate exists.  The
#     label gives the peak at 1.5 to 3 h; ka 2.0 /h puts it at 2.4 h for 300
#     mg in the reference patient (1.0 /h would put it at 4.1 h).  This
#     value is a LABEL-ANCHORED calibration, not an estimate, and is recorded
#     so in the registry's parameter audit.  Bioavailability 1.
#
# FOSPHENYTOIN
# ============
# Not a separate drug in the library: its doses are a route of phenytoin, so
# that the saturable elimination sees every source of phenytoin at once.
#   - conversion half-life 15 min (Cerebyx label; Boucher 1989 measured 8.0
#     +/- 2.9 min in 10 adults, Fischer 2003 reviews 7-15 min);
#   - intramuscular: complete (Boucher 1989, 100.5 +/- 20.3%), absorbed at
#     2.47 /h (Boucher 1989) into the same conversion compartment.
# The early displacement of phenytoin from albumin by fosphenytoin, which
# raises the unbound fraction for the first hour, is not modelled: the curve is
# total phenytoin.
#
# CYP2C9
# ======
# The CYP2C9 diplotype (CYP2C9_VALUES) sets a maximal-elimination-rate
# multiplier through the CPIC activity score (Karnes 2021).  Odani A et al.
# (Clin Pharmacol Ther 1997;62:287-292), 44 Japanese patients, measured Vmax
# 33% lower in *1/*3 heterozygotes (activity score 1); that is the anchor.
#   *1/*1, *1/*2  (activity score 2 or 1.5, normal): x 1.0 -- CPIC advises no
#                 adjustment at 1.5.
#   *1/*3, *2/*2  (activity score 1, intermediate): x 0.67 (Odani 1997).
#   *2/*3, *3/*3  (activity score 0.5 or 0, poor): x 0.50.  No population Vmax
#                 was retrievable for poor metabolisers; CPIC advises a
#                 maintenance dose at least 50% lower, which for a drug whose
#                 steady-state dose rate is proportional to Vmax is Vmax x 0.5.
#                 That factor is GUIDELINE-ANCHORED, not an estimate, and is
#                 recorded so in the parameter audit.
# CYP2C19 reduced Vmax by up to 14% in Odani 1997, not significant on its own,
# and is not applied.  No CYP2D6 term: none is established for phenytoin.
#
# NOT MODELLED
# ============
# Albumin and the unbound fraction (the curve is total; in hypoalbuminaemia or
# renal failure a total of 10 mg/L may carry a therapeutic unbound level),
# valproate displacement, enzyme induction or inhibition by co-medication,
# zonisamide (Km x 1.16 in Odani), saturable absorption of large oral doses,
# and the time until threshold (the engine does not compute it).  Pregnancy is
# below.
#
# PREGNANCY
# =========
# Tomson and colleagues (Epilepsia 1994;35:122-130) followed 93 pregnancies
# prospectively, 70 of them on carbamazepine or phenytoin alone, with the dose
# held constant unless seizures forced a rise and a baseline at least 10
# weeks after delivery in the same women: in the third trimester TOTAL
# phenytoin was 39% of baseline and UNBOUND phenytoin 82%.  Their companion
# paper (Epilepsia 1994;35:131-135) found that total clearance rose from the
# first trimester while unbound clearance rose less, significantly only in
# the third.  Lander 1977 (n = 10) and Dam 1979 (n = 14) found the same
# direction, and Yerby 1990 a significant rise in the free fraction with a
# non-significant fall in the free concentration.  Most of the fall in the
# total is therefore a fall in binding, not in the elimination of the active,
# unbound drug.
#
# A single clearance factor cannot say that, since this model's elimination
# saturates.  With the Patient Profile's Pregnant field on, the model applies
# two factors to the michaelisMenten block, and so to every route it serves
# (intravenous, oral, and fosphenytoin's mg PE and IM routes):
#
#   Km x PHENYTOIN_PREGNANCY_KM_FACTOR = 0.48.  Elimination saturates in the
#     unbound concentration, so Km on the total scale is Km(unbound) / fu,
#     and the unbound steady state, Km(unbound) R / (Vmax - R), does not
#     depend on fu at all: fu moves only the total, and at a constant dose
#     total / baseline = (unbound / baseline) / (fu / baseline).  0.48 is a
#     CONSTRUCTED CALIBRATION to Tomson's two group-mean falls, not a measured
#     free fraction: 0.39 / 0.82 = 0.476, the 2.10-fold rise in fu that those
#     two means imply (Table 5 of the first paper gives 6.1% to 13.2%, x2.15,
#     as mean free over mean total).  The free fraction Tomson MEASURED, each
#     sample's ultrafiltrate free / total (companion paper, Table 3), rose
#     only from 7.2 +/- 1.7% to 10.1 +/- 2.4% (x1.40), and Yerby 1990's from
#     7.7% to 10.1% (x1.31).  They are different statistics of different
#     cases: the measured fractions are unweighted means over all 36 phenytoin
#     cases, the 8 whose dose was raised among them; the concentrations come
#     from the 28 with an unchanged dose, and a ratio of their means weights
#     each sample by its total (SD 51-74% of the mean).  The same 28 cases'
#     mean clearances (Table 3: plasma x2.17, unbound x1.25 per kg) imply a
#     third value, x1.74.  Because free = fu x total in every sample, no
#     factors reproduce 39%, 82% and x1.40 together: x1.40 with an unbound of
#     82% gives a total of 59%, and with a total of 39% an unbound of 55%.
#     The calibration keeps the two concentration falls, which are what a
#     constant dose was measured to do, and gives up the measured fu.
#   Vmax x PHENYTOIN_PREGNANCY_VMAX_FACTOR = 1.1, chosen so that the unbound
#     steady state falls to 82% at a typical operating point.  At the band's
#     typical 15 mg/L, r = R / Vmax = 15 / (15 + 9.19) = 0.620, and the
#     unbound steady state is proportional to r / (1 - r) = 1.632; 82% of
#     that is 1.338, so r' = 0.572 and Vmax' = Vmax x 0.620 / 0.572 = 1.084
#     (at r = 0.5 the same rule gives 1.11): 1.1.
#
# Check: at the dose that gives 15 mg/L outside pregnancy, the total steady
# state becomes 9.19 x 0.48 x r' / (1 - r'), with r' = 0.620 / 1.1 = 0.564,
# = 5.7 mg/L, 38% of 15 (Tomson: 39%), and the unbound steady state 79% of
# its value (Tomson: 82%).  The volume is unchanged (no data).  The factors
# are tied to that operating point because the model saturates: nearer Vmax
# the same factors give a larger relative fall.  The help tables' cl1, the
# low-concentration limit Vmax / Km, rises 1.1 / 0.48 = 2.3-fold, which is a
# consequence of the two factors and not a measured clearance.
#
# INDEPENDENT CHECK (Dam 1979, read in full, 2026-10-19): Dam, Christiansen,
# Munck and Mygind (Clin Pharmacokinet 1979;4:53-62, Table II) give each
# woman's apparent phenytoin clearance, the daily dose over the trough level,
# through pregnancy and after delivery, with the doses ADJUSTED to keep the
# level in the therapeutic range.  Because the level was held, the ratio of
# dose over level in late pregnancy to the same after delivery is the ratio
# of the elimination rate at about the same total concentration, which the
# two factors predict without any further assumption: at 15 mg/L,
# 1.1 x (9.19 + 15) / (9.19 x 0.48 + 15) = 1.37.  In Dam's seven women with
# data at both times (weeks 37-40 against the latest postpartum values,
# weeks 8-20 where measured), the ratios were 1.08, 1.14, 3.33, 2.02, 1.61,
# 1.37 and 1.22: median 1.37, geometric mean 1.56.  The factors were fitted
# to Tomson's constant-dose data and reproduce Dam's dose-adjusted median.
#
# WHAT THIS MEANS FOR THE CURVE: the library plots TOTAL phenytoin, and the
# 10-20 mg/L band is a total-concentration band.  In late pregnancy the
# plotted total falls below the band while the unbound, active concentration
# stays near its baseline; a free phenytoin level, not the total, is the guide
# to the dose.  The fall of albumin itself is not modelled otherwise, the
# source population (Odani) was not pregnant, and the trimesters before the
# third are not represented.  See R/pregnancy.R.  (Claude Code, 2026-10-19.)
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate.  It is evaluated at the
# pharmacokinetic weight with the switch on and at total body weight with it
# off.
#
# BAND
# ====
# 10-20 mcg/mL total (ILAE, Patsalos 2008), typical 15.  Unbound 1-2 mcg/mL
# is the same range at normal binding.
#
# References
# ----------
# Odani A et al., Biol Pharm Bull 1996;19:444-448.
#   https://doi.org/10.1248/bpb.19.444
# Odani A et al., Clin Pharmacol Ther 1997;62:287-292.
#   https://doi.org/10.1016/S0009-9236(97)90031-X
# Karnes JH et al. (CPIC), Clin Pharmacol Ther 2021;109:302-309.
#   https://doi.org/10.1002/cpt.2008
# Jun H et al., Drugs R D 2020;20:343-358.  PMID 33026608.
#   https://doi.org/10.1007/s40268-020-00323-2
# Alqahtani S et al., Pharmacology 2019;104:60-66.
#   https://doi.org/10.1159/000500314
# Boucher BA et al., J Pharm Sci 1989;78:929-932.
#   https://doi.org/10.1002/jps.2600781110
# Fischer JH et al., Clin Pharmacokinet 2003;42:33-58.
#   https://doi.org/10.2165/00003088-200342010-00002
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# Dilantin, Dilantin-125, Dilantin Infatabs, phenytoin sodium injection and
# Cerebyx (fosphenytoin) US prescribing information, section 12.3.
# Tomson T, Lindbom U, Ekqvist B, Sundqvist A.  Epilepsia 1994;35:122-130.
#   https://doi.org/10.1111/j.1528-1157.1994.tb02921.x (pregnancy)
# Tomson T, Lindbom U, Ekqvist B, Sundqvist A.  Epilepsia 1994;35:131-135.
#   https://doi.org/10.1111/j.1528-1157.1994.tb02922.x (pregnancy)
# Yerby MS, Friel PN, McCormick K, et al.  Epilepsy Res 1990;5:223-228.
#   https://doi.org/10.1016/0920-1211(90)90042-t (pregnancy)
# Lander CM, Edwards VE, Eadie MJ, Tyrer JH.  Neurology 1977;27:128-131.
#   https://doi.org/10.1212/wnl.27.2.128 (pregnancy)
# Dam M, Christiansen J, Munck O, Mygind KI.  Clin Pharmacokinet 1979;4:53-62.
#   https://doi.org/10.2165/00003088-197904010-00005 (pregnancy)
# -----------------------------------------------------------------------------

#' Mass of phenytoin acid in a unit mass of phenytoin sodium
PHENYTOIN_SODIUM_FRACTION <- 252.27 / 274.25

#' Vmax multiplier by CYP2C9 diplotype; see the header of R/drugs_phenytoin.R
PHENYTOIN_CYP2C9_VMAX <- c(
  "*1/*1" = 1, "*1/*2" = 1, "*1/*3" = 0.67,
  "*2/*2" = 0.67, "*2/*3" = 0.5, "*3/*3" = 0.5
)

# Pregnancy (see the header): a calibration to Tomson 1994's group-mean
# concentrations, not a measured free fraction.  Km on the total scale is
# 0.39 / 0.82 = 0.476 of its value (total 39%, unbound 82% of baseline: the
# 2.1-fold fu rise those means imply, where the measured free fraction rose
# 1.40-fold), and Vmax rises so that the unbound steady state at 15 mg/L
# total falls to 82% (0.620 / 0.572 = 1.084).
PHENYTOIN_PREGNANCY_KM_FACTOR <- 0.48
PHENYTOIN_PREGNANCY_VMAX_FACTOR <- 1.1

#' Phenytoin pharmacokinetics (intravenous, oral and as fosphenytoin)
#'
#' Odani et al. (1996): one compartment with Michaelis-Menten elimination, on
#' total phenytoin; simulated by the numerical engine
#' (\code{advanceMichaelisMenten()}).  See the header of the file.
#'
#' @inheritParams cefazolin
#' @param cyp2c9 CYP2C9 diplotype, one of \code{CYP2C9_VALUES}
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies Km (total)
#'   by \code{PHENYTOIN_PREGNANCY_KM_FACTOR} and Vmax by
#'   \code{PHENYTOIN_PREGNANCY_VMAX_FACTOR} (Tomson 1994; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
phenytoin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      cyp2c9 = CYP2C9_DEFAULT, pregnant = FALSE)
{
  if (length(cyp2c9) != 1 || !cyp2c9 %in% CYP2C9_VALUES)
    stop("Invalid cyp2c9 for phenytoin: ", paste(cyp2c9, collapse = ", "))
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Odani 1996: per-kilogram values at the typical 42 kg, weight^0.463
  scale <- 42 * (pkW / 42)^0.463
  V    <- 1.23 * scale                                    # L
  vmax <- 9.80 * scale * PHENYTOIN_CYP2C9_VMAX[[cyp2c9]] / MINS_PER_DAY   # mg/min
  km   <- 9.19                                            # mg/L, total
  # Pregnancy (see the header): Km (total) and Vmax, in the one block every
  # route uses; the volume and the absorption are unchanged.
  if (isTRUE(pregnant)) {
    km   <- km * PHENYTOIN_PREGNANCY_KM_FACTOR
    vmax <- vmax * PHENYTOIN_PREGNANCY_VMAX_FACTOR
  }

  default <- list(
    v1 = V, v2 = 1, v3 = 1,
    # The low-concentration limit, Vmax / Km, for the help tables only: the
    # simulation uses the michaelisMenten block.
    cl1 = vmax / km, cl2 = 0, cl3 = 0,
    ka_PO = 0.225 / 60,          # ER capsule, Jun 2020 (fixed)
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Suspension and chewable tablet: label-anchored ka (see the header)
  fast <- list(ka_PO = 2.0 / 60, bioavailability_PO = 1, tlag_PO = 0)

  reference <- paste0(
    "Odani A et al., Biol Pharm Bull 1996;19:444-448 (one compartment, ",
    "Michaelis-Menten elimination of total phenytoin, Japanese patients); ",
    "CYP2C9 from Odani 1997 and CPIC 2021; fosphenytoin conversion from the ",
    "Cerebyx label and Boucher 1989. https://doi.org/10.1248/bpb.19.444"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("Km (total) x ", PHENYTOIN_PREGNANCY_KM_FACTOR, " and Vmax x ",
             PHENYTOIN_PREGNANCY_VMAX_FACTOR,
             ": the plotted total falls, the unbound stays near baseline"),
      paste0("Tomson T et al., Epilepsia 1994;35:122-130. ",
             "https://doi.org/10.1111/j.1528-1157.1994.tb02921.x")))
  }

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 15,
    upperTypical = 20,
    lowerTypical = 10,
    reference = reference,
    oralFormulations = list(liquid = fast, tablet = fast),
    michaelisMenten = list(
      vmax = vmax,
      km = km,
      kConversion = log(2) / 15,       # fosphenytoin, 1/min
      ka_IM = 2.47 / 60,               # fosphenytoin IM, 1/min
      bioavailability_IM = 1,
      saltFactor = list(
        IV      = PHENYTOIN_SODIUM_FRACTION,
        PE      = PHENYTOIN_SODIUM_FRACTION,
        default = PHENYTOIN_SODIUM_FRACTION,   # ER sodium capsule
        liquid  = 1,
        tablet  = 1
      )
    )
  )
}
