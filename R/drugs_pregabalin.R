# -----------------------------------------------------------------------------
# Pregabalin: oral only, one compartment, absorbed in proportion to the dose
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma (pregabalin is not bound to
# plasma proteins).
#
# SOURCE
# ======
# Chan PLS et al. pooled ten Pfizer studies: 724 adults (17-75 y, 40-180 kg,
# Cockcroft-Gault CLcr 42-261 mL/min, and a renal-impairment study down to
# about 10) and 255 children, healthy volunteers and patients with focal
# seizures (NONMEM 7.3, FOCE-I).  One compartment, first-order absorption after
# a lag, first-order elimination.  Table 2, for a fasted dose:
#
#     CL/F = 4.96 L/h x min(NCLcr, 96.4) / 96.4 x (WT/70)^0.52 x 0.92 (female)
#     V/F  = 39.8 L x (WT/70)^0.70 x 0.83 (female)
#     ka   = 10.0 /h
#     lag  = 0.32 h
#
# NCLcr is Cockcroft-Gault creatinine clearance normalised to 1.73 m^2 of body
# surface area.  Clearance is proportional to it up to the breakpoint of 96.4
# mL/min/1.73 m^2 and constant above.  Chan's 4.96 L/h at the breakpoint is the
# canonical Pfizer adult model's (Bockbrader 2011: CL/F = 0.0464 L/h per mL/min
# of CLcr, to 107 mL/min, which is 4.96 L/h), and clearance proportional to
# CLcr across renal impairment is Randinitis 2003 and the Lyrica label.
#
# The covariate equations are in Chan's supplement, which could not be
# retrieved; the form above is read from Table 2 and its footnotes (weight
# "normalized to 70 kg with a power function", male the reference sex, CL/F
# "a proportionality factor ... when CLcr <= breakpoint").  The body surface
# area is the library's Du Bois (R/renalFunction.R); Chan's own formula is in
# the supplement.
#
# The library's reference patient (70 kg, 170 cm, 35 y man, assumed creatinine
# 1.0 mg/dL) has NCLcr 97.6, above the breakpoint, so he receives Chan's
# typical values exactly: CL/F 4.96 L/h, V/F 39.8 L, half-life 5.6 h.
#
# ABSORPTION: ka READ AS PUBLISHED
# ================================
# Chan's footnote says ka was "estimated as a proportionality factor for the
# relationship between ka and elimination rate constant", to keep the model
# out of flip-flop, but Table 2 gives it as 10.0 /h.  Read as ka = 10 x k, ka
# would be 1.25 /h for the typical subject, the plasma peak would fall 2.4 h
# after a fasted dose and 150 mg would peak at 2.9 mcg/mL.  Fasted healthy
# volunteers peak at 0.7-1.3 h (Bockbrader 2010) and at 3.85-4.65 mcg/mL after
# 150 mg.  Read as published, ka 10 /h puts the peak at 0.76 h and 150 mg at 3.6
# mcg/mL, so that reading is taken.  (Either way ka is far above k, and
# whether ka is 10 or k + 10 changes nothing that can be seen.)
#
# The lag of 0.32 h (19.2 min, RSE 1.5%) is an estimated parameter and is kept,
# as gabapentin's is: time until threshold reads blank for the 19.2 min after
# each oral dose (test-recovery-lag.R).
#
# APPARENT SCALE
# ==============
# Every pregabalin model is apparent (/F): there is no intravenous product.
# Oral bioavailability is 90% or more and independent of dose (Bockbrader
# 2010, the label), so the apparent parameters are used with
# bioavailability_PO = 1; scaling CL and V both by 0.9 would give the same
# concentrations.  Pregabalin is absorbed in proportion to the dose, unlike
# gabapentin, whose transporter saturates: there is no oralSaturation block.
#
# Checks.  The reference patient: AUC after 150 mg 30.2 mcg.h/mL (observed
# 23.7-29.8), peak 3.6 mcg/mL at 0.76 h (observed 3.85-4.65 at about 1 h);
# after 300 mg, 7.1 mcg/mL (observed 7.80 +/- 1.66 in 24 volunteers, Calleja
# 2025, AUC 61.1 against 60.5 predicted).  A 65 kg, 162 cm, 49-year-old
# woman given 300 mg: 7.8 mcg/mL two hours later, against 7.67 +/- 3.00 at
# skin incision in Mueller 2026, who gave 300 mg two hours before induction
# to women of that size.  The peak is 5-20% below the single-dose studies: in
# densely sampled data, first-order absorption does not capture pregabalin's
# peak, which transit compartments do (Hong 2016).
#
# RENAL FUNCTION
# ==============
# Cockcroft-Gault at the patient's serum creatinine from the Patient Profile,
# or, when none is entered, an ASSUMED NORMAL creatinine (R/renalFunction.R),
# which captures the decline with age and the sex difference but not renal
# impairment.  Pregabalin, like gabapentin, is cleared unchanged by the
# kidney and accumulates in renal impairment.  Above the breakpoint clearance
# does not rise, so augmented renal clearance is not represented, by design of
# the source.  Haemodialysis, which removes pregabalin efficiently, is not
# represented.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Chan's model carries its own size covariates, on clearance through the
# weight term and through Cockcroft-Gault and the body surface area, and on
# volume through the weight term.  They are evaluated at the pharmacokinetic
# weight with the switch on and at total body weight with it off, as for the
# other models with their own weight and renal covariates; no library factor
# is applied on top.  The reference patient is the same either way.
#
# EFFECT SITE (see the constant below)
# ====================================
# See PREGABALIN_TPEAK.
#
# BAND
# ====
# 1.3-5.4 mcg/mL: the median steady-state average concentration in adults
# taking 150 and 600 mg/day, the labelled range for neuropathic pain and
# focal seizures (Chan 2021, Table 4); typical 2.7, for 300 mg/day.
# Orientation only: a chronic exposure, not a perioperative target, and a
# single preoperative dose of 150 or 300 mg peaks above it.
#
# NOT MODELLED
# ============
# Food (the label: the peak 25-30% lower and at about 3 h; Chan's food terms
# change ka and the lag), haemodialysis, and the slow, partial entry into
# cerebrospinal fluid (Buvanendran 2010, below).
#
# References
# ----------
# Chan PLS et al., Clin Pharmacol Ther 2021;110:132-140.
#   https://doi.org/10.1002/cpt.2132
# Bockbrader HN et al., Epilepsia 2011;52:248-257.
#   https://doi.org/10.1111/j.1528-1167.2010.02933.x
# Bockbrader HN et al., J Clin Pharmacol 2010;50:941-950.
#   https://doi.org/10.1177/0091270009352087
# Randinitis EJ et al., J Clin Pharmacol 2003;43:277-283.
#   https://doi.org/10.1177/0091270003251119
# Lyrica (pregabalin) US prescribing information, section 12.3.
# van Esdonk MJ et al., CPT Pharmacometrics Syst Pharmacol 2018;7:573-580.
#   https://doi.org/10.1002/psp4.12318
# Buvanendran A et al., Reg Anesth Pain Med 2010;35:535-538.
#   https://doi.org/10.1097/AAP.0b013e3181fa6b7a
# Mueller J et al., Anesth Analg 2026;143:373-382.
#   https://doi.org/10.1213/ANE.0000000000007824
# Calleja S et al., Pharmaceuticals (Basel) 2025;18:151.
#   https://doi.org/10.3390/ph18020151
# Hong T et al., Drug Des Devel Ther 2016;10:3995-4003.
#   https://doi.org/10.2147/DDDT.S123318
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# TIME TO PEAK EFFECT
# -----------------------------------------------------------------------------
# van Esdonk 2018 gave 300 mg by mouth to 16 healthy volunteers and modelled
# the cold pressor pain tolerance threshold with a turnover compartment: kout
# 0.39 /h (RSE 21%), a linear effect of the plasma concentration (0.135 per
# mg/L).  When the drug stimulates the production of the response in
# proportion to the concentration, a turnover model IS an effect site: the
# response is baseline x (1 + slope x Ce), with dCe/dt = kout (Cp - Ce), so
# ke0 = kout exactly.  When the drug instead slows the loss of the response,
# the equivalence is approximate.  The paper's equations are in a figure and a
# supplement that could not be retrieved, so which applies is not known; the
# delay, a half-time of 1.8 h, is the same order either way.  The electrical
# stimulation threshold gave a similar kout, 0.49 /h.  These are the only
# estimated human delays for pregabalin.
#
# Pregabalin in CSF peaks about 8 h after a 300 mg dose and reaches about a
# tenth to a fifth of plasma (Buvanendran 2010): a slower, deeper compartment
# than the analgesic delay, and not what the effect site represents.
#
# With ke0 0.39 /h on the reference patient's oral curve, the effect site
# peaks 283 min after an oral dose, 4.7 h, and that is the time to peak
# effect: getDrugPK() solves ke0 against the oral curve (tPeakRoute) for each
# patient, which gives 0.39 /h back for the reference patient.  The time is
# counted from the dose, lag included.  MEAC stays 0: pregabalin is not an
# opioid and does not belong on the MEAC panel.
PREGABALIN_TPEAK <- 283   # minutes after an ORAL dose
# -----------------------------------------------------------------------------


#' Pregabalin pharmacokinetics (oral)
#'
#' Chan et al. (2021): one compartment with first-order absorption after a
#' lag, clearance on body-surface-area-normalised Cockcroft-Gault creatinine
#' clearance to a breakpoint, and the effect site timed from van Esdonk et al.
#' (2018).  See the header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) evaluates Chan's weight,
#'   Cockcroft-Gault and body-surface-area terms at the pharmacokinetic
#'   weight; \code{FALSE} evaluates them at total body weight.  See the
#'   file's header.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
pregabalin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  # Size scaling (see the header): Chan's own covariates, evaluated at the
  # pharmacokinetic weight with the switch on and total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  female <- sex == SEX_FEMALE

  crcl  <- creatinineClearanceCG(pkW, age, sex,       # mL/min
                                 patientCreatinine(creatinine, sex))
  nclcr <- crcl * 1.73 / bsaDuBois(pkW, height)      # mL/min/1.73 m^2

  # Chan 2021, Table 2: clearance proportional to NCLcr up to the breakpoint
  v1  <- 39.8 * (pkW / 70)^0.70 * (if (female) 0.83 else 1)
  cl1 <- 4.96 * min(nclcr, 96.4) / 96.4 * (pkW / 70)^0.52 *
         (if (female) 0.92 else 1) / 60              # L/min
  v2  <- 1                                           # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  ka_PO              <- 10.0 / 60   # 1/min, fasted; read as published, see header
  bioavailability_PO <- 1           # apparent (/F) parameters
  tlag_PO            <- 0.32 * 60   # min

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, mcg/mL: median steady-state average at 150-600 mg/day, see the
  # header.  Orientation only.
  typical      <- 2.7
  upperTypical <- 5.4
  lowerTypical <- 1.3

  reference <- paste0(
    "Chan PLS et al., Clin Pharmacol Ther 2021;110:132-140. ",
    "One compartment with first-order absorption after a lag; clearance on ",
    "body-surface-area-normalised Cockcroft-Gault creatinine clearance (from ",
    "the entered creatinine or an assumed normal one) to a breakpoint; ",
    "effect site from van Esdonk 2018; oral only. ",
    "https://doi.org/10.1002/cpt.2132"
  )

  return(
    list(
      PK = PK,
      tPeak = PREGABALIN_TPEAK,
      # Timed after an oral dose: ke0 is solved against the oral curve.
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
