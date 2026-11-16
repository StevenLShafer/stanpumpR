# Physiology sliders: "what if" changes to organ function
#
# Four factors, each a multiple of normal function on a log scale from 0.1
# (a tenth of normal) to 10 (ten times normal), all 1 by default:
#
#   liver    hepatic metabolic capacity (Vm, and so intrinsic clearance)
#   renal    renal function: excretion of unchanged drug, and renal
#            metabolism where a drug has it (propofol)
#   cardiac  cardiac output; liver and kidney blood flow are fixed shares of it
#   gi       gastrointestinal absorption rate (ka of the oral routes)
#
# They change a drug's PARAMETERS, once, before the engines see them
# (applyPhysiology(), called by getDrugPK()).  The engines are unchanged: the
# PK is fixed throughout the simulation, as it is for any covariate.
#
# THE LIVER: the well-stirred model (Wilkinson and Shand 1975; Shafer,
# "Basic pharmacokinetics and pharmacodynamics", chapter 2, Figures 10-12).
# Hepatic clearance is CL_H = Q E, with E = CL_int / (Q + CL_int).  With a
# flow factor q and a capacity factor l (CL_int scales with Vm), the ratio of
# the new hepatic clearance to the old one needs only the drug's extraction
# ratio E at normal flow:
#
#   CL_H' / CL_H = q l / (q (1 - E) + l E)
#
# so a flow-limited drug (E near 1) follows hepatic blood flow (q) and is
# nearly indifferent to metabolic capacity, and a capacity-limited drug (E
# near 0) follows metabolic capacity (l) and is nearly indifferent to flow,
# which is Figures 10 and 11 of the chapter.  The new extraction ratio is
# E' = l E / (q (1 - E) + l E), which sets the oral first-pass loss.
#
# Each drug's clearance is split three ways in
# inst/extdata/physiologyParameters.csv: the renal fraction, the hepatic
# fraction, and the rest (extrahepatic, non-renal: plasma esterases, Hofmann
# elimination), which no slider changes.
#
# THE KIDNEY: a renal share that is filtration of unchanged drug scales in
# proportion to renal function.  A drug the kidney METABOLISES with a
# measured renal extraction ratio (propofol: 0.58, Takizawa 2005, renal
# blood flow by para-aminohippurate; its renal metabolism is 27% of total
# clearance, the likely source of the clearance above hepatic blood flow --
# Shafer, 2026-11-13) takes the same well-stirred form as the liver, with
# renal blood flow a fixed share of cardiac output:
#
#   CL_R' / CL_R = q r / (q (1 - E_R) + r E_R)
#
# (column renal_extraction_ratio; blank means filtration, linear in r).  Only a
# drug with a row whose values were read in a published source is changed;
# every other drug ignores the sliders, and the app says which.
#
# CARDIAC OUTPUT also scales the intercompartmental clearances (Q2, Q3:
# distribution is perfusion-limited in the mammillary model) and the central
# volume, V1, in the SAME direction: a low cardiac output gives higher early
# concentrations after a bolus (less dilution in the first pass), which a
# mammillary model expresses as a smaller V1.  The exponent on V1 is
# PHYSIOLOGY_V1_CARDIAC_EXPONENT, an assumption, not a fitted value.
#
# ke0 falls slightly with cardiac output (Shafer, 2026-11-14).  ke0 is an
# equilibration rate, set by the drug's relative solubility in blood and
# brain (and, for a polar drug, by the blood-brain barrier), not by brain
# blood flow alone: were it flow, every drug acting in the brain would share
# one ke0.  But with the arterial curve held the same, a drug whose entry is
# flow-limited reaches the brain more slowly when brain flow falls: for one
# well-stirred brain, ke0 = Q (1 - exp(-PS/Q)) / (V lambda), so its ke0 falls
# in proportion to brain blood flow, whatever its solubility (the absolute
# delay, 1/ke0, is longest for the most soluble).  Brain flow follows
# cardiac output with an exponent of about 0.35 while pressure is within
# autoregulation (abstracts read 2026-11-13): Levine 1994, PMID 8026012,
# cardiac output -29.9% and middle cerebral artery velocity -15.5% (b 0.47);
# Ogoh 2005, PMID 16210355, cardiac output lowered and raised at constant
# pressure and CO2 (b about 0.35); Vlastra 2020, PMID 33068017, grey-matter
# flow by MRI +8.2% per L/min of cardiac output from 4.0 (b 0.33).  Against:
# Cook 1997, PMID 9187987, pump flow did not change cerebral flow on
# hypothermic bypass with pressure held; and Castle-Kirszbaum 2021, PMID
# 33782372, found the 41 studies too weak to confirm the coupling.  Below
# the autoregulatory floor brain flow becomes pressure-passive and the
# exponent rises toward 1, which this does not model.  So ke0 is the model's
# own at normal physiology times cardiac^PHYSIOLOGY_KE0_CARDIAC_EXPONENT,
# except a drug whose row marks its equilibration as limited by the
# blood-brain barrier (ke0_permeability_limited; morphine, Shafer,
# 2026-11-14), whose ke0 is left as it is: with PS much smaller than Q,
# ke0 = PS / (V lambda) and brain blood flow drops out.

# The GI factor scales ka only: the drug arrives faster or slower, the
# amount absorbed is unchanged.

#' Names of the physiology factors
#' @noRd
PHYSIOLOGY_FACTORS <- c("liver", "renal", "cardiac", "gi")

#' Labels of the physiology sliders
#' @noRd
PHYSIOLOGY_LABELS <- c(
  liver   = "Liver function",
  renal   = "Renal function",
  cardiac = "Cardiac output",
  gi      = "GI absorption"
)

#' Range of every physiology factor (multiples of normal function)
#' @noRd
PHYSIOLOGY_MIN <- 0.1
PHYSIOLOGY_MAX <- 10

#' Normal physiology
#' @noRd
PHYSIOLOGY_DEFAULT <- c(liver = 1, renal = 1, cardiac = 1, gi = 1)

#' Exponent of cardiac output on V1 (V1' = V1 x cardiac^exponent).  An
#' assumption: the direction is physiological (less dilution in the first
#' pass at low output), the size is not fitted.
#' @noRd
PHYSIOLOGY_V1_CARDIAC_EXPONENT <- 1

#' Exponent of cardiac output on ke0 (ke0' = ke0 x cardiac^exponent): the
#' exponent of brain blood flow on cardiac output within autoregulation,
#' about 0.35 (Levine 1994, Ogoh 2005, Vlastra 2020; see the header).
#' Shafer, 2026-11-14.
#' @noRd
PHYSIOLOGY_KE0_CARDIAC_EXPONENT <- 0.35

#' Exponent of cardiac output on the intercompartmental clearances.
#' @noRd
PHYSIOLOGY_Q_CARDIAC_EXPONENT <- 1

# The Bayesian update (R/bayesian.R) fits its clearance and volume factors at
# NORMAL physiology, and the sliders are applied to the individual afterwards
# (bayesianAtPhysiology()): the measured levels describe the patient as they
# found them, so a slider is a what-if on the fitted patient, and moving one
# does not refit (Shafer, 2026-11-16, answering CodeRabbit's question on #727;
# until then the fit was made at the sliders, so a slider acted as a prior the
# levels pulled back against).
#
# OPEN QUESTION [decision, for Shafer, 2026-11-16]: should body temperature
# also lower the intravenous drugs' clearance, and how?  Read in full so far:
# midazolam (CYP3A), CL = 18.5 (T/36.5)^4.24 L/h, 11.6%/C at 36.5 C, fitted
# over 34.8-37.8 C only (Hostler 2010, DMD 38:781); vecuronium (hepatic
# carrier uptake, not CYP), 11.3%/C over 34.3-38.0 C, its renal share
# unchanged so renal clearance fell as much (Caldwell 2000, Anesthesiology
# 92:84); remifentanil (esterases), 6.37%/C over 20-35 C (Michelsen
# 2001, Anesth Analg 93:1100, below).  Cooled
# neonates (read in full; 33.5 C then rewarmed to 36.5 C, so a linear effect
# over 33.5-36.5 C only): morphine CL x (1 + 0.0689 (T - 36.5)), hepatic and
# renal together, and its glucuronides, cleared by the kidney, x (1 + 0.0491
# (T - 36.5)) (Favie 2019, PLoS One 14:e0211910); pooled, 6.83%/C (95% CI
# 5.16-8.34) for morphine, midazolam and 1-hydroxymidazolam together, so
# UGT2B7 and CYP3A drugs shared one factor, and not identifiable for the
# renal drugs and lidocaine, whose recovery after birth was much larger
# (Favie 2020, Clin Pharmacol Ther 108:1098).  Bjelland 2013 (DMD 41:214, read in full), cooled patients after
# cardiac arrest (33.3 C) against other ICU patients (37.6 C), a group
# comparison confounded by illness (creatinine clearance 66 against 137
# mL/min): clearance lower for morphine (1201 against 1687 mL/min, about
# 7.6%/C), fentanyl (726 against 1331, about 13%/C) and propofol (2046
# against 2665, about 6%/C), not significantly for midazolam.  Leslie 1995
# (Anesth Analg 80:1007, read in full), volunteers at 34 and 37 C: propofol
# concentrations 28% higher, total clearance carried by hepatic blood flow
# (0.79 against 0.59 L/min, intervals overlapping), intercompartmental
# clearances lower.  Enzymes and chemistry, read in full: in rat liver
# slices over 7-37 C, Q10 1.76 for ethanol (alcohol dehydrogenase), 1.56
# for diazepam (CYP) and 1.24 for oxazepam (glucuronidation), i.e. 5.5, 4.4
# and 2.2%/C, the authors allowing that uptake into the slice may limit
# oxazepam (Mortensen and Dale 1995, Acta Anaesthesiol Scand 39:199); but
# in native rat liver microsomes over about 26-56 C, glucuronidation's
# activation energy is 78-84 kJ/mol, about 10%/C at 37 C (Norling and
# Hanninen 1974, BBA 350:183); general CYP activity in adult minipig liver
# microsomes 36% lower at 33 than at 38 C, 8.9%/C (Stroe 2024, Basic Clin
# Pharmacol Toxicol 135:620); atracurium in human plasma at pH 7.9 (Hofmann
# elimination and ester hydrolysis), half-life 18 min at 37 C and 49 min at
# 23 C, 7.2%/C (Merrett 1983, Br J Anaesth 55:61); remifentanil in people
# on bypass, CL x exp(theta (T - 37)), 6.37% per degree, fitted over 20-35
# C with 16 of 68 patients below 28 C, the only human fit that spans the
# slider's range (Michelsen 2001, Anesth Analg 93:1100).  Also read in
# full: CYP3A4 (ethylmorphine N-demethylation) in pig liver microsomes 69%
# of its 38 C activity at 32 C and 48% at 26 C, 6%/C on both intervals,
# while cooled piglets' fentanyl rose 25% at 31.6 C with cardiac index 41%
# lower (Fritz 2005, Anesth Analg 100:996); healthy rats cooled to 32.5 C
# against 37.9 C, clearance 83% for midazolam (CYP3A), 90% for diclofenac
# (CYP2C), 107% for dextromethorphan (CYP2D) and 63% for chlorzoxazone
# (CYP2E), i.e. 0-8.5%/C by isoform, cardiac arrest itself lowering CYP3A
# and CYP2E more than cooling did (Zhou 2011, DMD 39:2209; Tortorici 2009,
# Crit Care Med 37:263: cooling attenuated the arrest's IL-6-mediated
# suppression); patients rewarmed from 33 to 37 C, concentrations at the
# same infusion rates 16, 12 and 11% lower for remifentanil, propofol and
# midazolam over the 4 C, fentanyl unchanged (Bjelland 2014, Acta
# Anaesthesiol Scand 58:709); neonatal morphine 292 against 206 ng/mL at
# 33-34 C (Roka 2008, Pediatrics 121:e844).  The choices: one factor
# on hepatic intrinsic clearance (inside the well-stirred model) for every
# drug, or separate CYP, UGT and esterase factors; and whether to extend a
# fit made above 34.8 C down to 28 C.
#
# Body temperature and cardiac output are NOT coupled: hypothermia does not
# lower cardiac output by itself.  Fever raises cardiac output and cooling
# would by the same logic lower it, but the mechanisms differ (inflammation;
# environmental heat loss), so the clinician sets each slider (Shafer,
# 2026-11-16).

# ---- The inhaled anesthetics -------------------------------------------------
#
# The gas engine (R/advanceClosedFormGas.R) is a physiological model already:
# cardiac output and its split among the vessel-rich group (VRG, the effect
# site), the muscle group and the fat group are its parameters, so the cardiac
# slider acts on them directly and nothing is scaled by assumption.  Eger and
# Shafer's chapter ("The Evolution of Pharmacokinetics", Shafer's decision
# 2026-11-15 that the slider act on the gases) gives the physics:
#
#   * Cardiac output alone.  Less blood passes the lungs, so less agent is
#     removed and FA/FI rises faster, most for a soluble agent.  The tissues
#     each fill on their own flow.
#   * Redistribution.  "In the extreme case of hypovolemic shock, cerebral
#     blood flow is preserved ... at the expense of blood flow to everything
#     else.  In this reduced heart-brain circuit, FA/FI rises extremely fast."
#
# So the flow to the brain and heart is not scaled with cardiac output: it
# follows cardiac^PHYSIOLOGY_KE0_CARDIAC_EXPONENT, the same autoregulated
# relation of brain flow to cardiac output that sets the intravenous drugs'
# ke0 (assumed to hold for the coronary flow too).  The rest of the output is
# shared among the rest of the VRG, muscle and fat in their normal proportions,
# so the total is exactly the slider's multiple of normal and, at a tenth of
# normal, nearly all of it goes to the heart and brain.  At a high output the
# protected share shrinks and the extra flow goes to the other tissues.  The
# brain and heart are split from the rest of the VRG as a compartment of their
# own, which is then the effect site: lumped into the 6 L group, the flow the
# brain keeps would be spread over the kidneys, liver and gut, and at a tenth
# of normal output brain sevoflurane at 2 min came out a fifth of normal
# instead of near it.  The split adds a sixth state only while the slider is
# away from normal, and at normal output the two parts fill exactly as the one
# group did, so the engine's validation against GasMan is untouched.
#
# Liver, renal and GI function do not enter: the engine has no metabolism of
# the agents (their elimination is the lungs) and no gut.

#' Share of resting cardiac output that perfuses the brain and the heart
#'
#' Brain 12% and coronary 4% of resting cardiac output in the reference adult
#' man (Williams and Leggett 1989, Clin Phys Physiol Meas 10:187-217,
#' doi:10.1088/0143-0815/10/3/001, the reference values ICRP 89 adopted).
#' Read in full (2026-11-16): Table 17 gives brain 12% and heart 4% for the
#' 35-year-old reference male (12% and 5% for the female; the reference man is
#' the library's reference patient).  Must stay below the engine's VRG share
#' (\code{getGasBody()$f_brain}, 0.76).
#' @noRd
PHYSIOLOGY_GAS_PROTECTED_SHARE <- 0.12 + 0.04

#' The gas engine's body at a multiple of normal cardiac output
#'
#' Scales \code{Q_cardiac} by \code{cardiac} and redistributes it: the brain
#' and heart (\code{PHYSIOLOGY_GAS_PROTECTED_SHARE} of normal output, inside
#' the VRG) receive \code{share x cardiac^PHYSIOLOGY_KE0_CARDIAC_EXPONENT} of
#' normal output, never more than the whole output, and the rest of the VRG,
#' muscle and fat share what is left in their normal proportions.  At
#' \code{cardiac = 1} the body is returned unchanged.
#'
#' @param body output of \code{getGasBody()}
#' @param cardiac cardiac output as a multiple of normal, positive
#' @returns the body, with \code{Q_cardiac}, \code{V_brain}, \code{f_brain},
#'   \code{f_muscle} and \code{f_fat} changed, the rest of the vessel-rich
#'   group added as \code{V_vrgRest} and \code{f_vrgRest}, and \code{cardiac}
#'   recorded
#' @export
gasBodyPhysiology <- function(body, cardiac = 1)
{
  if (is.null(cardiac)) cardiac <- 1
  if (!is.numeric(cardiac) || length(cardiac) != 1 || !is.finite(cardiac) ||
      cardiac <= 0)
    stop("Invalid cardiac output multiple: ", format(cardiac))
  if (cardiac == 1) return(body)
  share <- PHYSIOLOGY_GAS_PROTECTED_SHARE
  # Normal flows as fractions of normal output.
  rest0 <- c(vrg = body$f_brain - share, muscle = body$f_muscle,
             fat = body$f_fat)
  if (rest0[["vrg"]] < 0)
    stop("The brain and heart cannot take more than the VRG's share of output")
  protected <- min(share * cardiac ^ PHYSIOLOGY_KE0_CARDIAC_EXPONENT, cardiac)
  rest <- rest0 / sum(rest0) * (cardiac - protected)
  # The brain and heart become a compartment of their own (state 3, the effect
  # site) and the rest of the vessel-rich group a sixth state
  # (gasTissueShares()), each with the share of the group's volume that it has
  # of the group's normal flow.  So at normal output the two fill exactly as
  # the one group did, and away from it the flow the brain keeps is not spread
  # over the kidneys, liver and gut.
  vrgVolume <- body$V_brain
  body$V_brain   <- vrgVolume * share / body$f_brain
  body$V_vrgRest <- vrgVolume - body$V_brain
  body$Q_cardiac <- body$Q_cardiac * cardiac
  body$f_brain   <- protected / cardiac
  body$f_vrgRest <- rest[["vrg"]] / cardiac
  body$f_muscle  <- rest[["muscle"]] / cardiac
  body$f_fat     <- rest[["fat"]] / cardiac
  body$cardiac   <- cardiac
  body
}

# ---- Body temperature (the inhaled anesthetics) ------------------------------
#
# Eger and Shafer's chapter: a fall in body temperature raises the solubility
# of the agents in blood and in every tissue, which slows induction, most for
# a soluble agent, and lowers MAC (Shafer's decision 2026-11-16 to model it).
# Two measured relations, both in humans:
#
#   * Solubility.  Zhou, Liu and Liu 2002 (Chin Med J 115:1014-9) measured the
#     blood:gas partition coefficient of desflurane, sevoflurane and isoflurane
#     in human blood from 37 to 17 C: ln(lambda) is linear in temperature, with
#     slopes -0.0302, -0.0295 and -0.0382 per C at a fixed haematocrit.  Zhou
#     and Liu 2001 (Anesth Analg 93:234-8), in human brain, heart, liver,
#     muscle and fat over the same range, found the tissue:gas coefficients
#     rise in proportion, so tissue:blood does not change.  So every partition
#     coefficient of an agent (blood:gas and each tissue:gas) is multiplied by
#     exp(-slope (T - 37)).
#   * MAC.  Liu, Hu and Liu 2001 (Anesthesiology 94:429-32, read in full):
#     isoflurane MAC in children at 37, 34 and 31 C, regression
#     MAC = 0.0784 T - 1.203, so MAC(T) / MAC(37) = (T - T0) / (37 - T0) with
#     T0 = 1.203 / 0.0784 = 15.3 C: 4.6% of the normothermic MAC per degree.
#     (The paper's text says "approximately 5.1%" and an extrapolated zero at
#     16.2 C; neither follows from its own regression, which is used here.)
#     Applied to all three volatile agents: the same 4-5% per degree was
#     measured for isoflurane and halothane in rats and goats (Vitez 1974,
#     Antognini 1993) and for desflurane in rats (Eger 1987, 42% over 10 C).
#
# Nitrous oxide: its MAC follows Antognini 1994 (Anesth Analg 79:980-2,
# PMID 7978419, read in full), the only measurement of nitrous oxide MAC
# against temperature found in any species: rats in a hyperbaric chamber,
# 1.9 +/- 0.2 atm at 37.5 C and 1.6 +/- 0.2 atm at 29.7 C, and the regression
# of its Figure 1 over all the rats, MAC = 0.03827 T + 0.3836 atm, which is
# used here as Liu's is for the volatile agents: a factor of
# (T - T0) / (37 - T0) with T0 = -0.3836 / 0.03827 = -10.0 C, so 2.1% per
# degree at 37 C and 19% lower at 28 C (the paper's Table 1: 20% over 38-28
# C), half the volatile agents' fall, as its smaller change of lipid
# solubility predicts.  Applied to people on Shafer's decision of 2026-11-16
# ("look for animal data"); no human measurement exists.  Its solubility
# follows Allott 1973 (Br J Anaesth 45:294-300, PMID 4573001, read in full),
# the review of nearly 200 temperature coefficients that Antognini and White
# and Halsey 1974 both cite: 2.33%/C in water and aqueous media (Table II,
# four coefficients; blood, saline and serum showed no difference from
# water), for blood, the vessel-rich group and muscle; and 1.13%/C in oil
# and fat (Table III, its regression's prediction, none measured), for fat.
# So tissue:blood is unchanged except in fat, which Allott's oil and water
# lines separate for an agent so little more soluble in oil than in water.  Cardiac output and
# ventilation fall with metabolism in hypothermia, but they are set by their
# own controls (the Cardiac output slider, the ventilation row), so as not to
# be counted twice.  The thresholds of the time until threshold are typed
# tensions and do not move; the summed MAC series, and its threshold in MAC,
# do.  The intravenous drugs are not changed.

#' Body temperature range and default, degrees C
#' @noRd
PHYSIOLOGY_TEMPERATURE_MIN <- 28
PHYSIOLOGY_TEMPERATURE_MAX <- 37
PHYSIOLOGY_TEMPERATURE_DEFAULT <- 37
PHYSIOLOGY_TEMPERATURE_STEP <- 0.5

#' Slope of ln(blood:gas) on temperature, per degree C (Zhou 2002)
#' @noRd
GAS_SOLUBILITY_TEMPERATURE_SLOPE <- c(desflurane = 0.0302,
                                      sevoflurane = 0.0295,
                                      isoflurane = 0.0382)

#' Nitrous oxide's temperature coefficients of solubility, per degree C at
#' 37 C (Allott 1973, Br J Anaesth 45:294-300, read in full): water and
#' aqueous media, blood among them, 2.33%/C (Table II, the mean of four
#' measured coefficients, 95% limits 0.63); oil and fat 1.13%/C (Table III,
#' predicted by the paper's own regression on solubility, no coefficient
#' having been measured).  Blood, the vessel-rich group and muscle take the
#' aqueous one, fat the oil one.
#' @noRd
GAS_N2O_SOLUBILITY_TEMPERATURE_SLOPE <- c(aqueous = 0.0233, fat = 0.0113)

#' Temperature at which the regression of Liu 2001 gives zero MAC, C
#' @noRd
GAS_MAC_ZERO_TEMPERATURE <- 1.203 / 0.0784

#' Temperature at which Antognini 1994's regression of nitrous oxide MAC in
#' rats (MAC = 0.03827 T + 0.3836 atm) gives zero MAC, C
#' @noRd
GAS_MAC_N2O_ZERO_TEMPERATURE <- -0.3836 / 0.03827

#' A body temperature, checked
#' @noRd
checkBodyTemperature <- function(temperature)
{
  if (is.null(temperature)) return(PHYSIOLOGY_TEMPERATURE_DEFAULT)
  if (!is.numeric(temperature) || length(temperature) != 1 ||
      !is.finite(temperature) || temperature < PHYSIOLOGY_TEMPERATURE_MIN ||
      temperature > PHYSIOLOGY_TEMPERATURE_MAX)
    stop("Invalid body temperature: ", format(temperature), " (",
         PHYSIOLOGY_TEMPERATURE_MIN, " to ", PHYSIOLOGY_TEMPERATURE_MAX, " C)")
  temperature
}

#' MAC at a body temperature, as a fraction of MAC at 37 C
#'
#' Liu 2001's regression in people for the volatile agents; Antognini 1994's
#' in rats for nitrous oxide.
#' @param temperature body temperature, C
#' @param agent the agent: \code{"nitrousOxide"} for nitrous oxide, anything
#'   else for a volatile agent
#' @export
gasMacTemperatureFactor <- function(temperature = PHYSIOLOGY_TEMPERATURE_DEFAULT,
                                    agent = "volatile")
{
  temperature <- checkBodyTemperature(temperature)
  zero <- if (identical(agent, "nitrousOxide")) GAS_MAC_N2O_ZERO_TEMPERATURE
          else GAS_MAC_ZERO_TEMPERATURE
  (temperature - zero) / (37 - zero)
}

#' The gas properties at a body temperature
#'
#' \code{getGasProperties()} with each volatile agent's partition coefficients
#' (blood:gas and every tissue:gas) multiplied by
#' \code{exp(-slope (T - 37))} (Zhou 2002, 2001) and its MAC by
#' \code{gasMacTemperatureFactor()} (Liu 2001); nitrous oxide's blood:gas,
#' brain and muscle coefficients by Allott 1973's aqueous coefficient, its
#' fat:gas by the oil one, and its MAC by its own factor (Antognini 1994).
#' Nitrogen and oxygen are unchanged.  At 37 C (or NULL) the table is returned as it is.
#'
#' @param temperature body temperature, C, or NULL for 37
#' @returns a data frame as \code{getGasProperties()}
#' @export
gasPropertiesAt <- function(temperature = NULL)
{
  props <- getGasProperties()
  temperature <- checkBodyTemperature(temperature)
  if (temperature == 37) return(props)
  slope <- GAS_SOLUBILITY_TEMPERATURE_SLOPE[props$gas]
  volatile <- !is.na(slope)
  f <- ifelse(volatile, exp(-slope * (temperature - 37)), 1)
  for (col in c("lambda_blood", "tg_brain", "tg_muscle", "tg_fat"))
    props[[col]] <- props[[col]] * f
  props$MAC40[volatile] <- props$MAC40[volatile] * gasMacTemperatureFactor(temperature)
  n2o <- props$gas == "nitrousOxide"
  aqueous <- exp(-GAS_N2O_SOLUBILITY_TEMPERATURE_SLOPE[["aqueous"]] * (temperature - 37))
  for (col in c("lambda_blood", "tg_brain", "tg_muscle"))
    props[[col]][n2o] <- props[[col]][n2o] * aqueous
  props$tg_fat[n2o] <- props$tg_fat[n2o] *
    exp(-GAS_N2O_SOLUBILITY_TEMPERATURE_SLOPE[["fat"]] * (temperature - 37))
  props$MAC40[n2o] <- props$MAC40[n2o] * gasMacTemperatureFactor(temperature, "nitrousOxide")
  props
}

#' The inhaled agents the cardiac slider moves (every soluble agent a user
#' doses; not the carrier gases or the ventilation)
#' @noRd
physiologyGasAgents <- function()
{
  props <- getGasProperties()
  intersect(props$gas[props$soluble], gasDrugNames())
}

#' Validate a physiology vector
#'
#' @param physiology NULL (normal), or a numeric vector named by
#'   \code{PHYSIOLOGY_FACTORS}; a factor left out is 1
#' @returns a complete named numeric vector
#' @noRd
checkPhysiology <- function(physiology)
{
  if (is.null(physiology)) return(PHYSIOLOGY_DEFAULT)
  if (is.list(physiology)) physiology <- unlist(physiology)
  if (!is.numeric(physiology) || is.null(names(physiology)) ||
      !all(names(physiology) %in% PHYSIOLOGY_FACTORS) ||
      anyDuplicated(names(physiology)))
    stop("Invalid physiology: a numeric vector named by ",
         paste(PHYSIOLOGY_FACTORS, collapse = ", "))
  for (f in names(physiology))
    if (!is_valid_number(unname(physiology[[f]]), PHYSIOLOGY_MIN, PHYSIOLOGY_MAX))
      stop("Invalid physiology: ", f, " must be between ", PHYSIOLOGY_MIN,
           " and ", PHYSIOLOGY_MAX, " (multiples of normal function)")
  out <- PHYSIOLOGY_DEFAULT
  out[names(physiology)] <- physiology
  out
}

#' Is this normal physiology?
#' @noRd
physiologyIsNormal <- function(physiology)
{
  all(checkPhysiology(physiology) == 1)
}

#' The physiology table
#'
#' One row per drug: the renal and hepatic fractions of its clearance, its
#' hepatic extraction ratio, whether its oral bioavailability carries a
#' hepatic first pass, and where each value was read.
#' @returns a data frame read from
#'   \code{inst/extdata/physiologyParameters.csv}
#' @noRd
physiologyTable <- function()
{
  if (is.null(.physiologyCache$table))
  {
    path <- system.file("extdata", "physiologyParameters.csv", package = "stanpumpR")
    .physiologyCache$table <- utils::read.csv(path, stringsAsFactors = FALSE,
                                              na.strings = c("", "NA"))
  }
  .physiologyCache$table
}

.physiologyCache <- new.env(parent = emptyenv())

#' Verification values whose rows the sliders apply
#' @noRd
PHYSIOLOGY_APPLIED_VERIFICATION <- c("read_full_text", "read_abstract")

#' A drug's physiology row, or NULL when the sliders do not apply to it
#' @noRd
physiologyRow <- function(drug)
{
  tab <- physiologyTable()
  row <- tab[tab$drug == drug & tab$verification %in% PHYSIOLOGY_APPLIED_VERIFICATION, ]
  if (nrow(row) == 0) return(NULL)
  as.list(row[1, ])
}

#' Drugs the physiology sliders change
#' @noRd
physiologyDrugs <- function()
{
  tab <- physiologyTable()
  tab$drug[tab$verification %in% PHYSIOLOGY_APPLIED_VERIFICATION]
}

#' Ratio of the new hepatic clearance to the old (well-stirred model)
#'
#' @param E hepatic extraction ratio at normal flow and capacity, in [0, 1]
#' @param flow factor on hepatic blood flow
#' @param liver factor on hepatic metabolic capacity (intrinsic clearance)
#' @noRd
hepaticClearanceFactor <- function(E, flow, liver)
{
  flow * liver / (flow * (1 - E) + liver * E)
}

#' Hepatic extraction ratio at new flow and capacity (well-stirred model)
#' @noRd
hepaticExtractionAt <- function(E, flow, liver)
{
  liver * E / (flow * (1 - E) + liver * E)
}

#' Factor on a drug's total clearance
#'
#' @param row the drug's physiology row
#' @param physiology a validated physiology vector
#' @noRd
physiologyClearanceFactor <- function(row, physiology)
{
  fr <- row$fraction_renal
  fh <- row$fraction_hepatic
  E  <- row$extraction_ratio
  ER <- row$renal_extraction_ratio
  hepatic <- if (fh > 0) hepaticClearanceFactor(E, physiology[["cardiac"]],
                                                physiology[["liver"]]) else 1
  # The well-stirred form serves the kidney too (organClearanceFactor is the
  # same function); filtration, with no renal extraction ratio, is linear.
  renal <- if (!is.null(ER) && isTRUE(is.finite(ER)))
    hepaticClearanceFactor(ER, physiology[["cardiac"]], physiology[["renal"]])
  else physiology[["renal"]]
  fr * renal + fh * hepatic + (1 - fr - fh)
}

#' Factor on oral bioavailability from the change in hepatic first pass
#'
#' F = F_abs (1 - E), so F' / F = (1 - E') / (1 - E).  The result is capped
#' so that the bioavailability never exceeds 1.
#' @noRd
physiologyFirstPassFactor <- function(row, physiology)
{
  if (!isTRUE(as.logical(row$oral_first_pass))) return(1)
  E <- row$extraction_ratio
  if (E >= 1) return(1)
  E2 <- hepaticExtractionAt(E, physiology[["cardiac"]], physiology[["liver"]])
  (1 - E2) / (1 - E)
}

#' Apply the physiology to one drug's model output
#'
#' @param X what the drug's model function returns
#' @param row the drug's physiology row
#' @param physiology a validated physiology vector
#' @param ke0 NULL, or a named list of the ke0 of each PK set at normal
#'   physiology, which each set then takes scaled by cardiac output
#' @returns X with its PK sets (and oral formulations, and transit chain)
#'   changed
#' @noRd
applyPhysiology <- function(X, row, physiology, ke0 = NULL)
{
  clFactor <- physiologyClearanceFactor(row, physiology)
  cardiac  <- physiology[["cardiac"]]
  v1Factor <- cardiac ^ PHYSIOLOGY_V1_CARDIAC_EXPONENT
  qFactor  <- cardiac ^ PHYSIOLOGY_Q_CARDIAC_EXPONENT
  fFactor  <- physiologyFirstPassFactor(row, physiology)
  gi       <- physiology[["gi"]]
  # A drug whose entry into the brain is limited by the blood-brain barrier
  # (morphine) keeps its ke0: delivery is not what limits its equilibration.
  ke0Factor <- if (isTRUE(as.logical(row$ke0_permeability_limited))) 1 else
    cardiac ^ PHYSIOLOGY_KE0_CARDIAC_EXPONENT
  scaleF <- function(f) if (is.null(f)) NULL else min(1, f * fFactor)
  scaleKa <- function(ka) if (is.null(ka)) NULL else ka * gi

  for (event in names(X$PK))
  {
    s <- X$PK[[event]]
    s$v1  <- s$v1  * v1Factor
    s$cl1 <- s$cl1 * clFactor
    if (!is.null(s$cl2)) s$cl2 <- s$cl2 * qFactor
    if (!is.null(s$cl3)) s$cl3 <- s$cl3 * qFactor
    s$ka_PO  <- scaleKa(s$ka_PO)
    s$ka_PO2 <- scaleKa(s$ka_PO2)
    # A default oral bioavailability of 1 (none given) carries no first pass
    # to change, by the table's own oral_first_pass column.
    if (!is.null(s$ka_PO) && s$ka_PO > 0 && fFactor != 1)
    {
      f <- if (is.null(s$bioavailability_PO)) 1 else s$bioavailability_PO
      s$bioavailability_PO <- scaleF(f)
    }
    if (!is.null(ke0[[event]]) && ke0[[event]] > 0)
      s$ke0Physiology <- ke0[[event]] * ke0Factor
    X$PK[[event]] <- s
  }
  if (!is.null(X$oralFormulations))
    X$oralFormulations <- lapply(X$oralFormulations, function(a) {
      a$ka_PO <- scaleKa(a$ka_PO)
      if (fFactor != 1)
        a$bioavailability_PO <- scaleF(if (is.null(a$bioavailability_PO)) 1 else a$bioavailability_PO)
      a
    })
  if (!is.null(X$transitChain$ktr)) X$transitChain$ktr <- X$transitChain$ktr * gi
  X$physiology <- list(
    factors = physiology,
    clearance = clFactor,
    v1 = v1Factor,
    intercompartmental = qFactor,
    ke0 = if (length(ke0)) ke0Factor else 1,
    bioavailability = fFactor,
    ka = gi
  )
  X
}

#' The model structures the sliders do not change
#'
#' Saturable or target-mediated elimination, parallel systems, a dose-size
#' rebuild and an INR or ACT output each carry clearance outside the PK set
#' that applyPhysiology() scales, so a row for such a drug would change only
#' part of its elimination.  test-physiology.R holds the table to this.
#' @noRd
physiologyUnsupported <- function(X)
{
  !is.null(X$michaelisMenten) || !is.null(X$parallelSystems) ||
    isTRUE(X$doseDependent) || !is.null(X$inr) || !is.null(X$act) ||
    !is.null(X$dosePhase) || !is.null(X$transitChain$metabolite)
}

#' Which dosed drugs the physiology sliders do not change
#'
#' @param drugs drug names
#' @returns the names, among \code{drugs}, that have no applied row
#' @noRd
physiologyUnaffected <- function(drugs)
{
  setdiff(unique(drugs), physiologyDrugs())
}

# ---- The sidebar panel -------------------------------------------------------

#' Input id of a physiology slider
#' @noRd
physiologyInputId <- function(factor) paste0("physiology_", factor)

#' How long the sliders must rest before the PK is resolved again (ms)
#' @noRd
PHYSIOLOGY_DEBOUNCE_MS <- 400

#' Slider step, in log10 units (about 2.3% per step)
#' @noRd
PHYSIOLOGY_SLIDER_STEP <- 0.01

#' The Physiology panel of the sidebar
#'
#' Each slider holds log10 of its factor, -1 to 1, so that it moves on a log
#' scale; app.js shows the factor itself (formatPhysiologySlider()).
#' @noRd
physiologyPanel <- function()
{
  tips <- c(
    liver = paste(
      "Hepatic metabolic capacity (intrinsic clearance), as a multiple of",
      "normal. Changes the clearance of capacity-limited drugs (low hepatic",
      "extraction ratio) nearly in proportion, and flow-limited drugs (high",
      "extraction ratio) hardly at all."),
    renal = paste(
      "Renal function, as a multiple of normal. Changes only the renal share",
      "of each drug's clearance: excretion of unchanged drug in proportion,",
      "and renal metabolism (propofol) by the same flow-limited rule as the",
      "liver."),
    cardiac = paste(
      "Cardiac output, as a multiple of normal. Liver and kidney blood flow",
      "are taken as fixed shares of it, so the clearance of flow-limited",
      "drugs follows it.",
      "It also scales the intercompartmental clearances and the central",
      "volume (a low output gives higher early concentrations), and slows",
      "equilibration with the brain (ke0) a little, as brain blood flow",
      "follows cardiac output only partly.",
      "For the inhaled anesthetics it sets the blood flow through the lungs",
      "and the tissues, the brain and heart keeping most of theirs, so a low",
      "output makes the end-tidal and brain concentrations rise faster."),
    gi = paste(
      "Gastrointestinal absorption rate (oral ka), as a multiple of normal.",
      "The drug arrives faster or slower; the amount absorbed is unchanged.")
  )
  sliders <- lapply(PHYSIOLOGY_FACTORS, function(f)
    fieldHelp(
      sliderInput(
        inputId = physiologyInputId(f),
        label = PHYSIOLOGY_LABELS[[f]],
        min = log10(PHYSIOLOGY_MIN),
        max = log10(PHYSIOLOGY_MAX),
        value = 0,
        step = PHYSIOLOGY_SLIDER_STEP,
        ticks = FALSE
      ) |> htmltools::tagAppendAttributes(class = "physiology-slider"),
      tips[[f]],
      placement = "right"
    )
  )
  bslib::accordion_panel(
    "Physiology",
    value = "physiology",
    icon = icon("heart-pulse"),
    tags$p(class = "small text-muted",
           "What if? Each slider is a multiple of normal function, from a",
           "tenth to ten times, on a log scale."),
    sliders,
    fieldHelp(
      sliderInput(
        inputId = "physiology_temperature",
        label = "Body temperature (\u00b0C)",
        min = PHYSIOLOGY_TEMPERATURE_MIN,
        max = PHYSIOLOGY_TEMPERATURE_MAX,
        value = PHYSIOLOGY_TEMPERATURE_DEFAULT,
        step = PHYSIOLOGY_TEMPERATURE_STEP,
        ticks = FALSE
      ),
      paste("Core temperature. For the inhaled anesthetics, hypothermia",
            "raises their solubility in blood and tissue, which slows the rise",
            "of the end-tidal and brain concentrations, and lowers their MAC,",
            "about 4.6% a degree. It does not change the intravenous drugs,",
            "nor cardiac output or ventilation, which have their own controls."),
      placement = "right"
    ),
    actionLink("physiologyReset", "Reset to normal"),
    uiOutput("physiologyNote")
  )
}

#' Factors from the slider inputs (log10 values)
#'
#' @param values a list of the four inputs' values, named by factor; a NULL
#'   (not yet reported) is normal
#' @noRd
physiologyFromInputs <- function(values)
{
  out <- PHYSIOLOGY_DEFAULT
  for (f in PHYSIOLOGY_FACTORS)
  {
    v <- values[[f]]
    if (is.numeric(v) && length(v) == 1 && is.finite(v))
      out[[f]] <- 10 ^ max(log10(PHYSIOLOGY_MIN), min(log10(PHYSIOLOGY_MAX), v))
  }
  # A slider at 0 is exactly normal, not 10^0 rounded.
  out[abs(log10(out)) < PHYSIOLOGY_SLIDER_STEP / 2] <- 1
  out
}

#' The physiology sliders as rows of the email workbook's Covariates sheet
#'
#' One row per factor, as a multiple of normal; NULL (an export made without
#' them) is normal.
#' @param physiology the factors, or NULL
#' @param temperature the body temperature, C, or NULL (37)
#' @returns a data frame of Covariate and Value
#' @noRd
physiologyCovariateRows <- function(physiology, temperature = NULL)
{
  p <- tryCatch(checkPhysiology(physiology), error = function(e) NULL)
  value <- if (is.null(p)) rep("unreadable", length(PHYSIOLOGY_FACTORS)) else
    ifelse(p[PHYSIOLOGY_FACTORS] == 1, "normal",
           paste0("x", trimws(formatC(p[PHYSIOLOGY_FACTORS], digits = 3, format = "fg"))))
  t <- tryCatch(checkBodyTemperature(temperature), error = function(e) NULL)
  data.frame(Covariate = c(paste(PHYSIOLOGY_LABELS[PHYSIOLOGY_FACTORS],
                                 "(multiple of normal)"),
                           "Body temperature (C)"),
             Value = c(unname(value), if (is.null(t)) "unreadable" else format(t)),
             stringsAsFactors = FALSE)
}

#' The body temperature from its slider: 37 C unless a readable value in range
#' @noRd
bodyTemperatureFromInput <- function(value)
{
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value))
    return(PHYSIOLOGY_TEMPERATURE_DEFAULT)
  max(PHYSIOLOGY_TEMPERATURE_MIN, min(PHYSIOLOGY_TEMPERATURE_MAX, value))
}

#' A short description of non-normal physiology ("liver x0.5, cardiac x2")
#' @noRd
physiologySummary <- function(physiology)
{
  physiology <- checkPhysiology(physiology)
  changed <- physiology[physiology != 1]
  if (length(changed) == 0) return("")
  paste(sprintf("%s ×%s", tolower(PHYSIOLOGY_LABELS[names(changed)]),
                formatC(changed, digits = 2, format = "fg")),
        collapse = ", ")
}

#' What the Physiology panel says under its sliders
#'
#' @param physiology the factors
#' @param drugs the dosed drugs
#' @noRd
physiologyNote <- function(physiology, drugs,
                           temperature = PHYSIOLOGY_TEMPERATURE_DEFAULT)
{
  cold <- !is.null(temperature) && temperature != PHYSIOLOGY_TEMPERATURE_DEFAULT
  if ((physiologyIsNormal(physiology) && !cold) || length(drugs) == 0) return(NULL)
  drugs <- unique(drugs)
  # The carrier gases and the ventilation are settings, not drugs.  The agents
  # respond to cardiac output (gasBodyPhysiology()) and, the volatile ones, to
  # body temperature (gasPropertiesAt()); the intravenous drugs to the four
  # sliders only.
  gasAgents <- intersect(drugs, physiologyGasAgents())
  drugs <- drugs[!isGasDrug(drugs)]
  factorsMoved <- !physiologyIsNormal(physiology)
  cardiacMoved <- checkPhysiology(physiology)[["cardiac"]] != 1
  # Every agent a user doses responds to temperature: the volatile agents'
  # solubility and MAC, nitrous oxide's MAC
  volatile <- gasAgents %in% c(names(GAS_SOLUBILITY_TEMPERATURE_SLOPE), "nitrousOxide")
  changed <- c(if (factorsMoved) intersect(drugs, physiologyDrugs()),
               gasAgents[cardiacMoved | (cold & volatile)])
  noData <- if (factorsMoved) setdiff(drugs, physiologyDrugs()) else character(0)
  notByTemperature <- if (cold && !factorsMoved) drugs else character(0)
  notByTemperature <- c(notByTemperature,
                        if (cold && !cardiacMoved) gasAgents[!volatile])
  tags$div(
    class = "small physiology-note",
    if (length(changed))
      tags$p(tags$strong("Changed: "), paste(changed, collapse = ", ")),
    if (length(noData))
      tags$p(class = "text-warning",
             tags$strong("Not changed (no physiology data yet): "),
             paste(noData, collapse = ", ")),
    if (length(notByTemperature))
      tags$p(class = "text-muted",
             tags$strong("Not changed by body temperature: "),
             paste(notByTemperature, collapse = ", "),
             " (it acts on the inhaled agents only)."),
    if (length(gasAgents) && !cardiacMoved && !cold)
      tags$p(class = "text-muted",
             tags$strong("Inhaled agents "), "(", paste(gasAgents, collapse = ", "),
             ") respond to cardiac output and body temperature only.")
  )
}

#' The generated help block `physiology-table`: every row of the physiology
#' table, applied first, with its sources.
#' @noRd
helpPhysiologyTableMarkdown <- function()
{
  tab <- physiologyTable()
  applied <- tab$verification %in% PHYSIOLOGY_APPLIED_VERIFICATION
  tab <- tab[order(!applied, tab$drug), ]
  num <- function(x) ifelse(is.na(x), "—", formatC(x, digits = 3, format = "fg"))
  cell <- function(x) gsub("|", "\\|", ifelse(is.na(x), "", x), fixed = TRUE)
  rows <- sprintf(
    "| [%s](help:drugs/%s) | %s | %s | %s | %s | %s | %s |",
    tab$drug, tab$drug, num(tab$fraction_renal), num(tab$fraction_hepatic),
    num(tab$extraction_ratio),
    ifelse(tab$verification %in% PHYSIOLOGY_APPLIED_VERIFICATION, "yes", "not yet"),
    cell(paste(tab$source_renal, tab$source_hepatic, tab$source_extraction, sep = "; ")),
    cell(tab$notes)
  )
  paste(c(
    "| Drug | Renal share | Hepatic share | Extraction ratio | Applied | Sources | Notes |",
    "|---|---|---|---|---|---|---|",
    rows
  ), collapse = "\n")
}

# ---- The ke0 rationale: a brain fed by a fixed arterial curve -----------------
#
# For the help page (generated blocks physiology-ke0-figure,
# physiology-ke0-table, physiology-propofol-table).  The brain is one
# well-stirred tissue fed by arterial blood (Renkin-Crone):
#   V dCb/dt = Q E (Ca - Cb/lambda),  E = 1 - exp(-PS/Q),  Ce = Cb/lambda,
# which is the ke0 equation dCe/dt = ke0 (Ca - Ce) with
# ke0 = Q E / (V lambda).  Holding Ca(t) fixed and lowering Q shows how the
# effect site slows when less drug is delivered each minute.

#' Brain volume (L) and normal brain blood flow (L/min) for the illustration
#' @noRd
PHYSIOLOGY_BRAIN_VOLUME <- 1.4
PHYSIOLOGY_BRAIN_FLOW <- 0.75

#' The illustrative drugs: ke0 at normal flow (/min) and PS (L/min)
#' @noRd
physiologyKe0Drugs <- function()
{
  d <- data.frame(
    drug = c("Fast, flow-limited (ke0 0.5/min)",
             "Slow, flow-limited, highly soluble (ke0 0.05/min)",
             "Slow, permeability-limited, polar (ke0 0.05/min)"),
    ke0 = c(0.5, 0.05, 0.05),
    PS  = c(50, 50, 0.05),
    stringsAsFactors = FALSE)
  E0 <- 1 - exp(-d$PS / PHYSIOLOGY_BRAIN_FLOW)
  d$lambda <- PHYSIOLOGY_BRAIN_FLOW * E0 / (PHYSIOLOGY_BRAIN_VOLUME * d$ke0)
  d
}

#' ke0 of a well-stirred brain at a fraction of normal brain blood flow
#'
#' @param PS permeability-surface product (L/min)
#' @param lambda brain:blood partition coefficient
#' @param flow brain blood flow, fraction of normal
#' @noRd
brainKe0 <- function(PS, lambda, flow)
{
  Q <- PHYSIOLOGY_BRAIN_FLOW * flow
  Q * (1 - exp(-PS / Q)) / (PHYSIOLOGY_BRAIN_VOLUME * lambda)
}

#' The fixed arterial curve of the illustration (a bolus-like profile)
#' @noRd
physiologyArterialCurve <- function(t)
{
  s <- pmax(t - 0.5, 0)
  ifelse(t < 0.5, 0,
         10 * (0.85 * exp(-1.2 * s) + 0.15 * exp(-0.05 * s)) * (1 - exp(-6 * s)))
}

#' Effect-site concentration for an arterial curve, exact for a curve linear
#' between the times given
#' @noRd
effectSiteForArterial <- function(times, arterial, ke0)
{
  ce <- numeric(length(times))
  for (j in seq_along(times)[-1])
  {
    dt <- times[j] - times[j - 1]
    e <- exp(-ke0 * dt)
    slope <- (arterial[j] - arterial[j - 1]) / dt
    ce[j] <- ce[j - 1] * e + arterial[j - 1] * (1 - e) + slope * (dt - (1 - e) / ke0)
  }
  ce
}

#' Brain-flow fractions shown in the illustration, with their colours
#' @noRd
PHYSIOLOGY_KE0_FLOWS <- c(1, 0.7, 0.5, 0.3)
PHYSIOLOGY_KE0_COLOURS <- c("#2c3e50", "#2980b9", "#e67e22", "#c0392b")

#' One small SVG line chart for the help page
#' @noRd
physiologySvgChart <- function(x, ys, colours, xr, yr, title, xlab, ylab, dashed = NULL)
{
  W <- 640; H <- 250; left <- 52; right <- 20; top <- 26; bottom <- 40
  sx <- function(v) left + (v - xr[1]) / diff(xr) * (W - left - right)
  sy <- function(v) H - bottom - (pmin(pmax(v, yr[1]), yr[2]) - yr[1]) / diff(yr) * (H - top - bottom)
  f <- function(v) sprintf("%.1f", v)
  path <- function(y) paste0("M", paste(f(sx(x)), f(sy(y)), sep = ",", collapse = " L"))
  xTicks <- pretty(xr); xTicks <- xTicks[xTicks >= xr[1] & xTicks <= xr[2]]
  yTicks <- pretty(yr, n = 4); yTicks <- yTicks[yTicks >= yr[1] & yTicks <= yr[2]]
  c(
    sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s" font-size="12" font-family="inherit" style="width:100%%;max-width:640px;height:auto">',
            W, H, title),
    sprintf('<text x="%s" y="16" font-weight="bold">%s</text>', f(left), title),
    sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#e3e8ec"/>',
            f(sx(xr[1])), f(sy(yTicks)), f(sx(xr[2])), f(sy(yTicks))),
    sprintf('<text x="%s" y="%s" text-anchor="end" dominant-baseline="middle">%s</text>',
            f(sx(xr[1]) - 6), f(sy(yTicks)), format(yTicks)),
    sprintf('<text x="%s" y="%s" text-anchor="middle">%s</text>',
            f(sx(xTicks)), f(sy(yr[1]) + 16), format(xTicks)),
    if (!is.null(dashed))
      sprintf('<path d="%s" fill="none" stroke="#95a5a6" stroke-width="1.5" stroke-dasharray="5,4"/>', path(dashed)),
    vapply(seq_along(ys), function(i)
      sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="2.2"/>', path(ys[[i]]), colours[i]),
      character(1)),
    sprintf('<text x="%s" y="%s" text-anchor="middle">%s</text>', f((sx(xr[1]) + sx(xr[2])) / 2), f(H - 6), xlab),
    sprintf('<text transform="translate(13,%s) rotate(-90)" text-anchor="middle">%s</text>',
            f((sy(yr[1]) + sy(yr[2])) / 2), ylab),
    "</svg>")
}

#' Generated block physiology-ke0-figure: effect-site curves for one fixed
#' arterial curve at four brain blood flows, for three kinds of drug
#' @noRd
helpPhysiologyKe0Figure <- function()
{
  d <- physiologyKe0Drugs()
  times <- seq(0, 30, by = 0.02)
  ca <- physiologyArterialCurve(times)
  keep <- times <= 20
  panels <- lapply(seq_len(nrow(d)), function(i) {
    ys <- lapply(PHYSIOLOGY_KE0_FLOWS, function(q)
      effectSiteForArterial(times, ca, brainKe0(d$PS[i], d$lambda[i], q))[keep])
    top <- if (i == 1) 6.5 else 1.5
    physiologySvgChart(times[keep], ys, PHYSIOLOGY_KE0_COLOURS, c(0, 20), c(0, top),
                       d$drug[i], "Minutes", "Effect site",
                       dashed = if (i == 1) ca[keep] else NULL)
  })
  legend <- paste(sprintf('<span style="color:%s">&#9644; brain flow &#215;%s</span>',
                          PHYSIOLOGY_KE0_COLOURS, format(PHYSIOLOGY_KE0_FLOWS)),
                  collapse = " &nbsp; ")
  paste(c('<figure class="help-figure">', unlist(panels),
          sprintf('<figcaption>%s<br>The same arterial curve (dashed, top panel; off scale in the lower two) feeds every line. Only brain blood flow changes.</figcaption>', legend),
          "</figure>"), collapse = "\n")
}

#' Generated block physiology-ke0-table: equilibration half-times by brain
#' flow, and ke0 by cardiac output through the exponent
#' @noRd
helpPhysiologyKe0Table <- function()
{
  d <- physiologyKe0Drugs()
  rows <- vapply(seq_len(nrow(d)), function(i) {
    t50 <- vapply(PHYSIOLOGY_KE0_FLOWS, function(q)
      log(2) / brainKe0(d$PS[i], d$lambda[i], q), numeric(1))
    sprintf("| %s | %s |", d$drug[i], paste(helpFormatNumber(t50), collapse = " | "))
  }, character(1))
  co <- c(1, 0.7, 0.5, 0.3, 0.1)
  paste(c(
    "Minutes for the effect site to reach half of a step in arterial concentration:",
    "",
    sprintf("| Drug | %s |", paste(sprintf("Brain flow &#215;%s", format(PHYSIOLOGY_KE0_FLOWS)), collapse = " | ")),
    paste0("|---", strrep("|---", length(PHYSIOLOGY_KE0_FLOWS)), "|"),
    rows,
    "",
    sprintf("ke0 as stanpumpR applies it, ke0 &#215; CO<sup>%s</sup>:", PHYSIOLOGY_KE0_CARDIAC_EXPONENT),
    "",
    sprintf("| Cardiac output | %s |", paste(sprintf("&#215;%s", format(co)), collapse = " | ")),
    paste0("|---", strrep("|---", length(co)), "|"),
    sprintf("| ke0, fraction of normal | %s |",
            paste(helpFormatNumber(co ^ PHYSIOLOGY_KE0_CARDIAC_EXPONENT), collapse = " | "))
  ), collapse = "\n")
}

#' Generated block physiology-propofol-table: a 2 mg/kg propofol bolus at
#' three cardiac outputs, simulated by the app's own code
#' @noRd
helpPhysiologyPropofolTable <- function()
{
  if (is.null(physiologyRow("propofol"))) return("")
  dose <- data.frame(Drug = "propofol", Time = 0, Dose = 2, Units = "mg/kg",
                     stringsAsFactors = FALSE)
  ev <- data.frame(Time = numeric(0), Event = character(0), stringsAsFactors = FALSE)
  rows <- vapply(c(1, 0.5, 0.3), function(co) {
    p <- c(cardiac = co)
    r <- simulateDrugsWithCovariates(dose, ev, 70, 170, 40, "male", 30, FALSE,
                                     physiology = p)$propofol$results
    e <- r[r$Site == "Effect Site", ]
    pl <- r[r$Site == "Plasma", ]
    pk <- getDrugPK("propofol", 70, 170, 40, "male", physiology = p)
    sprintf("| &#215;%s | %s | %s | %s | %s |", format(co),
            helpFormatNumber(pk$PK$default$ke0),
            helpFormatNumber(stats::approx(pl$Time, pl$Y, 1)$y),
            helpFormatNumber(max(e$Y)),
            helpFormatNumber(e$Time[which.max(e$Y)]))
  }, character(1))
  paste(c(
    "| Cardiac output | ke0 (/min) | Plasma at 1 min (mcg/mL) | Peak effect site (mcg/mL) | Time of peak (min) |",
    "|---|---|---|---|---|",
    rows
  ), collapse = "\n")
}

#' The generated help block `physiology-gas-table`: sevoflurane and nitrous
#' oxide at several cardiac outputs, end-tidal (FA/FI) and brain, simulated by
#' the gas engine itself.
#' @noRd
helpPhysiologyGasTable <- function()
{
  if (!is.null(.physiologyCache$gasTable)) return(.physiologyCache$gasTable)
  runs <- list(
    sevoflurane = data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"),
                             Time = 0, Dose = c(2, 3, 3, 6)),
    # Nitrous oxide is a flow: 4 L/min of it in 2 L/min of oxygen.
    nitrousOxide = data.frame(Drug = c("nitrousOxide", "oxygen", "ventilation"),
                              Time = 0, Dose = c(4, 2, 6)))
  label <- c(sevoflurane = "Sevoflurane 2%", nitrousOxide = "Nitrous oxide 67%")
  inspired <- c(sevoflurane = 2, nitrousOxide = 100 * 4 / 6)
  rows <- character(0)
  for (g in names(runs)) for (co in c(0.25, 0.5, 1, 2)) {
    sim <- simulateGases(runs[[g]], weight = 70, age = 40, maximum = 10,
                         physiology = c(cardiac = co))
    r <- sim$results[sim$results$Drug == g, ]
    at <- function(site, t)
      stats::approx(r$Time[r$Site == site], r$Y[r$Site == site], t)$y
    rows <- c(rows, sprintf("| %s | &#215;%s | %s | %s | %s | %s |",
                            label[[g]], format(co),
                            formatC(at("Alveolar", 2) / inspired[[g]], digits = 2, format = "f"),
                            formatC(at("Alveolar", 10) / inspired[[g]], digits = 2, format = "f"),
                            helpFormatNumber(at("Brain", 2)),
                            helpFormatNumber(at("Brain", 10))))
  }
  .physiologyCache$gasTable <- paste(c(
    "| Agent (inspired) | Cardiac output | FA/FI at 2 min | FA/FI at 10 min | Brain at 2 min (%) | Brain at 10 min (%) |",
    "|---|---|---|---|---|---|",
    rows
  ), collapse = "\n")
}

#' The generated help block `physiology-temperature-table`: the volatile agents
#' at several body temperatures, simulated by the gas engine itself.
#' @noRd
helpPhysiologyTemperatureTable <- function()
{
  if (!is.null(.physiologyCache$temperatureTable)) return(.physiologyCache$temperatureTable)
  # Nitrous oxide is a flow: 4 L/min of it in 2 L/min of oxygen, 67% inspired
  agents <- c(isoflurane = 1.2, sevoflurane = 2, desflurane = 6, nitrousOxide = 100 * 4 / 6)
  label <- c(isoflurane = "Isoflurane 1.2%", sevoflurane = "Sevoflurane 2%",
             desflurane = "Desflurane 6%", nitrousOxide = "Nitrous oxide 67%")
  rows <- character(0)
  for (g in names(agents)) for (T in c(37, 34, 31, 28)) {
    dt <- if (g == "nitrousOxide")
      data.frame(Drug = c(g, "oxygen", "ventilation"), Time = 0, Dose = c(4, 2, 6))
    else
      data.frame(Drug = c(g, "oxygen", "air", "ventilation"), Time = 0,
                 Dose = c(agents[[g]], 3, 3, 6))
    sim <- simulateGases(dt, weight = 70, age = 40, maximum = 10, temperature = T)
    r <- sim$results
    at <- function(site, t) {
      x <- r[r$Drug == g & r$Site == site, ]
      stats::approx(x$Time, x$Y, t)$y
    }
    props <- gasPropertiesAt(T)
    mac <- macForAge(props$MAC40[props$gas == g], 40)
    rows <- c(rows, sprintf("| %s | %s | %s | %s | %s | %s |",
                            label[[g]], format(T),
                            helpFormatNumber(props$lambda_blood[props$gas == g]),
                            helpFormatNumber(mac),
                            formatC(at("Alveolar", 2) / agents[[g]], digits = 2, format = "f"),
                            formatC(at("Brain", 10) / mac, digits = 2, format = "f")))
  }
  .physiologyCache$temperatureTable <- paste(c(
    "| Agent (inspired) | Temperature (\u00b0C) | Blood:gas | MAC (%) | FA/FI at 2 min | Brain at 10 min (MAC) |",
    "|---|---|---|---|---|---|",
    rows
  ), collapse = "\n")
}
