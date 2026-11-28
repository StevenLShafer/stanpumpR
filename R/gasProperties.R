# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer, as the parameter layer for the inhaled-gas engine.
#
# Structure and parameter values follow the GasMan(R) model of James H. Philip
# as described in the peer-reviewed literature (Philip is a co-author of the
# first reference below).  GasMan's source is published under the GPL-3.0 at
# github.com/rasman/gasmanonline (its API, and its desktop and web
# application; commit d3a2dd3 was the one compared).  The values here are the
# literature's; they agree with that source's gasman.ini
# (inst/validation/VALIDATION.md), and the cardiac output and ventilation
# scaling are read from its GasDoc.cpp, as noted in getGasBody().  Until
# 2026-11-23 this said GasMan was closed source (ChatGPT's gas-engine audit of
# 2026-11-23).
#
#   Weber J, Schmidt J, Wirth S, Schumann S, Philip JH, Eberhart LHJ.
#   Context-sensitive decrement times for inhaled anesthetics in obese patients
#   explored with GasMan(R).  J Clin Monit Comput.  PMC7943506.
#     -> "a flow-limited four-compartment mammillary model (alveolar gas, the
#        vessel-rich group, muscle group and fat group)"; tissue volumes for the
#        70 kg standard (VRG 6 L, muscle 33 L, fat 14.5 L); cardiac output
#        5.0-6.03 L/min; partition coefficients for blood, brain, muscle, fat.
#
#   Hendrickx JFA, Lemmens HJM, Shafer SL.  Do distribution volumes and
#   clearances relate to tissue volumes and blood flows?  A computer simulation.
#   PMC1508141.  -> same four patient compartments.
#
# STATUS: the ODE structure has been verified numerically (see
# tests/testthat/test-gas-engine.R -- closed form vs. RK4, mass balance, and
# analytic single-compartment limits).  The partition coefficients below for
# sevoflurane, isoflurane and desflurane equal GasMan's own (Weber et al.
# 2021, J Clin Monit Comput 35:343-354, Table 1).  Their MAC at 40 is not
# GasMan's since 2026-11-23: it is Mapleson 1996's (MAC40 and GASMAN_MAC40
# below; ChatGPT's round-2 gas audit, A-01, found this sentence saying both
# were GasMan's).  Nitrous oxide and nitrogen are not in that table.  The circuit volume is marked
# TODO(fixture) in getGasBody().
# -----------------------------------------------------------------------------


#' Physical properties of the inhaled gases
#'
#' One row per gas.  All partition coefficients are dimensionless ratios at
#' 37 degrees C.
#'
#' Columns:
#' \describe{
#'   \item{gas}{name, matching the dose-table drug name}
#'   \item{soluble}{TRUE for gases carried by blood into the tissue
#'     compartments (nitrous oxide, the volatiles, nitrogen); FALSE for oxygen,
#'     which is modelled in the gas phase only with a metabolic sink -- it binds
#'     haemoglobin nonlinearly and has no meaningful partition coefficient.}
#'   \item{lambda_blood}{blood:gas partition coefficient}
#'   \item{tg_brain, tg_muscle, tg_fat}{tissue:GAS partition coefficients, as
#'     gasman.ini gives them (VRG, MUS, FAT).  These are what the equations
#'     need; they are NOT tissue:blood and must not be multiplied by
#'     \code{lambda_blood} again -- see \code{gasPartitionTissueGas()}.}
#'   \item{MAC40}{minimum alveolar concentration at age 40, in % of 1 atm.
#'     NA for gases with no anaesthetic potency in this context.}
#'   \item{potent}{TRUE if the gas contributes to the MAC sum}
#'   \item{flagged, flagNote}{provenance: TRUE, with a note, for a value used
#'     as GasMan states it although known to be wrong; see
#'     \code{flaggedGasParameters()}}
#' }
#'
#' The vessel-rich group is parameterised with the BRAIN partition coefficient,
#' which is how Weber et al. tabulate GasMan's parameters, and is why the VRG
#' tension is reported to the user as "brain".
#'
#' @returns a data frame of gas properties
#' @export
getGasProperties <- function()
{
  data.frame(
    gas     = c("nitrousOxide", "sevoflurane", "isoflurane", "desflurane",
                "nitrogen", "oxygen"),
    soluble = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),

    # Blood:gas ("Lambda" in gasman.ini)
    lambda_blood = c(0.47, 0.65, 1.3, 0.42, 0.014, NA),

    # Tissue:GAS ("VRG" / "MUS" / "FAT" in gasman.ini).  Note these are
    # tissue:gas, NOT tissue:blood: desflurane's VRG of 0.54 is its blood:gas
    # 0.42 times a brain:blood of about 1.3.  An earlier version of this file
    # stored tissue:blood and multiplied, which was one conversion too many.
    tg_brain  = c(0.42, 1.1, 2.1, 0.54, 0.010, NA),
    tg_muscle = c(0.54, 2.4, 4.5, 0.97, 0.014, NA),
    tg_fat    = c(1.08, 34,  70,  13,   0.070, NA),

    # MAC, % of 1 atm, at the reference age of 40: Mapleson 1996's global
    # values (Br J Anaesth 76:179-185, abstract and Table 2;
    # papers/digests/PMID_8777094.md), the same meta-analysis whose age slope
    # macForAge() applies, so slope and intercepts are one fit.  Shafer's
    # decision of 2026-11-23, from ChatGPT's gas-engine audit (B-02): "We
    # validated against GasMan initially, but are now moving to more recent
    # research."  Until then these were GasMan's (GASMAN_MAC40 below), which
    # are also the US labels' (Ultane 2.1% at 40, Forane 1.15% at 31-55,
    # Suprane 6.0% at 45); at a given tension Mapleson's raise sevoflurane's
    # MAC-equivalents by 16.7% and nitrous oxide's by 5.8%, and lower
    # isoflurane's by 6.0% and desflurane's by 9.1%.  MAC enters no uptake,
    # so the GasMan validation, which compares tensions, is untouched.
    # Nitrogen's 200 is GasMan's own figure, recorded as it stands and
    # flagged below rather than corrected or omitted; it is inert because
    # nitrogen is not summed into MAC.
    MAC40 = c(104, 1.80, 1.17, 6.6, 200, NA),

    # Whether the agent contributes to the summed MAC.  gasman.ini gives
    # nitrogen a MAC of 200, presumably for hyperbaric completeness, but
    # including it would post 0.4 MAC on room air, so it is excluded here.
    # Flagged rather than assumed -- see the note in the header.
    potent = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),

    # Provenance flags.  A value is used as GasMan states it, so that
    # validation compares like with like, but is marked when its provenance is
    # known to be wrong.  FALSE means "not yet checked", not "verified".
    flagged = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    flagNote = c(NA, NA, NA, NA,
                 paste("MAC 200 (2 atm) contradicts Eger's estimate of 110 atm",
                       "(Koblin, Eger et al., Anesth Analg 1998;87:419-424, rats),",
                       "i.e. 11000% of one atmosphere: low by about 55-fold.",
                       "Room air is 0.79 atm nitrogen, so the true contribution",
                       "is about 0.007 MAC, not the 0.4 MAC this figure implies."),
                 NA),

    stringsAsFactors = FALSE
  )
}


#' GasMan's MAC at 40, superseded
#'
#' The MAC of each potent agent in GasMan's gasman.ini, which stanpumpR used
#' until 2026-11-23, when Shafer moved to Mapleson 1996's (getGasProperties()
#' MAC40).  Kept as the record of what the engine was validated with, and for
#' a comparison with GasMan's own MAC output.
#' @noRd
GASMAN_MAC40 <- c(nitrousOxide = 110, sevoflurane = 2.1, isoflurane = 1.1,
                  desflurane = 6.0)


#' Parameters whose provenance is known to be wrong
#'
#' GasMan's values are used as they stand so that validation compares like with
#' like, but the ones known to be wrong are flagged rather than silently
#' corrected.  An empty result does not mean the table has been verified; it
#' means nothing further has been checked yet.
#'
#' @returns a data frame of the flagged gases and the reason
#' @export
flaggedGasParameters <- function()
{
  props <- getGasProperties()
  props[props$flagged, c("gas", "MAC40", "flagNote")]
}


#' The agents that contribute to summed MAC
#'
#' @returns character vector of gas names
#' @export
potentAgents <- function()
{
  props <- getGasProperties()
  props$gas[props$potent]
}


#' Tissue:gas partition coefficients for one gas
#'
#' The differential equations are written in gas tensions, so the capacity of a
#' tissue is its volume times its tissue:GAS partition coefficient.  That is
#' what \code{getGasProperties()} stores, so this is a selection, not a
#' conversion: no factor of \code{lambda_blood} is applied.
#'
#' @param props one row of \code{getGasProperties()}
#' @returns named numeric vector: brain, muscle, fat (tissue:gas)
#' @export
gasPartitionTissueGas <- function(props)
{
  c(
    brain  = props$tg_brain,
    muscle = props$tg_muscle,
    fat    = props$tg_fat
  )
}


#' Body and breathing-circuit geometry for the inhaled-gas model
#'
#' Tissue volumes and cardiac output scale with body size from the 70 kg
#' standard, by body composition when the patient's height, age and sex are
#' given and \code{adjustToFFM} is TRUE (below), otherwise linearly with
#' weight as GasMan does.  Circuit volume is a property of the anaesthesia
#' machine, not the patient, so it does not scale.
#'
#' BODY COMPOSITION (Shafer, 2026-11-25, built the same day).  With the
#' library's fat-free mass (\code{ffmAlSallami()}), FFM, and the fat mass
#' FM = weight - FFM, each over the reference man's (70 kg, 170 cm, 35
#' years: FFM 54.48 kg, FM 15.52 kg; \code{FFM_REFERENCE}):
#' \itemize{
#'   \item the vessel-rich group, the muscle group and the alveolar volume
#'     (functional residual capacity) scale with FFM;
#'   \item the fat group's volume scales with FM;
#'   \item the vessel-rich and muscle groups' blood flows scale with FFM to the
#'     0.75 (the clearance exponent of \code{pkSizeFactors()}), and the fat
#'     group's flow with its volume to the 0.75, Weber 2021's Equation 1
#'     (excess fat perfused at the standard fat's flow scaled allometrically);
#'   \item cardiac output is the sum of the three flows, and each group's
#'     share of it follows.
#' }
#' At the reference man this is GasMan's standard exactly (6.0, 33.0 and 14.5
#' L; 76, 18 and 6\% of 5 L/min).  Fat added to him leaves the lean volumes,
#' their flows and the lungs as they were and raises cardiac output by the
#' fat's flow alone, as Weber 2021 built its obese GasMan models (Table 2: at
#' 100 kg, 44.87 L of fat; Equation 1 gives 0.70 L/min where its table, with
#' its shares rounded to whole per cent, prints 0.77).  Weber kept the lean
#' volumes unchanged and so does this; it did not scale a child, which the
#' lean terms here do.  Oxygen consumption stays 3.5 mL/kg/min of total
#' weight, and the default ventilation stays allometric in weight
#' (\code{defaultGasVentilation()}): neither is a compartment.  With
#' \code{adjustToFFM} FALSE (the Patient Profile's fat-free-mass switch off),
#' or without height, age or sex, the scaling is GasMan's, which the
#' validation against GasMan at other weights uses.
#'
#' Cardiac output is GasMan's default: 5 L/min at 70 kg (gasman.ini
#' \code{[Defaults] CO=5}), scaled allometrically by \code{(weight / 70)^0.75},
#' as the desktop GasMan does when the user changes the weight.  GasMan's API
#' does not: \code{InitDocument} computes the factor at 70 kg and
#' \code{SetWeight} leaves the defaults alone, so the validation scenarios
#' give GasMan both flows (ChatGPT's round-2 gas audit, A-02).  Adopted 2026-10-05 at Shafer's direction ("for
#' now I want to be identical with GasMan; later on we will update with more
#' current data"), replacing the linear 75 mL/kg (5.25 L/min at 70 kg) he chose
#' on 2026-09-02.  Tissue and alveolar volumes still scale linearly with weight,
#' which is also what GasMan does (\code{fWtFactor = fWeight / STD_WEIGHT}).
#' DECIDED (Shafer, 2026-11-25; Gemini's gas-engine audit, B-01 and F-01):
#' the compartments are to scale by body composition, with the library's own
#' fat-free mass (Al-Sallami): the vessel-rich group and muscle by fat-free
#' mass, the fat group by fat mass, its blood flow following its volume as
#' Weber 2021 scaled it, anchored so that the 70 kg, 170 cm, 35-year-old man
#' is GasMan's standard exactly, with a GasMan-scaling option kept for the
#' validation cases at other weights.  Shafer: "Ted Eger would have approved."
#' Built 2026-11-25, as above.
#' Cardiac output is returned here as a single covariate-derived constant, but
#' \code{advanceClosedFormGas()} accepts it per segment, so making it
#' time-varying later requires no change to the engine.
#'
#' @param weight patient weight, kg
#' @param circuitVolume breathing-circuit volume in litres (bag + tubing +
#'   absorber).  This sets the time constant of the lag between the vaporiser
#'   dial and the inspired concentration, and is what makes low-flow anaesthesia
#'   behave differently from high-flow.
#'   TODO(fixture): confirm the value GasMan 4.2 uses.
#' @param height,age,sex the patient's height (cm), age (years) and sex, for
#'   the body-composition scaling; any NULL gives GasMan's linear scaling
#' @param adjustToFFM FALSE for GasMan's linear scaling whatever is given
#'
#' @returns a list of geometry and flow parameters
#' @export
getGasBody <- function(weight = 70, circuitVolume = 8, height = NULL, age = NULL,
                       sex = NULL, adjustToFFM = TRUE)
{
  scale <- weight / 70
  composition <- gasBodyComposition(weight, height, age, sex, adjustToFFM)

  body <- list(
    # Gas-phase volumes, litres
    V_circuit  = circuitVolume,   # machine, not weight-scaled
    V_alveolar = 2.5 * scale,     # functional residual capacity

    # Tissue volumes, litres (Weber et al., 70 kg standard)
    V_brain    = 6.0  * scale,    # vessel-rich group, parameterised as brain
    V_muscle   = 33.0 * scale,
    V_fat      = 14.5 * scale,

    # Cardiac output, L/min
    Q_cardiac  = GAS_DEFAULT_CO_70KG * scale^GAS_DEFAULT_VA_WEIGHT_EXPO,  # GasMan: 5 L/min, allometric

    # Fraction of cardiac output to each tissue group.  Must sum to 1.
    #
    # These are GasMan's [Ratio] values from gasman.ini, adopted deliberately.
    # An earlier version of this file used 0.75 / 0.20 / 0.05, which carried no
    # citation and was not GasMan's.  That difference was small but structural:
    # it was the entire residual disagreement between this engine and the
    # GasMan baseline once every other difference had been controlled -- about
    # 0.45% on alveolar sevoflurane at 30 minutes, and it did NOT shrink as the
    # step size shrank, which is how it was found.  See
    # tests/testthat/test-gas-convergence.R.
    #
    # PROVENANCE.  Weber et al. 2021 (Table 2) publish exactly these fractions,
    # 76 / 18 / 6%, for GasMan's 70 kg standard patient, but give no primary
    # physiological source for them.  They are carried unchanged so that this
    # engine can be checked against GasMan; replacing them with values
    # supported by the physiological literature is a later, separate,
    # documented step.  Do not change them casually -- doing so breaks the
    # validation chain.
    f_brain    = 0.76,
    f_muscle   = 0.18,
    f_fat      = 0.06,

    # Oxygen consumption, L/min STPD.  3.5 mL/kg/min -> 245 mL/min at 70 kg.
    # This is a constant volume sink in the gas phase and exists whether or not
    # nitrous oxide is present.  It is an STPD volume, as oxygen consumption
    # is by convention; the gas balance takes gasOxygenUptake(body), the same
    # oxygen at body temperature in the engine's unit of pressure, and the
    # arterial oxygen content (mL O2/dL, also STPD) takes this value.
    VO2        = 0.0035 * weight
  )
  if (is.null(composition)) return(body)

  # By body composition (the documentation above): lean volumes and the lungs
  # by FFM, fat by FM, flows allometric in each, cardiac output their sum
  lean <- composition[["lean"]]
  fat <- composition[["fat"]]
  body$V_alveolar <- 2.5 * lean
  body$V_brain <- 6.0 * lean
  body$V_muscle <- 33.0 * lean
  body$V_fat <- 14.5 * fat
  flow <- GAS_DEFAULT_CO_70KG * c(brain = 0.76 * lean^GAS_DEFAULT_VA_WEIGHT_EXPO,
                                  muscle = 0.18 * lean^GAS_DEFAULT_VA_WEIGHT_EXPO,
                                  fat = 0.06 * fat^GAS_DEFAULT_VA_WEIGHT_EXPO)
  body$Q_cardiac <- sum(flow)
  body$f_brain <- unname(flow[["brain"]] / body$Q_cardiac)
  body$f_muscle <- unname(flow[["muscle"]] / body$Q_cardiac)
  body$f_fat <- unname(flow[["fat"]] / body$Q_cardiac)
  body$composition <- composition
  body
}

#' The patient's fat-free and fat mass over the reference man's
#'
#' For \code{getGasBody()}'s body-composition scaling.  The fat mass is held
#' at a hundredth of the weight at least, so no fat group is empty.
#' @inheritParams getGasBody
#' @returns c(lean, fat), the two ratios, or NULL for GasMan's linear scaling
#'   (\code{adjustToFFM} FALSE, or any of height, age and sex missing)
#' @keywords internal
gasBodyComposition <- function(weight, height = NULL, age = NULL, sex = NULL,
                               adjustToFFM = TRUE)
{
  if (!isTRUE(adjustToFFM) || is.null(height) || is.null(age) || is.null(sex) ||
      length(height) != 1 || length(age) != 1 || length(sex) != 1 ||
      !is.finite(height) || !is.finite(age) || is.na(sex))
    return(NULL)
  ffm <- ffmAlSallami(weight, height, age, sex)
  ffm <- min(ffm, 0.99 * weight)
  fmReference <- FFM_REFERENCE_WEIGHT - FFM_REFERENCE
  # unname(): a named weight (c(patient = 70)) would give "lean.patient"
  c(lean = unname(ffm / FFM_REFERENCE), fat = unname((weight - ffm) / fmReference))
}


#' Age-adjusted MAC
#'
#' Mapleson's relation: MAC declines about 6% per decade of age.
#' \deqn{MAC(age) = MAC_{40} \times 10^{-0.00269 (age - 40)}}
#'
#' Mapleson (Br J Anaesth 1996;76:179-185) fitted ages from 1 year up.  Below
#' 1 year the relation is extrapolated, and changes MAC by under 1% from its
#' value at 1 year.  It does not model the MAC measured in infants: higher
#' than this relation gives in term neonates and infants (sevoflurane 3.3% in
#' neonates and 3.2% at 1-6 months, Lerman 1994, against 2.31% here at
#' birth), peaking within the first year for isoflurane and desflurane
#' (isoflurane 1.87% at 1-6 months, Cameron 1984; desflurane 9.92% at 6-12
#' months, Taylor and Lerman 1991), and lower in preterm neonates (isoflurane
#' 1.28% under 32 weeks' gestation and 1.41% at 32-37 weeks, LeDez and Lerman
#' 1987, against 1.50% here at birth): figures from the PubMed abstracts, not
#' yet read.  Whether measured
#' infant MAC replaces this relation below 1 year was put to Shafer on
#' 2026-11-25 (Gemini's gas-engine audit, B-03 and F-03) and DEFERRED until
#' the papers are read; they need interlibrary loan (PMIDs 8024136, 1741518,
#' 6703367 and, for preterm neonates, 3631603; \code{papers/pending-work.csv}).
#'
#' @param MAC40 MAC at age 40, % of 1 atm
#' @param age patient age in years
#' @returns age-adjusted MAC, % of 1 atm
#' @export
macForAge <- function(MAC40, age)
{
  MAC40 * 10^(-0.00269 * (age - 40))
}


# Dead space, as a fraction of minute ventilation (Shafer, 2026-10-05).  The
# "ventilation" row of the dose table is MINUTE ventilation; alveolar
# ventilation, which is what exchanges gas, is the rest.  GasMan has no dead
# space: what it calls ventilation is alveolar ventilation.
GAS_DEAD_SPACE_FRACTION <- 0.3


# Respiratory quotient: litres of carbon dioxide produced per litre of oxygen
# consumed.  0.8 is the usual figure for a mixed diet.
#
# Why it is here.  Oxygen consumed leaves the gas phase, and the gas volume
# shrinks by that much (Shafer, 2026-10-05: "as oxygen is consumed, the gas
# volume shrinks.  CO2 is added as oxygen is consumed, but is removed by the CO2
# absorber").  The two halves of that happen in different places.  In the
# ALVEOLI carbon dioxide replaces most of the oxygen taken up, so alveolar gas
# shrinks only by VO2 - VCO2.  In the CIRCUIT the absorber removes the carbon
# dioxide from whatever exhaled gas is rebreathed, which is where the rest of
# the volume goes.  Getting the split right needs VCO2, hence this number.
#
# NOT YET CONFIRMED BY SHAFER (Claude Code, Claude Fable 5.1, 2026-10-05).
GAS_RESPIRATORY_QUOTIENT <- 0.8


#' One litre STPD in the gas engine's unit of volume
#'
#' Oxygen consumption and carbon dioxide production are STPD volumes by
#' convention (0 C, 760 mmHg, dry).  The engine counts gas in the alveoli at
#' body temperature, and its unit of tension, 1\% of a sea-level atmosphere, is
#' \code{GAS_ALVEOLAR_TORR / 100} = 7.13 mmHg, the dry share of that
#' atmosphere.  One litre STPD is therefore
#' \code{(760 / 713) (273.15 + T) / 273.15} litres there: 1.2103 at 37 C.  Its
#' counterpart in mmHg and mL is the textbooks' 0.863 in
#' \code{PACO2 = 0.863 VCO2 / VA}: 760 x 310.15 / 273.15 / 1000.
#'
#' Until 2026-11-28 the engine subtracted \code{body$VO2} from the alveolar
#' gas unconverted, so its oxygen sink was 1/1.21 of the oxygen consumed: on
#' room air at 70 kg and 4 L/min of alveolar ventilation the alveolar oxygen
#' read 107 mmHg where the alveolar gas equation gives 99.  Found
#' while the mathematics of the gas engine expansion was drafted
#' (docs/gas-engine-expansion-mathematics.md, section 1); Steven L. Shafer,
#' 2026-11-28: "Yes to all eight."  GasMan carries no oxygen, so no comparison
#' with it moves.
#'
#' @param temperature body temperature, C; NULL is 37
#' @returns litres in the engine's unit per litre STPD
#' @keywords internal
gasStpdFactor <- function(temperature = NULL)
{
  if (is.null(temperature)) temperature <- 37
  (SEA_LEVEL_PRESSURE / GAS_ALVEOLAR_TORR) * (273.15 + temperature) / 273.15
}

#' Temperature of the gas at the flowmeters, C
#'
#' Steven L. Shafer, 2026-11-28: "Convert both, assuming 20 degrees C at the
#' flowmeter."  Room temperature, at which flowmeters are calibrated; the
#' vapour's molar volume in R/gasGreenhouse.R is taken at the same 20 C.
#' @keywords internal
GAS_FLOWMETER_TEMPERATURE <- 20

#' Litres in the lungs per litre of fresh gas at the flowmeters
#'
#' The three volumes of the gas engine arrive in three conditions.  The
#' ventilation is a volume at body temperature, saturated (BTPS).  The fresh
#' gas is read at the flowmeters dry and at room temperature
#' (\code{GAS_FLOWMETER_TEMPERATURE}): on its way to the alveoli it warms to
#' body temperature, (273.15 + T) / 293.15, 5.8\% at 37 C, and takes up water
#' vapour, P_B / (P_B - 47), 6.6\% at sea level, so a litre on the flowmeter
#' is 1.128 litres in the lungs.  Oxygen consumption is an STPD volume
#' (\code{gasStpdFactor()}, 1.210).  Until 2026-11-28 all three were taken
#' litre for litre, which kept the closed circuit's arithmetic (0.25 L/min of
#' oxygen against 0.245 L/min consumed) and put the alveolar oxygen about 9
#' mmHg high.  Found while piece 1 of the gas engine expansion was built:
#' converting the oxygen consumption alone left 0.25 L/min of oxygen 18\%
#' short of 0.245 L/min STPD.  Shafer, 2026-11-28: "Convert both, assuming 20
#' degrees C at the flowmeter.  That is correct.  Interestingly, I've never
#' even thought about it before."  With both, 0.25 L/min of oxygen at 20 C is
#' 0.233 L/min STPD, 5\% short of 0.245: the physics.
#'
#' The partial pressures of the humidified fresh gas are
#' \code{gasHumidityRatio()}'s business; this is the volume.  A body with
#' \code{volumesAsStated} TRUE takes every volume litre for litre, as GasMan
#' does (\code{advanceClosedFormGas(volumeConversion = FALSE)}, and always
#' with the semi-closed circuit).
#'
#' @param body the gas body
#' @returns the factor on every fresh gas flow; 1.1277 at 37 C and sea level
#' @keywords internal
gasFreshGasExpansion <- function(body)
{
  if (isTRUE(body$volumesAsStated)) return(1)
  temperature <- if (is.null(body$temperature)) 37 else body$temperature
  (SEA_LEVEL_PRESSURE / GAS_ALVEOLAR_TORR) / gasHumidityRatio(body) *
    (273.15 + temperature) / (273.15 + GAS_FLOWMETER_TEMPERATURE)
}

#' Litres at the anaesthesia machine per litre in the lungs
#'
#' What the Visualizer counts vapour in.  A clinician reads the agent
#' delivered as fresh gas flow times the vaporiser's setting, a volume at the
#' machine (dry, \code{GAS_FLOWMETER_TEMPERATURE}, one atmosphere), so the
#' Visualizer shows every amount of vapour in that volume: the engine's
#' amounts, counted in the lungs' litres, times this (Steven L. Shafer,
#' 2026-11-28: "show the flowmeter settings, and keep the actual delivered
#' concentrations what the user set.  Otherwise clinicians will be confused
#' and think it is a program bug").  It is the reciprocal of
#' \code{gasFreshGasExpansion()} at sea level, 0.8867 at 37 C, and does not
#' depend on the altitude: an amount is a number of molecules, and the
#' humidity's share of the pressure cancels between the fresh gas's volume and
#' its partial pressure.
#'
#' @param body the gas body
#' @returns the factor; 1 for a body whose volumes are taken as stated
#' @keywords internal
gasMachineLitres <- function(body)
{
  if (isTRUE(body$volumesAsStated)) return(1)
  temperature <- if (is.null(body$temperature)) 37 else body$temperature
  (GAS_ALVEOLAR_TORR / SEA_LEVEL_PRESSURE) *
    (273.15 + GAS_FLOWMETER_TEMPERATURE) / (273.15 + temperature)
}

#' Oxygen consumption as the gas engine's balance takes it
#'
#' \code{body$VO2} (L/min STPD) at body temperature, in the engine's unit of
#' volume (\code{gasStpdFactor()}).  A sea-level volume: at altitude the
#' ambient volume is this over \code{gasPressureRatio(body)}.
#'
#' @param body the gas body (\code{getGasBody()}, \code{simulateGases()}'s)
#' @returns L/min
#' @keywords internal
gasOxygenUptake <- function(body)
{
  if (isTRUE(body$volumesAsStated)) return(body$VO2)
  body$VO2 * gasStpdFactor(body$temperature)
}

# The most of the exhaled gas that is taken to be carbon dioxide.  The carbon
# dioxide fraction is VCO2 / MV, which is a few percent at any ventilation a
# patient could live with, but at a minute ventilation of a fraction of a litre
# it would exceed one and the circuit blend would go negative.  Capped here;
# such a patient is dead of hypercapnia whatever the gas model says.
GAS_MAX_EXHALED_CO2 <- 0.5


# Default ventilation, used when a gas is in the dose table but no usable
# ventilation setting has been entered.  It is a MINUTE ventilation, chosen so
# that the ALVEOLAR ventilation it implies is GasMan's default.
#
# These are GasMan's own defaults, adopted at Shafer's direction (2026-10-05:
# "for now I want to be identical with GasMan; later on we will update with
# more current data"): gasman.ini [Defaults] VA=4 (L/min at the 70 kg
# standard), scaled to the patient allometrically as (weight / 70)^0.75 --
# GasDoc.cpp computes
#     factor = sqrt(sqrt(factor * factor * factor))
# and sets m_fVA = m_fDfltVA * factor.  Source read from
# github.com/rasman/gasmanonline, gasman_api/gasmanAPI (GPL-3.0), 2026-10-05.
# (Corrected 2026-11-24: in the API that scaling has no effect, InitDocument
# computing the factor while the weight is still the default 70, and
# SetWeight changing only the tick; the (weight / 70)^0.75 scaling is the
# desktop GasMan's, applied when the user changes the weight,
# src/gasman_main/gasdoc.cpp.  A gasman_run built from d3a2dd3 at 10 kg
# with no VA or CO reports 4 and 5 L/min.  The values here are the desktop
# program's, so nothing changes (Gemini's gas-engine audit of 2026-11-23,
# A-02).)
#
# (Claude Code, Claude Fable 5.1, 2026-10-05.  An earlier draft the same day
# used 8 mL/kg x an assumed 10 breaths/min; this replaces it.)
GAS_DEFAULT_VA_70KG        <- 4     # ALVEOLAR, as in GasMan
GAS_DEFAULT_CO_70KG        <- 5     # gasman.ini [Defaults] CO=5, same scaling
GAS_DEFAULT_VA_WEIGHT_EXPO <- 0.75


#' Default minute ventilation for a patient, L/min
#'
#' The minute ventilation whose alveolar part is GasMan's default alveolar
#' ventilation: 4 L/min at 70 kg, scaled by \code{(weight / 70)^0.75}.  With a
#' dead space of 30% that is 5.7 L/min at 70 kg.
#'
#' DECIDED (Shafer, 2026-11-25): this default stays scaled with total
#' weight, "That is how clinicians think about it", although the gas
#' compartments now follow fat-free and fat mass (getGasBody()).  The
#' question as put: a 150 kg man of 170 cm gets 10.1 L/min against 5.7 at
#' 70 kg, and his 90% fall of brain sevoflurane after five hours at 2.5%
#' inspired is 17 minutes against 27.5 at 70 kg (56 at a fixed 5.7 L/min; 114
#' with the old linear compartments at 5.7); following fat-free mass would
#' have given him about 7.5 L/min.
#'
#' @param weight patient weight, kg.  Falls back to 70 kg if missing or invalid.
#' @param deadSpace dead space as a fraction of minute ventilation
#' @returns minute ventilation in L/min, rounded so it reads cleanly in the
#'   dose table: to 0.1 L/min from 1 L/min up, and to two significant figures
#'   below.  Rounded to 0.1 L/min throughout, a patient under 0.126 kg (the
#'   Patient Profile takes 0.1 kg) was given 0 L/min, the apnoea the default
#'   exists to prevent (\code{ensureGasVentilation()}), and every patient under
#'   about 7 kg was off by up to 30 percent (0.1 L/min for 0.14 at 0.5 kg; found by
#'   the session that addressed Carbon's third UI audit, 2026-11-15).
#' @export
defaultGasVentilation <- function(weight = 70, deadSpace = GAS_DEAD_SPACE_FRACTION)
{
  if (length(weight) != 1 || !is.finite(weight) || weight <= 0) weight <- 70
  ventilation <- GAS_DEFAULT_VA_70KG * (weight / 70)^GAS_DEFAULT_VA_WEIGHT_EXPO /
    (1 - deadSpace)
  if (ventilation >= 1) round(ventilation, 1) else signif(ventilation, 2)
}


# Fresh-gas oxygen fraction the oxygen row starts at when it is added
# automatically alongside nitrous oxide (Shafer, 2026-10-05: 21%, room air).
GAS_INITIAL_O2_FRACTION <- 0.21


# Composition of dry air.  Used to split an air flow into its oxygen and
# nitrogen contributions.
AIR_FRACTION_O2 <- 0.2093
AIR_FRACTION_N2 <- 0.7807


#' Names of the dose-table entries handled by the inhaled-gas engine
#'
#' Read from the \code{Class} column of \code{drugDefaults_global.csv}.  These
#' rows are routed to \code{advanceClosedFormGas()} as a group and never reach
#' \code{getDrugPK()} or \code{simCpCe()}, which assume a three-compartment
#' mammillary model and mass units that the gases do not have.
#'
#' @returns character vector of gas/setting names
#' @export
gasDrugNames <- function()
{
  d <- getDrugDefaultsGlobal()
  if (!"Class" %in% names(d)) return(character(0))
  d$Drug[!is.na(d$Class) & d$Class == "gas"]
}


#' Is this dose-table entry handled by the inhaled-gas engine?
#'
#' @param drug one or more drug names
#' @returns logical vector
#' @export
isGasDrug <- function(drug)
{
  drug %in% gasDrugNames()
}
