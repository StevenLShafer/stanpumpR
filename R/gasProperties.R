# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer, as the parameter layer for the inhaled-gas engine.
#
# Structure and parameter values follow the Gas Man(R) model of James H. Philip
# as described in the peer-reviewed literature (Philip is a co-author of the
# first reference below).  Gas Man itself is closed source; nothing here is
# derived from its code.
#
#   Weber J, Schmidt J, Wirth S, Schumann S, Philip JH, Eberhart LHJ.
#   Context-sensitive decrement times for inhaled anesthetics in obese patients
#   explored with Gas Man(R).  J Clin Monit Comput.  PMC7943506.
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
# analytic single-compartment limits).  The PARTITION COEFFICIENTS AND MAC
# VALUES BELOW ARE LITERATURE VALUES, NOT YET RECONCILED AGAINST GAS MAN 4.2's
# own tables (Weber et al. Table 1).  They must be replaced with the Gas Man
# values before any claim of fidelity to Gas Man is made.  Marked TODO(fixture)
# at each site.
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
#'   \item{tb_brain, tb_muscle, tb_fat}{tissue:blood partition coefficients, as
#'     published.  The equations need tissue:gas, obtained by multiplying by
#'     \code{lambda_blood} -- see \code{gasPartitionTissueGas()}.}
#'   \item{MAC40}{minimum alveolar concentration at age 40, in \% of 1 atm.
#'     NA for gases with no anaesthetic potency in this context.}
#'   \item{potent}{TRUE if the gas contributes to the MAC sum}
#' }
#'
#' The vessel-rich group is parameterised with the BRAIN partition coefficient,
#' which is how Weber et al. tabulate Gas Man's parameters, and is why the VRG
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

    # MAC, % of 1 atm, at the reference age.  Nitrogen's 200 is Gas Man's own
    # figure, recorded as it stands and flagged below rather than corrected or
    # omitted: the table should say what Gas Man says.  It is inert here because
    # nitrogen is not summed into MAC.
    MAC40 = c(110, 2.1, 1.1, 6.0, 200, NA),

    # Whether the agent contributes to the summed MAC.  gasman.ini gives
    # nitrogen a MAC of 200, presumably for hyperbaric completeness, but
    # including it would post 0.4 MAC on room air, so it is excluded here.
    # Flagged rather than assumed -- see the note in the header.
    potent = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),

    # Provenance flags.  A value is used as Gas Man states it, so that
    # validation compares like with like, but is marked when its provenance is
    # known to be wrong.  FALSE means "not yet checked", not "verified".
    flagged = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    flagNote = c(NA, NA, NA, NA,
                 paste("MAC 200 (2 atm) contradicts Eger's estimate of 110 atm,",
                       "i.e. 11000% of one atmosphere: low by about 55-fold.",
                       "Room air is 0.79 atm nitrogen, so the true contribution",
                       "is about 0.007 MAC, not the 0.4 MAC this figure implies."),
                 NA),

    stringsAsFactors = FALSE
  )
}


#' Parameters whose provenance is known to be wrong
#'
#' Gas Man's values are used as they stand so that validation compares like with
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
#' tissue is its volume times its tissue:GAS partition coefficient.  Published
#' tables give tissue:BLOOD, hence this conversion.
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
#' Tissue volumes and cardiac output scale with weight from the 70 kg standard.
#' Circuit volume is a property of the anaesthesia machine, not the patient, so
#' it does not scale.
#'
#' Cardiac output is Gas Man's default: 5 L/min at 70 kg (gasman.ini
#' \code{[Defaults] CO=5}), scaled allometrically by \code{(weight / 70)^0.75}
#' as \code{GasDoc.cpp} does.  Adopted 2026-10-05 at Shafer's direction ("for
#' now I want to be identical with Gas Man; later on we will update with more
#' current data"), replacing the linear 75 mL/kg (5.25 L/min at 70 kg) he chose
#' on 2026-09-02.  Tissue and alveolar volumes still scale linearly with weight,
#' which is also what Gas Man does (\code{fWtFactor = fWeight / STD_WEIGHT}).
#' Cardiac output is returned here as a single covariate-derived constant, but
#' \code{advanceClosedFormGas()} accepts it per segment, so making it
#' time-varying later requires no change to the engine.
#'
#' @param weight patient weight, kg
#' @param circuitVolume breathing-circuit volume in litres (bag + tubing +
#'   absorber).  This sets the time constant of the lag between the vaporiser
#'   dial and the inspired concentration, and is what makes low-flow anaesthesia
#'   behave differently from high-flow.
#'   TODO(fixture): confirm the value Gas Man 4.2 uses.
#'
#' @returns a list of geometry and flow parameters
#' @export
getGasBody <- function(weight = 70, circuitVolume = 8)
{
  scale <- weight / 70

  list(
    # Gas-phase volumes, litres
    V_circuit  = circuitVolume,   # machine, not weight-scaled
    V_alveolar = 2.5 * scale,     # functional residual capacity

    # Tissue volumes, litres (Weber et al., 70 kg standard)
    V_brain    = 6.0  * scale,    # vessel-rich group, parameterised as brain
    V_muscle   = 33.0 * scale,
    V_fat      = 14.5 * scale,

    # Cardiac output, L/min
    Q_cardiac  = GAS_DEFAULT_CO_70KG * scale^GAS_DEFAULT_VA_WEIGHT_EXPO,  # Gas Man: 5 L/min, allometric

    # Fraction of cardiac output to each tissue group.  Must sum to 1.
    #
    # These are Gas Man's [Ratio] values from gasman.ini, adopted deliberately.
    # An earlier version of this file used 0.75 / 0.20 / 0.05, which carried no
    # citation and was not Gas Man's.  That difference was small but structural:
    # it was the entire residual disagreement between this engine and the Gas
    # Man baseline once every other difference had been controlled -- about
    # 0.45% on alveolar sevoflurane at 30 minutes, and it did NOT shrink as the
    # step size shrank, which is how it was found.  See
    # tests/testthat/test-gas-convergence.R.
    #
    # PROVENANCE NOT YET ESTABLISHED.  Gas Man supplies these numbers but no
    # source for them.  They are carried unchanged so that this engine can be
    # checked against Gas Man; replacing them with values supported by the
    # peer-reviewed literature is a later, separate, documented step.  Do not
    # change them casually -- doing so breaks the validation chain.
    f_brain    = 0.76,
    f_muscle   = 0.18,
    f_fat      = 0.06,

    # Oxygen consumption, L/min.  3.5 mL/kg/min -> 245 mL/min at 70 kg.
    # This is a constant volume sink in the gas phase and exists whether or not
    # nitrous oxide is present.
    VO2        = 0.0035 * weight
  )
}


#' Age-adjusted MAC
#'
#' Mapleson's relation: MAC declines about 6\% per decade of age.
#' \deqn{MAC(age) = MAC_{40} \times 10^{-0.00269 (age - 40)}}
#'
#' @param MAC40 MAC at age 40, \% of 1 atm
#' @param age patient age in years
#' @returns age-adjusted MAC, \% of 1 atm
#' @export
macForAge <- function(MAC40, age)
{
  MAC40 * 10^(-0.00269 * (age - 40))
}


# Dead space, as a fraction of minute ventilation (Shafer, 2026-10-05).  The
# "ventilation" row of the dose table is MINUTE ventilation; alveolar
# ventilation, which is what exchanges gas, is the rest.  Gas Man has no dead
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


# Default ventilation, used when a gas is in the dose table but no usable
# ventilation setting has been entered.  It is a MINUTE ventilation, chosen so
# that the ALVEOLAR ventilation it implies is Gas Man's default.
#
# These are Gas Man's own defaults, adopted at Shafer's direction (2026-10-05:
# "for now I want to be identical with Gas Man; later on we will update with
# more current data"): gasman.ini [Defaults] VA=4 (L/min at the 70 kg
# standard), scaled to the patient allometrically as (weight / 70)^0.75 --
# GasDoc.cpp computes
#     factor = sqrt(sqrt(factor * factor * factor))
# and sets m_fVA = m_fDfltVA * factor.  Source read from
# github.com/rasman/gasmanonline, gasman_api/gasmanAPI (GPL-3.0), 2026-10-05.
#
# (Claude Code, Claude Fable 5.1, 2026-10-05.  An earlier draft the same day
# used 8 mL/kg x an assumed 10 breaths/min; this replaces it.)
GAS_DEFAULT_VA_70KG        <- 4     # ALVEOLAR, as in Gas Man
GAS_DEFAULT_CO_70KG        <- 5     # gasman.ini [Defaults] CO=5, same scaling
GAS_DEFAULT_VA_WEIGHT_EXPO <- 0.75


#' Default minute ventilation for a patient, L/min
#'
#' The minute ventilation whose alveolar part is Gas Man's default alveolar
#' ventilation: 4 L/min at 70 kg, scaled by \code{(weight / 70)^0.75}.  With a
#' dead space of 30\% that is 5.7 L/min at 70 kg.
#'
#' @param weight patient weight, kg.  Falls back to 70 kg if missing or invalid.
#' @param deadSpace dead space as a fraction of minute ventilation
#' @returns minute ventilation in L/min, rounded to 0.1 so it reads cleanly in
#'   the dose table
#' @export
defaultGasVentilation <- function(weight = 70, deadSpace = GAS_DEAD_SPACE_FRACTION)
{
  if (length(weight) != 1 || !is.finite(weight) || weight <= 0) weight <- 70
  round(GAS_DEFAULT_VA_70KG * (weight / 70)^GAS_DEFAULT_VA_WEIGHT_EXPO /
          (1 - deadSpace), 1)
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
