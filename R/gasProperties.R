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
    gas          = c("nitrousOxide", "sevoflurane", "isoflurane", "nitrogen", "oxygen"),
    soluble      = c(TRUE,           TRUE,          TRUE,         TRUE,       FALSE),

    # Blood:gas.  N2O 0.47 and isoflurane 1.4 are the values used by
    # Korman/Dash/Peyton (Anesthesiology 2018;128:1075-83); sevoflurane 0.65 is
    # the conventional value (they quote 0.67).  Nitrogen 0.014.
    # TODO(fixture): replace with Gas Man 4.2 values from Weber et al. Table 1.
    lambda_blood = c(0.47,           0.65,          1.4,          0.014,      NA),

    # Tissue:blood.  Conventional values (Eger).  Nitrogen is near-unity in
    # lean tissue and lipophilic in fat.
    # TODO(fixture): replace with Gas Man 4.2 values from Weber et al. Table 1.
    tb_brain     = c(1.1,            1.7,           2.6,          1.0,        NA),
    tb_muscle    = c(1.2,            3.1,           4.0,          1.0,        NA),
    tb_fat       = c(2.3,            48,            45,           5.3,        NA),

    # MAC at age 40, % of 1 atm.
    MAC40        = c(104,            2.05,          1.15,         NA,         NA),
    potent       = c(TRUE,           TRUE,          TRUE,         FALSE,      FALSE),

    stringsAsFactors = FALSE
  )
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
    brain  = props$tb_brain  * props$lambda_blood,
    muscle = props$tb_muscle * props$lambda_blood,
    fat    = props$tb_fat    * props$lambda_blood
  )
}


#' Body and breathing-circuit geometry for the inhaled-gas model
#'
#' Tissue volumes and cardiac output scale with weight from the 70 kg standard.
#' Circuit volume is a property of the anaesthesia machine, not the patient, so
#' it does not scale.
#'
#' Cardiac output is 75 mL/kg (Shafer, 2026-09-02): 5.25 L/min at 70 kg, which
#' sits inside the 5.0-6.03 L/min range Weber et al. report for Gas Man.  It is
#' returned here as a single covariate-derived constant, but
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
    Q_cardiac  = 0.075 * weight,  # 75 mL/kg

    # Fraction of cardiac output to each tissue group.  Must sum to 1.
    f_brain    = 0.75,
    f_muscle   = 0.20,
    f_fat      = 0.05,

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


# Composition of dry air.  Used to split an air flow into its oxygen and
# nitrogen contributions.
AIR_FRACTION_O2 <- 0.2093
AIR_FRACTION_N2 <- 0.7807
