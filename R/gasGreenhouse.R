# -----------------------------------------------------------------------------
# The greenhouse effect of the inhaled anesthetics
#
# Shafer, 2026-11-17, on what the Visualizer should show beside the agent
# delivered: "What people care more about is greenhouse gas effect. That
# would be interesting as a teaching point."  Cost was set aside the same day
# ("The drugs are all generic, so nobody cares").
#
# SOURCE.  Sulbaek Andersen MP, Nielsen OJ, Wallington TJ, Karpichev B,
# Sander SP.  Assessing the impact on global climate from general anesthetic
# gases.  Anesth Analg 2012;114:1081-5.  doi:10.1213/ANE.0b013e31824d6150.
# Read in full (supplied by Shafer, 2026-11-17).  Table 1 was read from the
# rendered page: in the text layer its reference superscripts run into the
# values ("25403" is a 100-year GWP of 2540 from the paper's reference 3).
#
#   Table 1                 lifetime (y)   GWP 20 y   GWP 100 y   GWP 500 y
#   nitrous oxide               114           289        298         153     (its ref 8, WMO 2010)
#   isoflurane                  3.2          1800        510         160     (ref 13)
#   desflurane                   14          6810       2540         130     (ref 3)
#   sevoflurane                 1.1           440        130          40     (ref 3)
#
# The paper gives the CO2 equivalent of an emission as the mass emitted, in
# kilograms, times the GWP, and uses the 100-year horizon, which it notes the
# US Environmental Protection Agency, the European Environment Agency and the
# Kyoto Protocol use.  So does stanpumpR.
#
# [decision] Later assessments may have revised these values (the paper's own
# nitrous oxide figures are the World Meteorological Organization's of 2010).
# No later source was read here; whether to move to one is Shafer's decision.
#
# WHAT IS EMITTED.  Nothing in the gas engine removes agent from the body or
# the circuit except the pop-off valve: there is no metabolism.  So all the
# agent delivered reaches the atmosphere, through the valve during the case or
# breathed out after it, unless it is captured.  The CO2 equivalent is
# therefore counted on the agent DELIVERED, not only the agent vented so far.
#
# MASS.  The Visualizer counts agent in mL of vapour at one atmosphere.  The
# agent delivered is the fresh gas flow times the vaporiser's setting, a gas
# volume read at the anaesthesia machine, so it is converted to moles as an
# ideal gas at 20 C: RT/P = 0.0820574 x 293.15 = 24.055 L/mol
# (GAS_VAPOUR_MOLAR_VOLUME).  At 37 C the same volume would be 5.8% less agent.
# Molecular weights are computed from the formulas Table 1 prints and the
# IUPAC standard atomic weights (abridged, to the third decimal): N2O 44.013,
# isoflurane 184.49, desflurane 168.04, sevoflurane 200.05 g/mol.
#
# COMPARISON.  The paper's footnote compares the global emissions with a
# typical US passenger car, which it gives as emitting 5.1 tonnes of CO2 a year
# (its reference 13).  The Visualizer says how long such a car takes to emit
# the same CO2: 5100 kg / 365.25 days = 14.0 kg a day.
# -----------------------------------------------------------------------------

#' Ideal-gas molar volume at 20 C and one atmosphere, L/mol
#' @noRd
GAS_VAPOUR_MOLAR_VOLUME <- 0.0820574 * 293.15

#' CO2 a typical US passenger car emits in a year, kg (Sulbaek Andersen 2012)
#' @noRd
GAS_CAR_CO2_KG_PER_YEAR <- 5100

#' Standard atomic weights, g/mol (IUPAC, abridged)
#' @noRd
GAS_ATOMIC_WEIGHTS <- c(H = 1.008, C = 12.011, N = 14.007, O = 15.999, F = 18.998, Cl = 35.45)

#' The greenhouse properties of the inhaled anesthetics
#'
#' Table 1 of Sulbaek Andersen 2012, with each agent's formula as the table
#' prints it and its atoms counted from it.
#' @noRd
GAS_GREENHOUSE <- data.frame(
  gas      = c("nitrousOxide", "isoflurane",   "desflurane",  "sevoflurane"),
  formula  = c("N2O",          "CF3CHClOCHF2", "CF3CHFOCHF2", "(CF3)2CHOCH2F"),
  C        = c(0, 3, 3, 4),
  H        = c(0, 2, 2, 3),
  N        = c(2, 0, 0, 0),
  O        = c(1, 1, 1, 1),
  F        = c(0, 5, 6, 7),
  Cl       = c(0, 1, 0, 0),
  lifetime = c(114, 3.2, 14, 1.1),
  gwp20    = c(289, 1800, 6810, 440),
  gwp100   = c(298, 510, 2540, 130),
  gwp500   = c(153, 160, 130, 40),
  stringsAsFactors = FALSE
)

#' An inhaled anesthetic's molecular weight, g/mol, from its formula
#'
#' @param gas the gas's name in stanpumpR (\code{"sevoflurane"})
#' @returns the molecular weight, or NA for a gas not in the table
#' @noRd
gasMolecularWeight <- function(gas)
{
  row <- GAS_GREENHOUSE[GAS_GREENHOUSE$gas == gas, , drop = FALSE]
  if (nrow(row) != 1) return(NA_real_)
  sum(unlist(row[names(GAS_ATOMIC_WEIGHTS)]) * GAS_ATOMIC_WEIGHTS)
}

#' What the Visualizer needs to show an agent's greenhouse effect
#'
#' @param gas the gas's name in stanpumpR
#' @returns a list: \code{gwp100}; \code{lifetime}, years; \code{kgPerLitre},
#'   kg of agent in a litre of its vapour at 20 C and one atmosphere;
#'   \code{carKgPerYear}; and \code{source}.  NULL for a gas not in the table
#'   (oxygen, air).
#' @noRd
gasGreenhouse <- function(gas)
{
  row <- GAS_GREENHOUSE[GAS_GREENHOUSE$gas == gas, , drop = FALSE]
  if (nrow(row) != 1) return(NULL)
  list(gwp100 = row$gwp100, lifetime = row$lifetime,
       kgPerLitre = gasMolecularWeight(gas) / 1000 / GAS_VAPOUR_MOLAR_VOLUME,
       carKgPerYear = GAS_CAR_CO2_KG_PER_YEAR,
       source = "Sulbaek Andersen 2012, 100-year GWP")
}

#' kg of CO2 equivalent in a volume of an agent's vapour
#'
#' @param gas the gas's name in stanpumpR
#' @param mL vapour at one atmosphere, measured at 20 C
#' @returns kg of CO2 equivalent (100-year GWP), NA for a gas not in the table
#' @noRd
gasCO2Equivalent <- function(gas, mL)
{
  g <- gasGreenhouse(gas)
  if (is.null(g)) return(rep(NA_real_, length(mL)))
  mL / 1000 * g$kgPerLitre * g$gwp100
}

#' The help's table of the inhaled anesthetics' greenhouse effect
#'
#' One row per agent: its atmospheric lifetime and 100-year GWP from Sulbaek
#' Andersen 2012, the mass of a litre of its vapour, and the CO2 equivalent of
#' one hour at a typical maintenance setting, so the table and the Visualizer
#' cannot disagree.
#' @noRd
helpGasGreenhouseTableMarkdown <- function(...)
{
  # One hour of each agent at about 1 MAC in 2 L/min, and nitrous oxide at
  # 60% in 2 L/min, as plain examples of the arithmetic.
  examples <- c(nitrousOxide = 60, isoflurane = 1.2, desflurane = 6, sevoflurane = 2)
  names <- c(nitrousOxide = "Nitrous oxide", isoflurane = "Isoflurane",
             desflurane = "Desflurane", sevoflurane = "Sevoflurane")
  carKgPerHour <- GAS_CAR_CO2_KG_PER_YEAR / (365.25 * 24)
  rows <- vapply(c("sevoflurane", "isoflurane", "desflurane", "nitrousOxide"), function(g) {
    gh <- gasGreenhouse(g)
    mL <- 10 * 2 * examples[[g]] * 60
    kg <- gasCO2Equivalent(g, mL)
    sprintf("| %s | %s | %s | %s | %s%% in 2 L/min | %s | %s |",
            names[[g]], format(gh$lifetime), format(gh$gwp100),
            formatC(gh$kgPerLitre * 1000, format = "f", digits = 2),
            format(examples[[g]]), formatC(kg, format = "f", digits = 1),
            formatC(kg / carKgPerHour, format = "f", digits = 0))
  }, character(1))
  paste(c(
    "| Agent | Atmospheric lifetime, years | 100-year GWP | g in 1 L of vapour | One hour at | kg CO<sub>2</sub> equivalent | Hours of a typical US car's CO<sub>2</sub> |",
    "|---|---|---|---|---|---|---|",
    rows), collapse = "\n")
}
