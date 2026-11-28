# -----------------------------------------------------------------------------
# Altitude and the inhaled anesthetics
# -----------------------------------------------------------------------------
# Steven L. Shafer, 2026-11-18: "add to the anesthesia sliders altitude,
# starting at sea level and increasing to 7000 meters.  The scale can be
# linear."  Drafted by Claude Code the same day.
#
# WHAT ALTITUDE DOES.  The barometric pressure falls; nothing about the drug or
# the patient changes.  The gas engine (R/advanceClosedFormGas.R) carries every
# tension as a PARTIAL PRESSURE relative to sea level (in the alveoli, % of
# the dry share of a sea-level atmosphere, 713 Torr, GAS_ALVEOLAR_TORR; in the
# dry circuit, % of 760 Torr), and in those units its mass balance does not
# depend on the barometric
# pressure at all: ventilation, cardiac output and the tissue capacities move
# amounts in proportion to partial pressure (C = P / RT in gas, lambda P / RT in
# blood and tissue), so the RT cancels and the barometric pressure never
# enters.  Anesthetic potency follows partial pressure too, so MAC, in % of a
# sea-level atmosphere, is the same at every altitude (James and White 1984,
# PMID 6239572, who proposed calling it the minimum alveolar PARTIAL PRESSURE;
# James 1982, PMID 7091603, read in full: 50% nitrous oxide raised the pain
# threshold of 20 volunteers by 71.5% at a sea-level pressure (100 kPa), 40%
# at 1460 m (85 kPa) and 19% at a simulated 3300 m (69 kPa), the last not
# significant).  What altitude changes is where gas VOLUMES and
# FRACTIONS meet partial pressures, with r = P_B / 760:
#
#   1. Room air, and every carrier gas from a flowmeter, is a FRACTION of the
#      ambient pressure: oxygen, nitrogen and nitrous oxide enter at r times
#      their fraction.  70% nitrous oxide at 7000 m is about 32% of a
#      sea-level atmosphere.
#   2. A variable-bypass vaporizer (sevoflurane, isoflurane) delivers about the
#      dial's sea-level PARTIAL PRESSURE at any altitude: the saturated vapour
#      pressure does not depend on the barometric pressure, so the vapour's
#      fraction of the output rises as the pressure falls and its partial
#      pressure stays put.  James and White 1984 (read in full, 2026-11-19)
#      measured it: two Fluotec Mk II and a Drager Vapor halothane vaporizer
#      at sea level, 5000 and 10,000 feet in a chamber, read on an analyser
#      of partial pressure, changed by no more than 0.1 kPa from their sea
#      level output.  The desflurane vaporizer (Tec 6) is a heated,
#      pressurised gas-vapour blender that "works at absolute pressures ... It
#      will deliver accurately the dialed volumes percent of desflurane"
#      (Andrews and Johnston 1993, PMID 8498675, read in full, 2026-11-19), so
#      its partial pressure is r times the dial
#      (ALTITUDE_VOLUME_PERCENT_VAPORIZERS): their example raises the dial
#      from 10% to 12.8% at 2000 m, which is 760/593 Torr, the ICAO pressure
#      there (West gives 604 Torr, 12.6%).  Their note that the Tec 6 delivers
#      up to 20% less than its dial with 70% nitrous oxide as carrier at low
#      flows is a sea-level effect of the carrier gas and is not modelled.
#   3. The vapour displaces carrier gas in proportion to its FRACTION of the
#      fresh gas, its partial pressure over r.
#   4. Gas taken up leaves an ambient VOLUME behind: uptake in L/min at the
#      ambient pressure is lambda Qco dP / (100 r), 1/r times the sea-level
#      volume for the same partial pressures.  So the concentration and second
#      gas effects (the uptake coupling) grow at altitude, and so does the
#      volume oxygen consumption takes, VO2 (1 - RQ) / r, VO2 being a
#      sea-level volume.  The carbon dioxide in the exhaled gas, VCO2 / MV of
#      its volume at sea level, is VCO2 / (MV r) of it.
#
#   5. WATER VAPOUR (Shafer, 2026-11-19: "add water vapour").  Inspired gas
#      is humidified to 47 Torr at 37 C whatever the pressure, so the dry
#      gases share P_B - 47.  The sea-level humidification is already inside
#      the engine's units (MAC and the GasMan validation are of humidified
#      alveolar gas), so what fresh gas brings to the alveoli, and the room
#      air the patient starts in, take w = ((P_B - 47)/P_B) / (713/760),
#      exactly 1 at sea level, 0.91 at 7000 m (gasHumidityRatio()).  Rebreathed
#      gas was humidified, dried and humidified again, so the factor cancels
#      for it.  The water vapour pressure is held at its 37 C value at every
#      body temperature (28 Torr at 28 C), which leaves sea level untouched by
#      the temperature slider; the semi-closed circuit, kept for comparison
#      with GasMan at sea level, does not take it.
#
# NOT MODELLED.  The flows are taken as true ambient flows, a [decision] put
# to Shafer on 2026-11-18.  The evidence:
# James and White 1984 measured three oxygen and three nitrous oxide
# bobbin flowmeters calibrated at sea level.  Above 4 L/min they delivered
# more than they read, close to the density prediction sqrt(760 / P_B):
# 7.2% more at 4800 ft (8.5% predicted) and 18.5% (oxygen) and 21% (nitrous
# oxide) more at 10,000 ft (20.3% predicted).  At low flows, where viscosity
# rather than density sets the bobbin, the error was much smaller, and they
# did not test low-flow flowmeters; electronic flow sensors on modern
# machines may not behave like either.  The intravenous drugs do not respond to altitude, and neither
# does cardiac output or ventilation (each has its own control), nor
# acclimatisation (haemoglobin, a hypoxic ventilatory response).
#
# THE PRESSURE.  West 1996 (PMID 8904608, J Appl Physiol 81:1850), the mean of
# the model atmospheres for latitude 15 degrees in all seasons and 30 degrees
# in summer, which predicts the pressure at high mountain sites to within 1%
# where the ICAO standard atmosphere reads too low (read in full, 2026-11-19):
#     P_B (Torr) = exp(6.63268 - 0.1112 h - 0.00149 h^2),   h in km
# 759.5 Torr at 0 km by the equation (its Table 2 prints 759.2), 324.1 at
# 7 km (Table 2: 324.1 by the equation, 324.4 the mean of the two models),
# where the ICAO standard atmosphere gives 308.  Its 14 measured sites, 3,094
# to 8,848 m, were all within 1%.  West warns that other models should be
# used at latitudes of 45 degrees and above: in the Alps (46 N) the equation
# read 3 Torr high.  pressureRatio() divides by the equation's own sea
# level, so the ratio is exactly 1 at 0 m.
# -----------------------------------------------------------------------------

#' Altitude range, step and default, metres
#' @keywords internal
ALTITUDE_MIN <- 0
#' @rdname ALTITUDE_MIN
#' @keywords internal
ALTITUDE_MAX <- 7000
#' @rdname ALTITUDE_MIN
#' @keywords internal
ALTITUDE_STEP <- 100
#' @rdname ALTITUDE_MIN
#' @keywords internal
ALTITUDE_DEFAULT <- 0

#' West 1996's model atmosphere: ln P_B (Torr) = a + b h + c h^2, h in km
#' @keywords internal
ALTITUDE_WEST_COEFFICIENTS <- c(a = 6.63268, b = -0.1112, c = -0.00149)

#' Sea-level pressure, Torr: the unit of the gas engine's tensions
#' @keywords internal
SEA_LEVEL_PRESSURE <- 760

#' The vaporizers that deliver the dial's volume percent rather than its
#' sea-level partial pressure (the desflurane Tec 6; Andrews and Johnston 1993)
#' @keywords internal
ALTITUDE_VOLUME_PERCENT_VAPORIZERS <- "desflurane"

#' An altitude, checked
#' @param altitude metres above sea level, or NULL for sea level
#' @returns the altitude
#' @keywords internal
checkAltitude <- function(altitude)
{
  if (is.null(altitude)) return(ALTITUDE_DEFAULT)
  if (!is.numeric(altitude) || length(altitude) != 1 || !is.finite(altitude) ||
      altitude < ALTITUDE_MIN || altitude > ALTITUDE_MAX)
    stop("Invalid altitude: ", format(altitude), " (", ALTITUDE_MIN, " to ",
         ALTITUDE_MAX, " m)")
  altitude
}

#' Barometric pressure at an altitude (West 1996)
#'
#' @param altitude metres above sea level
#' @returns the barometric pressure, Torr
#' @export
barometricPressure <- function(altitude = ALTITUDE_DEFAULT)
{
  h <- checkAltitude(altitude) / 1000
  k <- ALTITUDE_WEST_COEFFICIENTS
  exp(k[["a"]] + k[["b"]] * h + k[["c"]] * h^2)
}

#' The barometric pressure as a fraction of a sea-level atmosphere
#'
#' West's equation gives 759.5 Torr at sea level, not exactly 760, so the
#' fraction is taken relative to the equation's own sea level: exactly 1 at
#' 0 m, so that nothing changes there.
#'
#' @param altitude metres above sea level
#' @returns P_B(altitude) / P_B(0)
#' @keywords internal
pressureRatio <- function(altitude = ALTITUDE_DEFAULT)
{
  barometricPressure(altitude) / barometricPressure(0)
}

#' The pressure ratio a gas body was built at
#' @param body output of \code{getGasBody()}, possibly with \code{altitude}
#' @returns P_B / 760, 1 at sea level
#' @keywords internal
gasPressureRatio <- function(body)
{
  if (is.null(body) || is.null(body$altitude)) return(1)
  pressureRatio(body$altitude)
}

#' The dry gas pressure in the lungs as a fraction of sea level's
#'
#' What turns an amount of gas (a partial pressure times a volume, in the
#' engine's sea-level unit) into the volume it takes up in the lungs: the
#' amount over this.  Alveolar gas is saturated, so its dry gases share
#' P_B - 47 Torr, which over sea level's 713 is the pressure ratio times
#' \code{gasHumidityRatio()}.  Until 2026-11-28 the uptake, the oxygen
#' consumed and the exhaled carbon dioxide were divided by the pressure ratio
#' alone, P_B / 760, which at sea level is the same thing.  Once the fresh gas
#' was counted in the lungs' litres (\code{gasFreshGasExpansion()}), which
#' carries the humidity, the two no longer matched at altitude: at 5000 m with
#' 0.5 L/min of oxygen the alveolar gases summed to 25.6\% of a sea-level
#' atmosphere where the dry pressure there is 52.3\%, and the alveolar oxygen
#' read 131 mmHg (Devin's review of PR 1013, 2026-11-28, reproduced).  A body
#' whose volumes are taken as stated keeps the pressure ratio.
#'
#' @param body the gas body
#' @returns the fraction; exactly 1 at sea level
#' @keywords internal
gasLungPressureRatio <- function(body)
{
  ratio <- gasPressureRatio(body)
  if (is.null(body) || isTRUE(body$volumesAsStated)) return(ratio)
  ratio * gasHumidityRatio(body)
}

#' Water vapour pressure in the airway at 37 C, Torr
#' @keywords internal
WATER_VAPOUR_PRESSURE <- 47

#' Torr per 100% of an alveolar tension
#'
#' The engine's alveolar tensions are fractions of the dry alveolar gas at
#' sea level: room air enters the lungs at 20.93% oxygen, and on pure oxygen
#' the alveolar gases sum to about 100 less the carbon dioxide, because the
#' sea-level humidification is already carried in the unit (MAC and every
#' sea-level validation are of humidified alveolar gas;
#' \code{gasHumidityRatio()}).  So 100% is the dry share of a sea-level
#' atmosphere, 760 - 47 = 713 Torr, and this is what an alveolar or arterial
#' tension is multiplied by to read in mmHg.  Until 2026-11-24 it was 760, so
#' the alveolar oxygen on pure oxygen read 722.7 mmHg, more than the 713 the
#' dry gas can hold, and 6.6% high at every altitude (Gemini's gas-engine
#' audit of 2026-11-23, C-06).  (The circuit's gas is dry: there 100% is 760
#' Torr.)
#' @keywords internal
GAS_ALVEOLAR_TORR <- SEA_LEVEL_PRESSURE - WATER_VAPOUR_PRESSURE

#' The dry share of the inspired gas, relative to sea level
#'
#' Inspired gas is humidified in the airway to the water vapour pressure,
#' which does not change with altitude, so the other gases share P_B - 47
#' Torr.  The engine's tensions are sea-level partial pressures, in which
#' the sea-level humidification is already carried (MAC and every sea-level
#' validation are of humidified alveolar gas), so only the CHANGE in the dry
#' share matters: w = ((P_B - 47) / P_B) / ((760 - 47) / 760), exactly 1 at
#' sea level and 0.91 at 7000 m.  Shafer, 2026-11-19: "add water vapour".
#' It multiplies what the fresh gas brings to the alveoli (gasSystemSoluble(),
#' gasSystemOxygen()), and the room air the patient starts in; gas rebreathed
#' from the patient was humidified in the airway, dried in the circuit and
#' humidified again, so for it the factor cancels.
#'
#' @param body a gas body
#' @returns w, 1 at sea level
#' @keywords internal
gasHumidityRatio <- function(body)
{
  if (is.null(body) || is.null(body$altitude)) return(1)
  P <- barometricPressure(body$altitude)
  P0 <- barometricPressure(0)
  ((P - WATER_VAPOUR_PRESSURE) / P) / ((P0 - WATER_VAPOUR_PRESSURE) / P0)
}

#' An altitude read from the slider; an unreadable value is sea level
#' @param value \code{input$physiology_altitude}
#' @returns metres
#' @keywords internal
altitudeFromInput <- function(value)
{
  v <- suppressWarnings(as.numeric(value))
  if (length(v) != 1 || !is.finite(v)) return(ALTITUDE_DEFAULT)
  min(max(v, ALTITUDE_MIN), ALTITUDE_MAX)
}

#' The generated help block `physiology-altitude-table`
#'
#' Each agent at four altitudes, simulated: the barometric pressure, the
#' inspired partial pressure (% of a sea-level atmosphere), the brain at 10
#' minutes as a multiple of MAC, and the alveolar oxygen.  Fresh gas equals the
#' minute ventilation, 6 L/min, so nothing is rebreathed.
#' @returns markdown table
#' @noRd
helpPhysiologyAltitudeTable <- function()
{
  if (!is.null(.physiologyCache$altitudeTable)) return(.physiologyCache$altitudeTable)
  agents <- c(sevoflurane = 2, desflurane = 6, nitrousOxide = 100 * 4 / 6)
  label <- c(sevoflurane = "Sevoflurane, dial 2%", desflurane = "Desflurane, dial 6%",
             nitrousOxide = "Nitrous oxide 4 L/min, oxygen 2 L/min")
  rows <- character(0)
  for (g in names(agents)) for (h in c(0, 2000, 4000, 7000)) {
    dt <- if (g == "nitrousOxide")
      data.frame(Drug = c(g, "oxygen", "ventilation"), Time = 0, Dose = c(4, 2, 6))
    else
      data.frame(Drug = c(g, "oxygen", "air", "ventilation"), Time = 0,
                 Dose = c(agents[[g]], 3, 3, 6))
    sim <- simulateGases(dt, weight = 70, age = 40, maximum = 10, altitude = h)
    n <- length(sim$timeLine)
    props <- gasPropertiesAt(NULL)
    mac <- macForAge(props$MAC40[props$gas == g], 40)
    rows <- c(rows, sprintf("| %s | %s | %.0f | %s | %s | %.0f |",
                            label[[g]], format(h, big.mark = ","),
                            barometricPressure(h),
                            formatC(sim$state[[g]][n, 1] * gasHumidityRatio(sim$body),
                                    digits = 1, format = "f"),
                            formatC(sim$state[[g]][n, 3] / mac, digits = 2, format = "f"),
                            sim$state$oxygen[n, 2] * GAS_ALVEOLAR_TORR / 100))
  }
  .physiologyCache$altitudeTable <- paste(c(
    "| Fresh gas | Altitude (m) | Barometric pressure (Torr) | Inspired, humidified (% of a sea-level atmosphere) | Brain at 10 min (MAC) | Alveolar oxygen at 10 min (Torr) |",
    "|---|---|---|---|---|---|",
    rows
  ), collapse = "\n")
}
