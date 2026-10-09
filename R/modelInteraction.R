# Propofol-opioid interaction for laryngoscopy
#
# Bouillon TW et al., Anesthesiology 2004;100:1353-1372, Table 4 (Bayesian
# predicted parameters).  The surface was fitted with remifentanil, so both
# arguments are concentrations: propofol in mcg/mL and the opioid as a
# remifentanil-equivalent effect-site concentration in ng/mL.  The MEAC panel's
# total opioid is a percentage of MEAC; remifentanilEquivalent() turns it into
# the second argument.  Until 2026-10-09 simulationPlot() passed the percentage
# itself, which made every opioid 100 times as potent as intended: 1 ng/mL of
# remifentanil with 3 mcg/mL of propofol read as P(response) 3e-9 rather than
# 0.27.
#
# Returns the probability of RESPONSE to laryngoscopy (1 minus the probability
# of no response that Bouillon modelled), which is what the panel plots.  The
# element names are historical: "pNR" is that probability of response.
modelInteraction <- function(propofol, remifentanil)
{
  # Bouillon Anesthesiology 2004, v100, p1360, Table 4, Bayesian Predicted
  ce50Remi <- 1.01
  ce50Prop <- 6.68
  steepnessRemi <- 0.72
  steepnessProp <- 6.9
  preopioidIntensity <- 0.83  # Laryngoscopy

  # Floored at zero: a concentration rounded to -1e-18 would otherwise give
  # NaN, a negative number to a fractional power.
  opioid <- pmax(remifentanil, 0)^steepnessRemi
  propofol <- pmax(propofol, 0)^steepnessProp

  # Both drugs together
  postopioidIntensity <- preopioidIntensity *
    (1 - opioid / (opioid + (ce50Remi * preopioidIntensity)^steepnessRemi))
  pNR <- 1 - propofol / (propofol + (ce50Prop * postopioidIntensity)^steepnessProp)

  # propofol only
  pNRpropofol <- 1 - propofol / (propofol + (ce50Prop * preopioidIntensity)^steepnessProp)

  # opioid only (have to have some propofol, or they will respond, based on the model)
  pNRopioid <- 1

  return(
    list(
      pNR = pNR,
      pNRpropofol = pNRpropofol,
      pNRopioid = pNRopioid
    )
  )
}

# The remifentanil-equivalent concentration, in ng/mL, of a total opioid
# expressed as a percentage of MEAC: each opioid is counted in multiples of its
# own MEAC (the MEAC panel's sum), and one multiple is remifentanil's MEAC.
# remifentanilMEAC is the drug library's value, 1 ng/mL unless it has been
# edited; with it, remifentanil alone maps back to its own concentration.
remifentanilEquivalent <- function(percentMEAC, remifentanilMEAC = 1)
{
  if (length(remifentanilMEAC) != 1 || !is.finite(remifentanilMEAC) ||
      remifentanilMEAC <= 0)
    remifentanilMEAC <- 1
  percentMEAC / 100 * remifentanilMEAC
}
