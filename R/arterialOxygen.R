# -----------------------------------------------------------------------------
# Arterial oxygen, and the hypoxaemia of an endobronchial tube
# -----------------------------------------------------------------------------
# Steven L. Shafer, 2026-11-19: "Arterial oxygen should be modeled when there
# is an endobronchial intubation.  Roughly 50% of the blood is shunted
# (initially) through the non-ventilated lung.  The ventilated lung gets twice
# as much air.  Because CO2 is freely soluble in blood, doubling the
# ventilation to the ventilated lung mostly compensates for the lack of
# ventilation in the lung that isn't ventilated.  However, that doesn't apply
# to oxygen because it is poorly soluble in blood (which is why we have red
# blood cells).  Doubling the ventilation in the ventilated lung adds a tiny
# amount of oxygen on that side, but nowhere nearly enough to compensate for
# the shunt fraction in the non-ventilated lung.  Hypoxia will necessarily
# result."  Drafted by Claude Code the same day.
#
# THE GAS ENGINE carries oxygen as gas only: the alveolar oxygen tension, a
# constant consumption VO2 drawn from the alveoli (R/advanceClosedFormGas.R,
# (4)).  That is unchanged.  What this file adds is the BLOOD, read off the
# engine's alveolar oxygen at each time:
#
#   * blood leaving the ventilated alveoli (end-capillary) is at the alveolar
#     oxygen tension PAO2, with content
#         Cc'O2 = 1.34 Hb S(PAO2) + 0.003 PAO2          (mL O2/dL)
#     S() the haemoglobin saturation from Severinghaus 1979's equation 1
#     (J Appl Physiol 46:599, PMID 35496, the abstract read 2026-11-19:
#     S = (23400 / (PO2^3 + 150 PO2) + 1)^-1, within 0.0055 of the standard
#     human curve at 37 C and pH 7.4), 1.34 mL O2 per g of haemoglobin, and
#     0.003 mL/dL/mmHg dissolved;
#   * the shunt, the share s of cardiac output through the unventilated lung
#     (gasShunt(), R/endobronchial.R: 0.5 with an endobronchial tube), leaves
#     at the mixed venous content CvO2;
#   * arterial blood is their mixture, CaO2 = (1 - s) Cc'O2 + s CvO2, and
#     the tissues take VO2 from it, CvO2 = CaO2 - VO2 / Q (Fick).  Solving the
#     two together,
#         CaO2 = Cc'O2 - (s / (1 - s)) VO2 / Q
#     (VO2 in mL/min, Q in dL/min), so the shunt subtracts its share of the
#     whole body's oxygen use from what the ventilated lung loads.  This is
#     the steady state: blood's own oxygen store is not followed, so a step in
#     ventilation or the tube reaches the arterial blood at once, where a
#     patient takes a circulation time or two.
#   * PaO2 is the tension whose content is CaO2 (inverted numerically), and
#     SaO2 its saturation.  With no shunt, CaO2 = Cc'O2 and PaO2 = PAO2 to
#     the last digit.
#
# That is the arithmetic of Shafer's sentence: doubling the ventilated lung's
# ventilation raises its PAO2, but haemoglobin leaving it is already nearly
# saturated, so its content barely rises (the 0.003 mL/dL/mmHg of dissolved
# oxygen is all that more ventilation buys), while the shunted half arrives
# at mixed venous content.  Carbon dioxide, whose content rises nearly in
# proportion to its tension, is not modelled here (the engine has no CO2 in
# blood); its compensation is the reason the patient is hypoxaemic but not
# hypercapnic.
#
# NOT MODELLED.  Hypoxic pulmonary vasoconstriction, which over minutes
# diverts blood from the unventilated lung, so the shunt falls from about
# half (Shafer: "initially"); the haemoglobin, held at
# ARTERIAL_OXYGEN_HAEMOGLOBIN for every patient, there being no Patient
# Profile field; the dissociation curve's shift with temperature and pH
# (Severinghaus's equations 3 and 4), so at 32 C the saturation read is the
# 37 C curve's; and oxygen in blood feeding back into the gas engine, whose
# oxygen uptake stays VO2.
# -----------------------------------------------------------------------------

#' Haemoglobin used for arterial oxygen content, g/dL
#'
#' [decision] for Shafer (2026-11-19): one value for every patient, there
#' being no haemoglobin in the Patient Profile; 15 g/dL, a normal adult man.
#' @keywords internal
ARTERIAL_OXYGEN_HAEMOGLOBIN <- 15

#' Oxygen carried per gram of haemoglobin, mL O2/g (Hufner's number as used
#' clinically)
#' @keywords internal
OXYGEN_HUFNER <- 1.34

#' Oxygen dissolved in blood, mL O2/dL/mmHg
#' @keywords internal
OXYGEN_DISSOLVED <- 0.003

#' Haemoglobin saturation from PO2 (Severinghaus 1979, equation 1)
#' @param po2 oxygen tension, mmHg
#' @returns fractional saturation, 0 to 1
#' @keywords internal
oxygenSaturation <- function(po2)
{
  po2 <- pmax(po2, 0)
  out <- 1 / (23400 / (po2^3 + 150 * po2) + 1)
  out[po2 == 0] <- 0
  out
}

#' Blood oxygen content, mL O2/dL
#' @param po2 oxygen tension, mmHg
#' @param hb haemoglobin, g/dL
#' @keywords internal
oxygenContent <- function(po2, hb = ARTERIAL_OXYGEN_HAEMOGLOBIN)
{
  OXYGEN_HUFNER * hb * oxygenSaturation(po2) + OXYGEN_DISSOLVED * pmax(po2, 0)
}

#' The oxygen tension of a content
#' @param content mL O2/dL
#' @param hb haemoglobin, g/dL
#' @param upper a tension known to hold at least this content, mmHg
#' @returns mmHg, 0 for a content of 0 or less
#' @keywords internal
oxygenTensionOfContent <- function(content, hb = ARTERIAL_OXYGEN_HAEMOGLOBIN, upper = 800)
{
  vapply(seq_along(content), function(i) {
    c <- content[[i]]
    if (!is.finite(c) || c <= 0) return(0)
    hi <- max(upper[[min(i, length(upper))]], 1)
    while (oxygenContent(hi, hb) < c) hi <- hi * 2
    stats::uniroot(function(p) oxygenContent(p, hb) - c, c(0, hi),
                   tol = 1e-10)$root
  }, numeric(1))
}

#' Arterial oxygen at each time of a gas run
#'
#' @param alveolar the engine's alveolar oxygen, % of a sea-level atmosphere
#'   (state 2 of oxygen)
#' @param shunt the shunt fraction at each time (gasShunt())
#' @param Qco cardiac output at each time, L/min
#' @param VO2 oxygen consumption at each time, L/min (a sea-level volume; 0
#'   where the engine has stopped consuming)
#' @param hb haemoglobin, g/dL
#' @returns a data frame: PAO2 and PaO2 (mmHg), SaO2 (%), and arterial, the
#'   PaO2 as % of a sea-level atmosphere, the unit of the oxygen panel
#' @keywords internal
arterialOxygen <- function(alveolar, shunt, Qco, VO2, hb = ARTERIAL_OXYGEN_HAEMOGLOBIN)
{
  n <- length(alveolar)
  shunt <- rep_len(shunt, n)
  Qco <- rep_len(Qco, n)
  VO2 <- rep_len(VO2, n)
  PAO2 <- pmax(alveolar, 0) / 100 * 760
  PaO2 <- PAO2
  s <- which(shunt > 0)
  if (length(s) > 0) {
    cc <- oxygenContent(PAO2[s], hb)
    # (s / (1 - s)) VO2 / Q, mL/min over dL/min
    ca <- cc - shunt[s] / (1 - shunt[s]) * (1000 * VO2[s]) / (10 * Qco[s])
    PaO2[s] <- oxygenTensionOfContent(ca, hb, upper = PAO2[s])
  }
  data.frame(PAO2 = PAO2, PaO2 = PaO2, SaO2 = 100 * oxygenSaturation(PaO2),
             arterial = PaO2 / 760 * 100)
}

#' Arterial oxygen of a simulateGases() run, at its own times
#' @param sim simulateGases()'s value
#' @returns arterialOxygen()'s data frame with Time, or NULL without oxygen
#' @keywords internal
gasArterialOxygen <- function(sim)
{
  ox <- sim$state[["oxygen"]]
  if (is.null(ox) || is.null(sim$timeLine)) return(NULL)
  n <- nrow(ox)
  bodies <- if (!is.null(sim$bodySteps) && !is.null(sim$bodyRow))
    sim$bodySteps$body[sim$bodyRow] else rep(list(sim$body), n)
  shunt <- vapply(bodies, gasShunt, numeric(1))
  Qco <- vapply(bodies, function(b)
    if (!is.null(sim$cardiacOutput)) sim$cardiacOutput
    else if (is.null(sim$bodySteps) && !is.null(sim$Qco)) sim$Qco
    else b$Q_cardiac, numeric(1))
  # The engine stops consuming oxygen once the alveoli hold none
  VO2 <- vapply(bodies, function(b) b$VO2, numeric(1)) * (ox[, 2] > 0)
  out <- arterialOxygen(ox[, 2], shunt, Qco, VO2)
  cbind(Time = sim$timeLine, out)
}
