# -----------------------------------------------------------------------------
# Endobronchial intubation and the inhaled anesthetics
# -----------------------------------------------------------------------------
# Steven L. Shafer, 2026-11-19: "add a checkbox for endobronchial intubation.
# The effects on anesthetic gas uptake and distribution are well documented
# in [the] chapter by Ted Eger", relevant only when inhaled agents are dosed.
# Drafted by Claude Code the same day.
#
# THE SOURCE.  Eger EI II and Shafer SL, "The evolution of pharmacokinetics",
# chapter 8 of Eger's scientific autobiography (manuscript, 2017), section
# "Ventilation/Perfusion Inequalities", read in full 2026-11-19:
#   "endobronchial intubation (placement of the tracheal tube into a main
#   bronchus) ... results in just ventilating the left or right lung.  The
#   ventilated lung gets twice the normal ventilation, while the other lung
#   isn't ventilated at all.  The non-ventilated lung is still perfused.  Since
#   no anesthetic is delivered, no anesthetic is taken up by the blood
#   perfusing that lung.  FA, as measured using end-tidal concentrations, and
#   Fa, the arterial concentration, will diverge."
# The divergence is a function of solubility: for a poorly soluble agent
# (nitrous oxide) FA approaches FI whatever the blood takes, doubling the
# ventilation of one lung cannot raise it past FI, and the arterial partial
# pressure is initially about half normal; for a highly soluble one (ether,
# methoxyflurane) doubling the ventilation doubles the delivery and exactly
# compensates, so the arterial rise is not delayed.  Eger and Severinghaus
# 1964 (Anesthesiology 25:620) predicted this with an analogue model, and
# Stoelting and Longnecker 1972 (Anesthesiology 36:352) confirmed it in dogs
# and people.
#
# THE MODEL, the chapter's idealisation:
#   * The ventilated lung receives the whole alveolar ventilation and holds
#     half the alveolar gas volume (ENDOBRONCHIAL_VENTILATED_VOLUME_SHARE).
#   * It is perfused by (1 - s) of the cardiac output, and the unventilated
#     lung by s = ENDOBRONCHIAL_SHUNT_FRACTION, a right-to-left shunt that
#     takes up no agent: blood leaves it at the mixed venous partial pressure.
#   * Arterial blood, which supplies every tissue, is the mixture
#         Fa = (1 - s) FA + s Fv,
#     still linear in the state, so the matrix exponential stays exact.
#   * Uptake, and with it the uptake coupling, is lambda (1 - s) Qco (FA - Fv).
# With s = 0 and the whole volume ventilated this is the engine unchanged.
#
# NOT MODELLED.  Hypoxic pulmonary vasoconstriction, which in practice
# diverts part of the blood from the unventilated lung, so the shunt is less
# than the half the chapter assumes; the lungs' unequal sizes (the right lung
# takes about 55% of the blood); the slow absorption of the gas trapped in the
# unventilated lung.  Arterial hypoxaemia IS shown (Shafer, 2026-11-19): the
# engine carries oxygen as gas only, and R/arterialOxygen.R reads the
# arterial oxygen off its alveolar oxygen through the shunt.  The shunt fraction was put
# to Shafer on 2026-11-19 and DECIDED the same day: keep the chapter's 50%.
# Reopened by Gemini's gas-engine audit of 2026-11-23 (C-05: hypoxic
# vasoconstriction lowers the shunt; its comparison was mostly planned
# one-lung ventilation in the lateral position, not an accidental mainstem
# intubation, and its 30% arithmetic did not reproduce) and DECIDED again on
# 2026-11-25: while the box is ticked a Shunt fraction slider is shown, 0 to
# 50%, starting at 50%, its maximum, so the user can lower it and watch the
# oxygenation improve.  Built 2026-11-25: input$physiology_shunt, in percent
# (shuntFromInput()), simulateGases(shunt =), and the physiology event's
# "shunt" field, a fraction, which a link and the Visualizer's arterial
# oxygen carry.  Lowering it stands in for hypoxic vasoconstriction; at 0 the
# unventilated lung takes no blood and only the ventilated lung's smaller
# alveolar volume remains of the tube.
# The volume share remains a [decision].
# -----------------------------------------------------------------------------

#' Share of cardiac output through the unventilated lung (Eger and Shafer's
#' chapter: the non-ventilated lung is still perfused, half the blood; kept
#' at 50% on Shafer's decision of 2026-11-19, without hypoxic pulmonary
#' vasoconstriction).  Since 2026-11-25 the default and the most the Shunt
#' fraction slider gives.
#' @keywords internal
ENDOBRONCHIAL_SHUNT_FRACTION <- 0.5

#' The Shunt fraction slider's step, percent of cardiac output
#' @keywords internal
ENDOBRONCHIAL_SHUNT_STEP_PERCENT <- 1

#' A shunt fraction, checked: NULL is the default, otherwise a number from 0
#' to ENDOBRONCHIAL_SHUNT_FRACTION
#' @param x the fraction of cardiac output, or NULL
#' @keywords internal
checkShunt <- function(x)
{
  if (is.null(x)) return(ENDOBRONCHIAL_SHUNT_FRACTION)
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < 0 ||
      x > ENDOBRONCHIAL_SHUNT_FRACTION)
    stop("Invalid shunt fraction: ", format(x), " (0 to ",
         ENDOBRONCHIAL_SHUNT_FRACTION, ")")
  unname(x)
}

#' The Shunt fraction slider, read: its percent as a fraction, the default
#' when unreadable (a link is untrusted), held to the slider's range
#' @param value \code{input$physiology_shunt}, percent
#' @keywords internal
shuntFromInput <- function(value)
{
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value))
    return(ENDOBRONCHIAL_SHUNT_FRACTION)
  max(0, min(100 * ENDOBRONCHIAL_SHUNT_FRACTION, value)) / 100
}

#' Share of the alveolar gas volume in the ventilated lung
#' @keywords internal
ENDOBRONCHIAL_VENTILATED_VOLUME_SHARE <- 0.5

#' The gas body with one lung intubated
#'
#' @param body output of \code{getGasBody()}, possibly through
#'   \code{gasBodyPhysiology()}
#' @param endobronchial TRUE for an endobronchial tube
#' @param shunt the share of cardiac output through the unventilated lung,
#'   0 to ENDOBRONCHIAL_SHUNT_FRACTION (the default)
#' @returns the body, with \code{shunt} set and the alveolar volume that of
#'   the ventilated lung when \code{endobronchial} is TRUE
#' @keywords internal
gasBodyEndobronchial <- function(body, endobronchial = FALSE,
                                 shunt = ENDOBRONCHIAL_SHUNT_FRACTION)
{
  if (!isTRUE(endobronchial)) return(body)
  body$shunt <- checkShunt(shunt)
  body$V_alveolar <- body$V_alveolar * ENDOBRONCHIAL_VENTILATED_VOLUME_SHARE
  body$endobronchial <- TRUE
  body
}

#' The share of cardiac output that bypasses the ventilated alveoli
#' @param body a gas body
#' @returns 0, or the shunt fraction of an endobronchial tube
#' @keywords internal
gasShunt <- function(body)
{
  if (is.null(body) || is.null(body$shunt)) return(0)
  body$shunt
}

#' Arterial partial pressure for one soluble gas
#'
#' The ventilated alveoli's blood, at the alveolar tension, mixed with the
#' shunt's, at the mixed venous tension; the alveolar tension when nothing is
#' shunted.
#'
#' @param y state vector for one soluble gas
#' @param body a gas body
#' @returns arterial tension, percent of a sea-level atmosphere
#' @keywords internal
gasArterial <- function(y, body)
{
  s <- gasShunt(body)
  if (s == 0) return(y[[2]])
  (1 - s) * y[[2]] + s * gasMixedVenous(y, body)
}

#' The endobronchial intubation box, read: TRUE only for a TRUE
#' @param value \code{input$endobronchialIntubation}
#' @keywords internal
endobronchialFromInput <- function(value) isTRUE(value)
