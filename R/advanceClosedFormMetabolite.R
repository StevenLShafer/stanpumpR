# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer, for drugs whose effect is mediated by an active metabolite.
# Extended 2026-10-05 to carry oral doses, including metabolite formed during
# first pass through the liver.
#
# STATUS: run and verified on R 4.6.1 by tests/testthat/test-metabolite.R and
# tests/testthat/test-advance-metabolite.R.  The parent columns are asserted to
# be identical to advanceClosedForm0()'s and advanceClosedFormPO_IM_IN()'s, and
# the metabolite columns are checked against the closed form in
# metaboliteCoefficients.R.
# -----------------------------------------------------------------------------
#
# This is a fourth sibling of advanceClosedForm0 / advanceClosedForm1 /
# advanceClosedFormPO_IM_IN.  Like them it builds its own timeline and its own
# bolus, infusion and oral lines; that duplication is the established shape of
# this part of the package, and the alternative -- refactoring the timeline out
# of the routines that carry the whole intravenous path -- is a change worth
# making on its own rather than as a side effect of adding metabolites.
#
# The parent columns are computed exactly as the sibling routines compute them,
# so a drug with a metabolite gives the same Cp and Ce it would without one.
#
# The metabolite is a sum of exponentials over the union of the parent's and the
# metabolite's eigenvalues, plus the absorption constant when there is an oral
# route (see metaboliteCoefficients.R for the derivation).  That means it
# advances through the same advanceStatePO() the parent uses, with no new
# machinery.  Its effect site is one further exponential at the metabolite's own
# ke0, advanced the same way -- the metabolite has its own effect site because
# its effect, not the parent's, is what matters clinically.  Codeine is the
# clearest case: the analgesia is morphine's.
#
# Both effect sites, the parent's and the metabolite's, are carried as states
# and read off as their sum, rather than derived from the plasma curve by
# calculateCe().  That keeps them exact, and it is what lets the metabolite
# drug's row report a time until threshold for the metabolite it actually
# has, formed and given together; see R/recoveryStates.R.
#
# INTRAMUSCULAR AND INTRANASAL ARE NOT SUPPORTED HERE
# ---------------------------------------------------
# Only the intravenous and oral routes carry metabolite coefficients.  No drug
# with a metabolite offers IM or IN units today, and silently dropping the
# metabolite for those doses would be worse than refusing them, so they raise.
#
# A NOTE ON THE PRODRUG EFFECT SITE
# ---------------------------------
# A pure prodrug returns NA for its own effect site (see the ke0 guard below).
# That is right for the PLOTTED series: simulationPlot() drops NA rows, so
# codeine is drawn as plasma only and morphine carries the effect, which is the
# specified behaviour.  simCpCe() substitutes zero for the derived scalars,
# which cannot carry NA; see the guards there.
# -----------------------------------------------------------------------------


#' Simulate a parent drug together with an active metabolite
#'
#' @param dose the drug's dose-table rows, already unit-converted, with
#'   \code{Bolus}, \code{PO}, \code{IM} and \code{IN} columns
#' @param pkSet the parent's PK set, which must carry a \code{metabolite}
#'   element holding \code{coefs} (from \code{metaboliteCoefficients()}),
#'   \code{ke0} and \code{name}
#' @param maximum end of the simulation in minutes
#' @param plotRecovery should recovery be calculated (parent only)
#' @param emerge emergence threshold passed to \code{recoveryCalc()}
#'
#' @returns a data frame of \code{Time}, \code{Cp}, \code{Ce},
#'   \code{CpMetabolite}, \code{CeMetabolite} and \code{Recovery}, carrying
#'   the \code{recoveryStates} and \code{metaboliteRecoveryStates} attributes
#'   that \code{simCpCe()} lifts off for \code{foldMetabolites()}
#' @export
advanceClosedFormMetabolite <- function(dose, pkSet, maximum, plotRecovery, emerge)
{
  met <- pkSet$metabolite
  if (is.null(met)) stop("advanceClosedFormMetabolite() needs pkSet$metabolite")

  hasPO <- !is.null(dose$PO) && any(dose$PO)
  if ((!is.null(dose$IM) && any(dose$IM)) || (!is.null(dose$IN) && any(dose$IN)))
    stop("A drug with an active metabolite cannot yet be given intramuscularly ",
         "or intranasally; only intravenous and oral routes carry metabolite ",
         "coefficients.")

  # Oral doses appear after their absorption lag.
  givenAt <- dose$Time
  if (hasPO) dose$Time[dose$PO] <- dose$Time[dose$PO] + pkSet$tlag_PO

  # Timeline: dose times, the instant before each dose, and a geometric fill so
  # the early curvature is drawn smoothly.
  #
  # The instant-before point is needed for ORAL doses as well as boluses, even
  # though an oral dose does not make the concentration jump.  Recovery does
  # jump at every dose, and without a grid point just before the next one, the
  # plotted series interpolates straight across that jump and reports a time
  # until threshold that is far too long.  advanceClosedFormPO_IM_IN() takes
  # the same precaution for the same reason.
  # A lagged dose also contributes the instant it was GIVEN, so that the window
  # over which recovery cannot be reported starts exactly where it should; see
  # pendingDoseTimes().  Only when some dose is actually lagged, so an unlagged
  # run keeps the line it always had.
  before <- dose$Bolus
  if (hasPO) before <- before | dose$PO
  timeLine <- sort(unique(c(0, dose$Time, dose$Time[before] - .01,
                            givenAt[givenAt < dose$Time], maximum)))
  timeLine <- timeLine[timeLine >= 0]

  gapStart <- timeLine[1:length(timeLine) - 1]
  gapEnd   <- timeLine[2:length(timeLine)]
  # A pure prodrug has ke0 == 0, so the usual ke0-based grid start is undefined.
  # Fall back on the metabolite's own ke0, which is what the plotted effect
  # actually follows.
  gridKe0 <- if (pkSet$ke0 > 0) pkSet$ke0 else met$ke0
  start <- if (!is.null(gridKe0) && gridKe0 > 0) min(0.693 / gridKe0 / 4, 1) else 1
  newTimes <- c(exp(log(start) + 0:40 * log(MINS_PER_DAY / start) / 41))
  for (i in 1:length(gapEnd))
  {
    distance <- gapEnd[i] - gapStart[i]
    timeLine <- c(timeLine, gapStart[i] + newTimes[newTimes <= distance])
  }
  timeLine <- sort(unique(timeLine))
  L <- length(timeLine)
  doseNA <- rep(0, L)

  bolusLine <- infusionLine <- poLine <- dt <- rate <- doseNA
  for (i in 1:L)
  {
    bolusLine[i] <- sum(dose$Dose[dose$Time == timeLine[i] & dose$Bolus])
    if (hasPO) poLine[i] <- sum(dose$Dose[dose$Time == timeLine[i] & dose$PO])
    USE <- dose$Time == timeLine[i] & !dose$Bolus
    if (hasPO) USE <- USE & !dose$PO
    if (i == 1)
    {
      infusionLine[i] <- sum(dose$Dose[USE])
      rate[1] <- 0
      dt[1]   <- 0
    } else {
      if (sum(USE) == 0)
      {
        infusionLine[i] <- infusionLine[i - 1]
      } else {
        infusionLine[i] <- sum(dose$Dose[USE])
      }
      dt[i]   <- timeLine[i] - timeLine[i - 1]
      rate[i] <- infusionLine[i - 1]
    }
  }

  # ---- Parent ----

  l1_dt <- exp(-pkSet$lambda_1 * dt)
  l2_dt <- exp(-pkSet$lambda_2 * dt)
  l3_dt <- exp(-pkSet$lambda_3 * dt)

  p_state_l1 <- advanceStatePO(l1_dt,
                               pkSet$p_coef_bolus_l1 * bolusLine,
                               pkSet$p_coef_infusion_l1 * rate * (1 - l1_dt),
                               pkSet$p_coef_PO_l1 * poLine, doseNA, doseNA, L)
  p_state_l2 <- advanceStatePO(l2_dt,
                               pkSet$p_coef_bolus_l2 * bolusLine,
                               pkSet$p_coef_infusion_l2 * rate * (1 - l2_dt),
                               pkSet$p_coef_PO_l2 * poLine, doseNA, doseNA, L)
  p_state_l3 <- advanceStatePO(l3_dt,
                               pkSet$p_coef_bolus_l3 * bolusLine,
                               pkSet$p_coef_infusion_l3 * rate * (1 - l3_dt),
                               pkSet$p_coef_PO_l3 * poLine, doseNA, doseNA, L)

  Cp <- p_state_l1 + p_state_l2 + p_state_l3
  if (hasPO && pkSet$ka_PO > 0)
  {
    ka_dt <- exp(-pkSet$ka_PO * dt)
    Cp <- Cp + advanceStatePO(ka_dt, doseNA, doseNA,
                              pkSet$p_coef_PO_ka * poLine, doseNA, doseNA, L)
  }

  # The parent's effect site is worked out below, from its own states,
  # together with its recovery.

  # ---- Metabolite ----
  #
  # One exponential state per term in the union of the parent's eigenvalues, the
  # metabolite's, and the absorption constant.  Each is driven by the same
  # bolus, infusion and oral input as the parent, because the coefficients
  # already carry the whole parent-to-metabolite convolution as well as the
  # first-pass branch.

  advance <- function(coefs)
  {
    states <- matrix(0, L, length(coefs$lambda))
    for (k in seq_along(coefs$lambda))
    {
      lk_dt <- exp(-coefs$lambda[k] * dt)
      states[, k] <- advanceStatePO(
        lk_dt,
        coefs$bolus[k] * bolusLine,
        coefs$infusion[k] * rate * (1 - lk_dt),
        coefs$PO[k] * poLine,
        doseNA, doseNA, L
      )
    }
    states
  }

  Cm <- rowSums(advance(met$coefs))

  # The metabolite's effect site, as one state per eigenvalue rather than as a
  # single curve.  It used to come from calculateCe(), which interpolates the
  # plasma concentration between time points; the closed form is exact, and --
  # the reason it is worth the change -- it leaves the amplitudes in hand, which
  # is what the metabolite drug's row needs to work out its own time until
  # threshold once this contribution has been folded in.  See
  # R/recoveryStates.R and R/mergeMetabolite.R.
  #
  # The metabolite drug may itself have no effect site, either because it is
  # another prodrug or because its potency has not been supplied yet.  Then NA,
  # not the plasma concentration, for the same reason as the parent above: the
  # plotted row shows plasma only rather than an effect-site line that is really
  # a mislabelled copy of it, and the merge keeps the receiving drug's own
  # convention instead of mixing NA with a number.  There are no effect-site
  # states to carry either, so foldMetabolites() leaves that row's time until
  # threshold alone rather than solving a problem that has no answer.
  hasCe <- !is.null(met$ke0) && met$ke0 > 0
  metCoefs  <- if (hasCe) effectSiteCoefficients(met$coefs, met$ke0) else NULL
  CemStates <- if (hasCe) advance(metCoefs) else NULL
  Cem <- if (hasCe) rowSums(CemStates) else rep(NA_real_, L)

  # Floating point can leave either sum a hair below zero at t = 0, where the
  # coefficients cancel exactly.  A negative concentration is meaningless.  The
  # states are left alone: recoveryCalc() reads them as a signed sum, and
  # clipping one would stop them adding up to the curve.
  Cm[Cm < 0] <- 0
  if (hasCe) Cem[Cem < 0] <- 0

  # The parent's pending doses leave the METABOLITE's row unable to report a
  # time too: metabolite certain to be formed from a dose that has not started
  # absorbing is missing from these amplitudes just as the parent's own is.
  pending <- pendingDoseTimes(givenAt, dose$Time, dose$Dose, timeLine)

  metaboliteStates <- if (hasCe) {
    recoveryStateSet(timeLine, CemStates, metCoefs$lambda, pending)
  } else {
    NULL
  }

  # ---- Parent effect site and recovery ----
  #
  # The parent's own effect site, which a pure prodrug does not have, as the
  # sum of its own exponential states: the same closed form as
  # advanceClosedForm0() and advanceClosedFormPO_IM_IN(), so the parent's
  # columns are identical with or without a metabolite attached.  A pure
  # prodrug -- codeine, tramadol -- carries no tPeak, so getDrugPK leaves ke0
  # at zero and there are no effect-site states to sum: NA rather than zero,
  # because simulationPlot() drops NA rows and the parent is then plotted as
  # plasma only, instead of carrying a meaningless flat effect-site line along
  # the axis.  The metabolite's time
  # until threshold is not computed here: it belongs to the metabolite drug's
  # row, where foldMetabolites() works it out from the states above together
  # with whatever of that drug was given directly.
  recoveryStates <- NULL
  recovery <- doseNA
  Ce <- rep(NA_real_, L)
  if (pkSet$ke0 > 0)
  {
    ke0_dt <- exp(-pkSet$ke0 * dt)
    e_state_l1 <- advanceStatePO(l1_dt,
                                 pkSet$e_coef_bolus_l1 * bolusLine,
                                 pkSet$e_coef_infusion_l1 * rate * (1 - l1_dt),
                                 pkSet$e_coef_PO_l1 * poLine, doseNA, doseNA, L)
    e_state_l2 <- advanceStatePO(l2_dt,
                                 pkSet$e_coef_bolus_l2 * bolusLine,
                                 pkSet$e_coef_infusion_l2 * rate * (1 - l2_dt),
                                 pkSet$e_coef_PO_l2 * poLine, doseNA, doseNA, L)
    e_state_l3 <- advanceStatePO(l3_dt,
                                 pkSet$e_coef_bolus_l3 * bolusLine,
                                 pkSet$e_coef_infusion_l3 * rate * (1 - l3_dt),
                                 pkSet$e_coef_PO_l3 * poLine, doseNA, doseNA, L)
    e_state_ke0 <- advanceStatePO(ke0_dt,
                                  pkSet$e_coef_bolus_ke0 * bolusLine,
                                  pkSet$e_coef_infusion_ke0 * rate * (1 - ke0_dt),
                                  pkSet$e_coef_PO_ke0 * poLine, doseNA, doseNA, L)
    states  <- list(e_state_l1, e_state_l2, e_state_l3, e_state_ke0)
    lambdas <- c(pkSet$lambda_1, pkSet$lambda_2, pkSet$lambda_3, pkSet$ke0)
    if (hasPO && pkSet$ka_PO > 0)
    {
      ka_dt <- exp(-pkSet$ka_PO * dt)
      states[[5]] <- advanceStatePO(ka_dt, doseNA, doseNA,
                                    pkSet$e_coef_PO_ka * poLine, doseNA, doseNA, L)
      lambdas <- c(lambdas, pkSet$ka_PO)
    }
    recoveryStates <- recoveryStateSet(timeLine, states, lambdas, pending)
    Ce <- rowSums(recoveryStates$state)
    if (plotRecovery) {
      recovery <- recoveryFromStates(recoveryStates, emerge)
    } else {
      recoveryStates <- NULL
    }
  }

  results <- data.frame(
    Time         = timeLine,
    Cp           = Cp,
    Ce           = Ce,
    CpMetabolite = Cm,
    CeMetabolite = Cem,
    Recovery     = recovery
  )
  attr(results, "recoveryStates")           <- recoveryStates
  attr(results, "metaboliteRecoveryStates") <- metaboliteStates
  results
}
