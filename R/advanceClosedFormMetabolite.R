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
# bolus, infusion and oral lines.  That duplication was the established shape
# of this part of the package until 2026-10-07, when the timeline and the dose
# lines were refactored out of all four routines into simulationTimeGrid() and
# doseLines() (R/simulationTimeGrid.R), so that the grid could scale with the
# length of the plot; each engine now only names its own knots and routes.
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
  if ((!is.null(dose$IM) && any(dose$IM)) || (!is.null(dose$IN) && any(dose$IN)) ||
      (!is.null(dose$SL) && any(dose$SL)) || (!is.null(dose$RA) && any(dose$RA)))
    stop("A drug with an active metabolite cannot yet be given intramuscularly, ",
         "intranasally, sublingually or by tissue injection (RA); only intravenous ",
         "and oral routes carry metabolite coefficients.")

  # Oral doses appear after their absorption lag.
  givenAt <- dose$Time
  if (hasPO) dose$Time[dose$PO] <- dose$Time[dose$PO] + pkSet$tlag_PO
  # The second oral depot (getDrugPK()), whose rows simCpCe() flags PO2, with
  # its own lag.
  hasPO2 <- !is.null(dose$PO2) && any(dose$PO2)
  if (hasPO2) dose$Time[dose$PO2] <- dose$Time[dose$PO2] + pkSet$tlag_PO2

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
  #
  # The fill between those points is scaled to the plot; see
  # R/simulationTimeGrid.R.  A pure prodrug has ke0 == 0, so the usual
  # ke0-based grid start is undefined; gridStart() falls back on the
  # metabolite's own ke0, which is what the plotted effect actually follows.
  before <- dose$Bolus
  if (hasPO) before <- before | dose$PO
  if (hasPO2) before <- before | dose$PO2
  timeLine <- simulationTimeGrid(
    c(dose$Time, dose$Time[before] - PRE_DOSE_OFFSET,
      givenAt[givenAt < dose$Time]),
    maximum,
    gridStart(pkSet$ke0, met$ke0)
  )
  L <- length(timeLine)
  doseNA <- rep(0, L)

  # The oral line is all zero when there is no oral dose, and an oral row is
  # never an infusion, which is what the loop this replaced did with hasPO.
  inputs    <- doseLines(dose, timeLine, c("PO", "PO2"))
  bolusLine <- inputs$bolus
  poLine    <- inputs$PO
  po2Line   <- inputs$PO2
  # Oral input to the parent's disposition states: both depots feed them.
  poIn <- function(prefix, j) {
    x <- pkSet[[paste0(prefix, "_coef_PO_", j)]] * poLine
    if (hasPO2) x <- x + pkSet[[paste0(prefix, "_coef_PO2_", j)]] * po2Line
    x
  }
  rate      <- inputs$rate
  dt        <- inputs$dt

  # ---- Parent ----

  l1_dt <- exp(-pkSet$lambda_1 * dt)
  l2_dt <- exp(-pkSet$lambda_2 * dt)
  l3_dt <- exp(-pkSet$lambda_3 * dt)

  p_state_l1 <- advanceStatePO(l1_dt,
                               pkSet$p_coef_bolus_l1 * bolusLine,
                               pkSet$p_coef_infusion_l1 * rate * (1 - l1_dt),
                               poIn("p", "l1"), doseNA, doseNA, L)
  p_state_l2 <- advanceStatePO(l2_dt,
                               pkSet$p_coef_bolus_l2 * bolusLine,
                               pkSet$p_coef_infusion_l2 * rate * (1 - l2_dt),
                               poIn("p", "l2"), doseNA, doseNA, L)
  p_state_l3 <- advanceStatePO(l3_dt,
                               pkSet$p_coef_bolus_l3 * bolusLine,
                               pkSet$p_coef_infusion_l3 * rate * (1 - l3_dt),
                               poIn("p", "l3"), doseNA, doseNA, L)

  Cp <- p_state_l1 + p_state_l2 + p_state_l3
  if (hasPO && pkSet$ka_PO > 0)
  {
    ka_dt <- exp(-pkSet$ka_PO * dt)
    Cp <- Cp + advanceStatePO(ka_dt, doseNA, doseNA,
                              pkSet$p_coef_PO_ka * poLine, doseNA, doseNA, L)
  }
  # The second depot's own state, kept apart for the time until threshold.
  p_state_ka2 <- doseNA
  if (hasPO2)
  {
    p_state_ka2 <- advanceStatePO(exp(-pkSet$ka_PO2 * dt), doseNA, doseNA,
                                  pkSet$p_coef_PO2_ka * po2Line, doseNA, doseNA, L)
    Cp <- Cp + p_state_ka2
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
        coefs$PO[k] * poLine + (if (is.null(coefs$PO2)) 0 else coefs$PO2[k] * po2Line),
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
  CmStates  <- if (hasCe) NULL else advance(met$coefs)

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

  # A metabolite drug with no effect site has its time until threshold timed
  # on its plasma instead (see "Which concentration is timed" in
  # R/recoveryStates.R), so the formed contribution then carries plasma states.
  # The receiving drug's own states are plasma states for the same reason, so
  # the fold adds like to like.
  metaboliteStates <- if (hasCe) {
    recoveryStateSet(timeLine, CemStates, metCoefs$lambda, pending)
  } else {
    recoveryStateSet(timeLine, CmStates, met$coefs$lambda, pending,
                     horizon = recoveryHorizonPlasma(maximum))
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
                                 poIn("e", "l1"), doseNA, doseNA, L)
    e_state_l2 <- advanceStatePO(l2_dt,
                                 pkSet$e_coef_bolus_l2 * bolusLine,
                                 pkSet$e_coef_infusion_l2 * rate * (1 - l2_dt),
                                 poIn("e", "l2"), doseNA, doseNA, L)
    e_state_l3 <- advanceStatePO(l3_dt,
                                 pkSet$e_coef_bolus_l3 * bolusLine,
                                 pkSet$e_coef_infusion_l3 * rate * (1 - l3_dt),
                                 poIn("e", "l3"), doseNA, doseNA, L)
    e_state_ke0 <- advanceStatePO(ke0_dt,
                                  pkSet$e_coef_bolus_ke0 * bolusLine,
                                  pkSet$e_coef_infusion_ke0 * rate * (1 - ke0_dt),
                                  poIn("e", "ke0"), doseNA, doseNA, L)
    states  <- list(e_state_l1, e_state_l2, e_state_l3, e_state_ke0)
    lambdas <- c(pkSet$lambda_1, pkSet$lambda_2, pkSet$lambda_3, pkSet$ke0)
    if (hasPO && pkSet$ka_PO > 0)
    {
      ka_dt <- exp(-pkSet$ka_PO * dt)
      states[[5]] <- advanceStatePO(ka_dt, doseNA, doseNA,
                                    pkSet$e_coef_PO_ka * poLine, doseNA, doseNA, L)
      lambdas <- c(lambdas, pkSet$ka_PO)
    }
    if (hasPO2)
    {
      states[[length(states) + 1]] <- advanceStatePO(
        exp(-pkSet$ka_PO2 * dt), doseNA, doseNA,
        pkSet$e_coef_PO2_ka * po2Line, doseNA, doseNA, L)
      lambdas <- c(lambdas, pkSet$ka_PO2)
    }
    recoveryStates <- recoveryStateSet(timeLine, states, lambdas, pending)
    Ce <- rowSums(recoveryStates$state)
  } else {
    # No effect site of its own -- a pure prodrug, or a drug like prednisone
    # given for what it forms -- so its own time until threshold, if it has a
    # threshold at all, is timed on its plasma.
    states  <- list(p_state_l1, p_state_l2, p_state_l3)
    lambdas <- c(pkSet$lambda_1, pkSet$lambda_2, pkSet$lambda_3)
    if (hasPO && pkSet$ka_PO > 0)
    {
      states[[4]] <- Cp - p_state_l1 - p_state_l2 - p_state_l3 - p_state_ka2
      lambdas <- c(lambdas, pkSet$ka_PO)
    }
    if (hasPO2)
    {
      states[[length(states) + 1]] <- p_state_ka2
      lambdas <- c(lambdas, pkSet$ka_PO2)
    }
    recoveryStates <- recoveryStateSet(timeLine, states, lambdas, pending,
                                       horizon = recoveryHorizonPlasma(maximum))
  }
  if (plotRecovery) {
    recovery <- recoveryFromStates(recoveryStates, emerge)
  } else {
    recoveryStates <- NULL
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
