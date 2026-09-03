# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer, for drugs whose effect is mediated by an active metabolite.
#
# STATUS: run and verified on R 4.6.1 by tests/testthat/test-metabolite.R and
# tests/testthat/test-advance-metabolite.R.  The parent columns are asserted to
# be identical to advanceClosedForm0()'s, and the metabolite columns are checked
# against the closed form in metaboliteCoefficients.R.
# -----------------------------------------------------------------------------
#
# This is a fourth sibling of advanceClosedForm0 / advanceClosedForm1 /
# advanceClosedFormPO_IM_IN.  Like them it builds its own timeline and its own
# bolus and infusion lines; that duplication is the established shape of this
# part of the package, and the alternative -- refactoring the timeline out of
# the three routines that carry the whole intravenous path -- is a change worth
# making on its own rather than as a side effect of adding metabolites.
#
# The parent columns are computed exactly as advanceClosedForm0 computes them,
# so a drug with a metabolite gives the same Cp and Ce it would without one.
#
# The metabolite is a sum of exponentials over the union of the parent's and the
# metabolite's eigenvalues (see metaboliteCoefficients.R for the derivation),
# which means it advances through the same advanceState() the parent uses, with
# no new machinery.  Its effect site then comes from calculateCe() applied to
# the metabolite concentration with the metabolite's own ke0 -- the metabolite
# has its own effect site because its effect, not the parent's, is what matters
# clinically.  Codeine is the clearest case: the analgesia is morphine's.
# -----------------------------------------------------------------------------


#' Simulate a parent drug together with an active metabolite
#'
#' @param dose the drug's dose-table rows, already unit-converted, with a
#'   \code{Bolus} column
#' @param pkSet the parent's PK set, which must carry a \code{metabolite}
#'   element holding \code{coefs} (from \code{metaboliteCoefficients()}),
#'   \code{ke0} and \code{name}
#' @param maximum end of the simulation in minutes
#' @param plotRecovery should recovery be calculated (parent only)
#' @param emerge emergence threshold passed to \code{recoveryCalc()}
#'
#' @returns a data frame of \code{Time}, \code{Cp}, \code{Ce},
#'   \code{CpMetabolite}, \code{CeMetabolite} and \code{Recovery}
#' @export
advanceClosedFormMetabolite <- function(dose, pkSet, maximum, plotRecovery, emerge)
{
  met <- pkSet$metabolite
  if (is.null(met)) stop("advanceClosedFormMetabolite() needs pkSet$metabolite")

  # Timeline, built as in advanceClosedForm0: dose times, the instant before
  # each bolus, and a geometric fill so the early curvature is drawn smoothly.
  timeLine <- sort(unique(c(0, dose$Time, dose$Time[dose$Bolus] - .01, maximum)))
  timeLine <- timeLine[timeLine >= 0]

  gapStart <- timeLine[1:length(timeLine) - 1]
  gapEnd   <- timeLine[2:length(timeLine)]
  start <- min(0.693 / pkSet$ke0 / 4, 1)
  newTimes <- c(exp(log(start) + 0:40 * log(1440 / start) / 41))
  for (i in 1:length(gapEnd))
  {
    distance <- gapEnd[i] - gapStart[i]
    timeLine <- c(timeLine, gapStart[i] + newTimes[newTimes <= distance])
  }
  timeLine <- sort(unique(timeLine))
  L <- length(timeLine)

  bolusLine <- infusionLine <- dt <- rate <- rep(0, L)
  for (i in 1:L)
  {
    bolusLine[i] <- sum(dose$Dose[dose$Time == timeLine[i] & dose$Bolus])
    USE <- dose$Time == timeLine[i] & !dose$Bolus
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

  # ---- Parent, exactly as advanceClosedForm0 ----

  l1_dt <- exp(-pkSet$lambda_1 * dt)
  l2_dt <- exp(-pkSet$lambda_2 * dt)
  l3_dt <- exp(-pkSet$lambda_3 * dt)

  p_state_l1 <- advanceState(l1_dt,
                             pkSet$p_coef_bolus_l1 * bolusLine,
                             pkSet$p_coef_infusion_l1 * rate * (1 - l1_dt), 0, L)
  p_state_l2 <- advanceState(l2_dt,
                             pkSet$p_coef_bolus_l2 * bolusLine,
                             pkSet$p_coef_infusion_l2 * rate * (1 - l2_dt), 0, L)
  p_state_l3 <- advanceState(l3_dt,
                             pkSet$p_coef_bolus_l3 * bolusLine,
                             pkSet$p_coef_infusion_l3 * rate * (1 - l3_dt), 0, L)

  Cp <- p_state_l1 + p_state_l2 + p_state_l3
  Ce <- calculateCe(Cp, rep(pkSet$ke0, L), dt, L)

  # ---- Metabolite ----
  #
  # One exponential state per term in the union of the parent's and the
  # metabolite's eigenvalues.  Each is driven by the same bolus and infusion
  # input as the parent, because the coefficients already carry the whole
  # parent-to-metabolite convolution.

  Cm <- rep(0, L)
  for (k in seq_along(met$coefs$lambda))
  {
    lk_dt <- exp(-met$coefs$lambda[k] * dt)
    Cm <- Cm + advanceState(
      lk_dt,
      met$coefs$bolus[k] * bolusLine,
      met$coefs$infusion[k] * rate * (1 - lk_dt),
      0, L
    )
  }

  # Floating point can leave the sum a hair below zero at t = 0, where the
  # coefficients cancel exactly.  A negative concentration is meaningless.
  Cm[Cm < 0] <- 0

  Cem <- if (!is.null(met$ke0) && met$ke0 > 0) {
    calculateCe(Cm, rep(met$ke0, L), dt, L)
  } else {
    Cm
  }

  # ---- Recovery, parent only ----

  if (plotRecovery)
  {
    ke0_dt <- exp(-pkSet$ke0 * dt)
    e_state_l1 <- advanceState(l1_dt,
                               pkSet$e_coef_bolus_l1 * bolusLine,
                               pkSet$e_coef_infusion_l1 * rate * (1 - l1_dt), 0, L)
    e_state_l2 <- advanceState(l2_dt,
                               pkSet$e_coef_bolus_l2 * bolusLine,
                               pkSet$e_coef_infusion_l2 * rate * (1 - l2_dt), 0, L)
    e_state_l3 <- advanceState(l3_dt,
                               pkSet$e_coef_bolus_l3 * bolusLine,
                               pkSet$e_coef_infusion_l3 * rate * (1 - l3_dt), 0, L)
    e_state_ke0 <- advanceState(ke0_dt,
                                pkSet$e_coef_bolus_ke0 * bolusLine,
                                pkSet$e_coef_infusion_ke0 * rate * (1 - ke0_dt), 0, L)
    recovery <- sapply(1:L, function(i)
      recoveryCalc(
        c(e_state_l1[i], e_state_l2[i], e_state_l3[i], e_state_ke0[i]),
        c(pkSet$lambda_1, pkSet$lambda_2, pkSet$lambda_3, pkSet$ke0),
        emerge))
  } else {
    recovery <- rep(0, L)
  }

  data.frame(
    Time         = timeLine,
    Cp           = Cp,
    Ce           = Ce,
    CpMetabolite = Cm,
    CeMetabolite = Cem,
    Recovery     = recovery
  )
}
