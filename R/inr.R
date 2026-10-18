# -----------------------------------------------------------------------------
# The INR from S- and R-warfarin concentrations: prothrombin complex activity
# (PCA) turnover, Xue 2017
# -----------------------------------------------------------------------------
# A drug that returns an `inr` block (warfarin, R/drugs_warfarin.R) is
# simulated as two parallel systems, S-warfarin (the drug's own) and
# R-warfarin (its one parallel system), each taking half of every racemic
# dose.  Their concentrations are not added.  They drive the synthesis of the
# prothrombin complex, a fraction of normal activity, PCA:
#
#     C50_app = C50 x (1 + C_R / RIC50)                (R competes with S)
#     I       = C_S^h / (C50_app^h + C_S^h)            (Emax fixed at 1)
#     dPCA/dt = kPCA x (PCA0 x (1 - I) - PCA),  kPCA = ln 2 / T2PCA
#     INR     = 1 / PCA
#
# starting from PCA0, the steady state before any warfarin.  This is the
# turnover model of Xue et al., Br J Clin Pharmacol 2017;83:823-835,
# Equations 2, 3 and 5 with the feedback of Equation 4 absent (it was
# rejected) and the theory-based INR of Equation 5 (A = 1, B = 0), which the
# authors found as good as the in-vitro calibration in vivo and recommend.
# The R-warfarin term is the competitive antagonism of Equation 2 as
# corrected by the erratum (Br J Clin Pharmacol 2017;83:1602).  The erratum
# itself could not be read here; the form above is the one that reproduces
# the paper's worked example, an apparent C50 of 0.514 against 0.378 mg/L
# (36% higher) at the C_R of about 0.86 mg/L a maintenance dose gives, which
# test-drugs-warfarin.R checks.
#
# This is not an effect site.  The INR is a nonlinear function of the two
# concentrations, delayed by the turnover of the clotting factors, so it
# cannot be carried by the closed-form engines' exponential states.  It is
# computed on the engines' time line once they have run: between two points
# the inhibition is taken as linear in time and the turnover equation, linear
# in PCA, is solved exactly over the step.  The engines' grid puts fine steps
# after every dose, where the concentrations change fastest; the error
# against a fine numerical solution is checked in test-drugs-warfarin.R.
#
# A second form, the Hamberg 2007 transit-compartment model, rides the same
# path as a companion of warfarin (R/alternativeModels.R).  It is declared by
# an inr block with `type = "transit"` and integrated by inrTransit() below;
# its INR is driven by S-warfarin only.  Both forms keep R-warfarin as the one
# parallel system, and inrBaseline() gives the drug-free INR each one starts
# from.
#
# What is plotted is the INR.  The concentrations are not, and there is no
# effect site, no MEAC and no time until threshold: the INR falls after
# warfarin is stopped at the pace of clotting-factor synthesis, which the
# exponential states the recovery search reads do not describe.
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' The type of an INR block: the turnover model (Xue) unless it says otherwise
#' @keywords internal
inrType <- function(inr) if (is.null(inr$type)) "turnover" else inr$type

#' Check a drug model's INR block
#'
#' Two forms are allowed.  The default, \code{type = "turnover"}, is the Xue
#' 2017 single-compartment prothrombin-complex turnover (R/drugs_warfarin.R).
#' The second, \code{type = "transit"}, is the Hamberg 2007 two-chain
#' transit-compartment model (R/alternativeModels.R); both drive the INR from
#' the S-warfarin concentration and both keep R-warfarin as the one parallel
#' system.
#'
#' @param inr the block a drug model returned, or NULL
#' @param drug the drug's name, for the error message
#' @param concentrationUnits the CSV's \code{Concentration.Units}, which must
#'   be "INR" exactly when there is a block
#' @param parallelSystems the model's parallel systems: an INR drug has
#'   exactly one, R-warfarin
#' @returns \code{inr}, unchanged, if it is valid; otherwise an error
#' @keywords internal
validateINR <- function(inr, drug, concentrationUnits, parallelSystems)
{
  isINR <- identical(as.character(concentrationUnits), "INR")
  if (is.null(inr)) {
    if (isINR)
      stop("Invalid INR model for ", drug, ": Concentration.Units \"INR\" ",
           "needs an inr block.")
    return(NULL)
  }
  if (!isINR)
    stop("Invalid INR model for ", drug, ": only a drug with ",
         "Concentration.Units \"INR\" may return an inr block.")
  type <- inrType(inr)
  if (!type %in% c("turnover", "transit"))
    stop("Invalid INR model for ", drug, ": unknown type \"", type, "\".")
  if (type == "turnover") {
    ok <- is.list(inr) &&
      is_valid_number(inr$pca0, 0, 1) && inr$pca0 > 0 &&
      is_valid_number(inr$c50) && inr$c50 > 0 &&
      is_valid_number(inr$hill) && inr$hill > 0 &&
      is_valid_number(inr$ric50) && inr$ric50 > 0 &&
      is_valid_number(inr$halfLife) && inr$halfLife > 0
    if (!ok)
      stop("Invalid INR model for ", drug, ": needs pca0 in (0, 1], and ",
           "positive c50, hill, ric50 (mg/L) and halfLife (min).")
  } else {
    ok <- is.list(inr) &&
      is_valid_number(inr$c50) && inr$c50 > 0 &&
      is_valid_number(inr$gamma) && inr$gamma > 0 &&
      is_valid_number(inr$mtt1) && inr$mtt1 > 0 &&
      is_valid_number(inr$mtt2) && inr$mtt2 > 0 &&
      is_valid_number(inr$n1) && inr$n1 >= 1 && inr$n1 == round(inr$n1) &&
      is_valid_number(inr$baseline) && inr$baseline > 0 &&
      is_valid_number(inr$inrMax) && inr$inrMax > 0 &&
      is_valid_number(inr$lambda) && inr$lambda > 0
    if (!ok)
      stop("Invalid transit INR model for ", drug, ": needs positive c50 ",
           "(mg/L), gamma, mtt1, mtt2 (min), a positive integer n1, and ",
           "positive baseline, inrMax and lambda.")
  }
  if (length(parallelSystems) != 1)
    stop("Invalid INR model for ", drug, ": the drug's own system is ",
         "S-warfarin and it needs exactly one parallel system, R-warfarin.")
  inr
}

#' The baseline INR before any warfarin, for normalising the rise
#'
#' The turnover model starts at \code{1 / pca0}; the transit model starts at
#' its fitted \code{baseline} (at steady state every transit amount is 1, so
#' \code{(1 - A6 A7) = 0} and the INR is \code{baseline}).
#' @keywords internal
inrBaseline <- function(inr)
{
  if (inrType(inr) == "transit") inr$baseline else 1 / inr$pca0
}

#' Inhibition of prothrombin complex synthesis
#'
#' @param cS,cR S- and R-warfarin concentrations, mg/L
#' @param inr the drug's INR block
#' @returns the fractional inhibition, 0 to 1
#' @keywords internal
inrInhibition <- function(cS, cR, inr)
{
  cS <- pmax(cS, 0)
  c50 <- inr$c50 * (1 + pmax(cR, 0) / inr$ric50)
  cS^inr$hill / (c50^inr$hill + cS^inr$hill)
}

#' Prothrombin complex activity over time
#'
#' Solves dPCA/dt = k (PCA0 (1 - I) - PCA) from PCA0 at the first time, with
#' I linear in time between neighbouring points, exactly over each step.
#'
#' @param time times, minutes, increasing
#' @param inhibition the inhibition at those times
#' @param inr the drug's INR block
#' @returns PCA at \code{time}
#' @keywords internal
inrPCA <- function(time, inhibition, inr)
{
  n <- length(time)
  k <- log(2) / inr$halfLife
  target <- inr$pca0 * (1 - inhibition)       # the PCA each I would settle to
  pca <- numeric(n)
  if (n == 0) return(pca)
  pca[1] <- inr$pca0
  if (n == 1) return(pca)
  dt <- diff(time)
  decay <- exp(-k * dt)
  # For a target g linear over the step, from g0 to g1 (slope m = (g1-g0)/dt),
  # P(dt) = g1 - m/k + (P(0) - g0 + m/k) exp(-k dt).  A zero-length step (two
  # points at one time) leaves P unchanged.
  lag <- ifelse(dt > 0, (target[-1] - target[-n]) / (k * pmax(dt, .Machine$double.xmin)), 0)
  for (i in seq_len(n - 1)) {
    pca[i + 1] <- if (dt[i] > 0)
      target[i + 1] - lag[i] + (pca[i] - target[i] + lag[i]) * decay[i]
    else pca[i]
  }
  pca
}

#' Combine the S- and R-warfarin engine runs into the INR
#'
#' @param parts the engine outputs, S-warfarin first, then R-warfarin, on the
#'   same time line
#' @param inr the drug's INR block
#' @returns one engine output: \code{Time}, \code{Cp} (the INR), \code{Ce}
#'   (NA: no effect site) and \code{Recovery} (NA)
#' @keywords internal
inrEngineResults <- function(parts, inr)
{
  s <- parts[[1]]
  r <- parts[[2]]
  if (!identical(s$Time, r$Time))
    stop("Cannot combine S- and R-warfarin runs on different time lines.")
  cp <- if (inrType(inr) == "transit") {
    # Hamberg 2007: S-warfarin only; R-warfarin is not a predictor of the INR.
    inrTransit(s$Time, s$Cp, inr)
  } else {
    1 / inrPCA(s$Time, inrInhibition(s$Cp, r$Cp, inr), inr)
  }
  data.frame(
    Time = s$Time,
    Cp = cp,
    Ce = rep(NA_real_, nrow(s)),
    Recovery = rep(NA_real_, nrow(s))
  )
}

#' The INR from S-warfarin by the Hamberg 2007 transit-compartment model
#'
#' Equations 8 and 10 of Hamberg et al., Clin Pharmacol Ther 2007;81:529-538.
#' S-warfarin inhibits the turnover of two parallel transit chains through one
#' inhibitory Emax relationship (Emax fixed at 1):
#'
#'     E     = C_S^gamma / (EC50^gamma + C_S^gamma)
#'     dA1/dt = ktr1 ((1 - E) - A1),  dAj/dt = ktr1 (A_{j-1} - Aj), j = 2..n1
#'     dA7/dt = ktr2 ((1 - E) - A7)
#'     INR    = baseline + inrMax (1 - A_{n1} A7)^lambda
#'
#' with ktr1 = 1/MTT1, ktr2 = 1/MTT2 (the paper's own convention; the rate is
#' the reciprocal of the mean transit time whatever the number of
#' compartments), every A starting at 1 (the drug-free steady state).  The
#' chains are integrated with RK4 on a fixed internal sub-step, with the
#' S-warfarin concentration taken linearly between the engine's own points;
#' the system's slowest time constant is many hours, so the sub-step is far
#' finer than it needs to be.
#'
#' @param time times, minutes, increasing
#' @param cS S-warfarin concentration at those times, mg/L
#' @param inr the drug's INR block (type "transit")
#' @param dtMax the longest internal integration step, minutes
#' @returns the INR at \code{time}
#' @keywords internal
inrTransit <- function(time, cS, inr, dtMax = 5)
{
  n <- length(time)
  nchain <- as.integer(inr$n1)
  ktr1 <- 1 / inr$mtt1
  ktr2 <- 1 / inr$mtt2
  gamma <- inr$gamma
  c50g <- inr$c50^gamma
  A6A7 <- numeric(n)
  if (n == 0) return(A6A7)
  # The amounts: A[1:nchain] are the long chain, A[nchain + 1] is A7.
  A <- rep(1, nchain + 1L)
  A6A7[1] <- A[nchain] * A[nchain + 1L]
  if (n == 1) return(inr$baseline + inr$inrMax * max(1 - A6A7[1], 0)^inr$lambda)
  inhibitionAt <- function(c) {
    c <- max(c, 0)
    if (c <= 0) 0 else c^gamma / (c50g + c^gamma)
  }
  deriv <- function(A, E) {
    dA <- numeric(nchain + 1L)
    dA[1] <- ktr1 * ((1 - E) - A[1])
    if (nchain >= 2L) for (j in 2:nchain) dA[j] <- ktr1 * (A[j - 1L] - A[j])
    dA[nchain + 1L] <- ktr2 * ((1 - E) - A[nchain + 1L])
    dA
  }
  for (i in seq_len(n - 1L)) {
    dt <- time[i + 1L] - time[i]
    if (dt <= 0) { A6A7[i + 1L] <- A[nchain] * A[nchain + 1L]; next }
    nsub <- max(1L, as.integer(ceiling(dt / dtMax)))
    h <- dt / nsub
    c0 <- cS[i]; c1 <- cS[i + 1L]
    for (s in seq_len(nsub)) {
      ta <- (s - 1L) / nsub; tb <- s / nsub; tm <- (ta + tb) / 2
      Ea <- inhibitionAt(c0 + (c1 - c0) * ta)
      Em <- inhibitionAt(c0 + (c1 - c0) * tm)
      Eb <- inhibitionAt(c0 + (c1 - c0) * tb)
      k1 <- deriv(A, Ea)
      k2 <- deriv(A + h / 2 * k1, Em)
      k3 <- deriv(A + h / 2 * k2, Em)
      k4 <- deriv(A + h * k3, Eb)
      A <- A + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    }
    A6A7[i + 1L] <- A[nchain] * A[nchain + 1L]
  }
  inr$baseline + inr$inrMax * pmax(1 - A6A7, 0)^inr$lambda
}
