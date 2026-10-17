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
# the paper's worked example, an apparent C50 of 0.530 against 0.391 mg/L
# (36% higher) at 5 mg a day, which test-drugs-warfarin.R checks.
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
# What is plotted is the INR.  The concentrations are not, and there is no
# effect site, no MEAC and no time until threshold: the INR falls after
# warfarin is stopped at the pace of clotting-factor synthesis, which the
# exponential states the recovery search reads do not describe.
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' Check a drug model's INR block
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
  ok <- is.list(inr) &&
    is_valid_number(inr$pca0, 0, 1) && inr$pca0 > 0 &&
    is_valid_number(inr$c50) && inr$c50 > 0 &&
    is_valid_number(inr$hill) && inr$hill > 0 &&
    is_valid_number(inr$ric50) && inr$ric50 > 0 &&
    is_valid_number(inr$halfLife) && inr$halfLife > 0
  if (!ok)
    stop("Invalid INR model for ", drug, ": needs pca0 in (0, 1], and ",
         "positive c50, hill, ric50 (mg/L) and halfLife (min).")
  if (length(parallelSystems) != 1)
    stop("Invalid INR model for ", drug, ": the drug's own system is ",
         "S-warfarin and it needs exactly one parallel system, R-warfarin.")
  inr
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
  inhibition <- inrInhibition(s$Cp, r$Cp, inr)
  pca <- inrPCA(s$Time, inhibition, inr)
  data.frame(
    Time = s$Time,
    Cp = 1 / pca,
    Ce = rep(NA_real_, nrow(s)),
    Recovery = rep(NA_real_, nrow(s))
  )
}
