# -----------------------------------------------------------------------------
# Factor VIII carried by von Willebrand factor: an indirect-response model
# driven by the VWF:RCo activity a VWF concentrate leaves in the patient
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-11-28, at the request of Steven L. Shafer
# (docs/library-expansion/plan.csv, Biologics (surgery)); the source is Bauer
# 2023 and its supplement, digest card papers/digests/PMID_37332615.md.
#
# Von Willebrand factor carries factor VIII in the circulation and protects it
# from clearance, so a patient with von Willebrand disease type 3 (no VWF) has
# almost no factor VIII, and a VWF concentrate that carries no factor VIII
# (Vonvendi) raises it nonetheless, over hours.  Bauer 2023 fitted that as an
# indirect response in which VWF:RCo inhibits the elimination of factor VIII:
#
#     dF/dt = kin - kout x (1 - Imax x V / (IC50 + V)) x F
#
# with V the total VWF:RCo activity (the patient's own plus the infused,
# IU/dL) and F the factor VIII activity (IU/dL).  kin = kout x F0, F0 being
# "the theoretical baseline FVIII:C assuming that no VWF is affecting kout"
# (Table 4, note d), so F0 is the activity at V = 0 and the steady state at a
# constant V is F0 / (1 - Imax V / (IC50 + V)).
#
# The model is not linear in the dose (V enters through a saturable function),
# so it is not an engine's metabolite: simCpCe() integrates it after the
# engines have given the VWF:RCo curve, and hands it on as the series of the
# metabolite-only drug factorVIIIvwf, which foldMetabolites() draws as its own
# panel.  Because it is not linear, two VWF products given to one patient
# would need ONE integration on their summed VWF, not two added together;
# Vonvendi is the only VWF product in the library, so that case cannot arise
# yet (the Humate-P model, which would raise it, is a later pull request).
#
# The integration: the VWF:RCo curve is taken as linear between the
# simulation's points, and each interval is cut into steps short enough that
# the elimination rate constant changes by less than 0.1% across one; within a
# step the rate constant is held at its value at the step's midpoint and the
# equation is solved exactly.  That is second order in the step, and
# test-drugs-vonicogAlfa.R checks it against an independent classical
# Runge-Kutta integration on a fine grid.  F starts at its steady state on
# the patient's own VWF:RCo, which the curve is at before the first dose.
# -----------------------------------------------------------------------------

#' Factor VIII activity driven by VWF:RCo (Bauer 2023)
#'
#' Integrates Bauer 2023's indirect-response model of factor VIII over a
#' VWF:RCo curve; see the header of \code{R/vwfFactorVIII.R}.
#'
#' @param time minutes, increasing
#' @param vwf total VWF:RCo activity at \code{time}, IU/dL
#' @param response the drug's \code{factorVIIIResponse} block: \code{kout}
#'   (per minute), \code{imax}, \code{ic50} (IU/dL), \code{baseline} (F0,
#'   IU/dL) and \code{vwfBaseline} (the patient's own VWF:RCo, IU/dL)
#' @returns factor VIII activity at \code{time}, IU/dL
#' @keywords internal
vwfFactorVIIISeries <- function(time, vwf, response)
{
  kout <- response$kout
  imax <- response$imax
  ic50 <- response$ic50
  kin  <- kout * response$baseline
  rate <- function(v) kout * (1 - imax * v / (ic50 + v))

  n <- length(time)
  out <- numeric(n)
  if (n == 0) return(out)
  # Before the first dose the patient is at steady state on their own VWF
  f <- kin / rate(response$vwfBaseline)
  out[1] <- f
  if (n == 1) return(out)
  for (i in 2:n)
  {
    dt <- time[i] - time[i - 1]
    if (dt > 0)
    {
      v0 <- vwf[i - 1]
      v1 <- vwf[i]
      k0 <- rate(v0)
      k1 <- rate(v1)
      steps <- max(1L, min(10000L, ceiling(abs(log(k1 / k0)) / 1e-3)))
      h <- dt / steps
      for (s in seq_len(steps))
      {
        v <- v0 + (v1 - v0) * (s - 0.5) / steps
        k <- rate(v)
        css <- kin / k
        f <- css + (f - css) * exp(-k * h)
      }
    }
    out[i] <- f
  }
  out
}

#' Check a drug's factor VIII response block
#'
#' @param x the block, or NULL
#' @param drug the drug's name, for messages
#' @returns the block, or NULL
#' @keywords internal
validateFactorVIIIResponse <- function(x, drug)
{
  if (is.null(x)) return(NULL)
  if (!is.list(x) || !is.character(x$name) || length(x$name) != 1 || !nzchar(x$name))
    stop(drug, ": factorVIIIResponse needs the name of the drug it is drawn as")
  for (field in c("kout", "imax", "ic50", "baseline", "vwfBaseline"))
  {
    value <- x[[field]]
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value < 0)
      stop(drug, ": factorVIIIResponse$", field, " must be one finite number, not negative")
  }
  if (x$imax >= 1) stop(drug, ": factorVIIIResponse$imax must be below 1")
  if (x$kout <= 0 || x$ic50 <= 0)
    stop(drug, ": factorVIIIResponse$kout and $ic50 must be positive")
  x
}
