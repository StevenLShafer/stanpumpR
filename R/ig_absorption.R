#' NOTE FROM DEAN: This entire file is commented out, because it requires using
#' a package {RcppFaddeeva} that none of us have tried to install. When you're
#' ready to use the code in this file, uncomment the entire file, install the
#' {RcppFaddeeva} package, and test the code in this file to see that it actually
#' works. If it does, then {RcppFaddeeva} will need to be added to the DESCRIPTION
#' and to the {renv} lockfile.
#'
#'
#'
#'
#'
#'
#'
#'
#' # -----------------------------------------------------------------------------
#' # Provenance
#' # ----------
#' # Drafted by Claude Chat (claude.ai), 2026-08-07. Not authored by Claude Code.
#' # The underlying mathematics was verified numerically by the authoring model
#' # against adaptive quadrature (see STATUS below), but this R transcription has
#' # NOT itself been executed end-to-end. Run the verification block at the bottom
#' # before relying on any of it. Not yet exported or wired into the simulation
#' # engine as of this writing.
#' # -----------------------------------------------------------------------------
#' # Closed-form convolution of an Inverse Gaussian absorption density with a
#' # sum-of-exponentials disposition function.
#' #
#' #   C(t) = F * Dose * integral_0^t  f(s; mu, lambda) * sum_i A_i exp(-k_i (t-s)) ds
#' #
#' # where f is the Inverse Gaussian (Wald) density with mean mu and shape lambda.
#' #
#' # Derivation sketch
#' # -----------------
#' # Writing the IG density with the exponent expanded,
#' #
#' #   f(t) = sqrt(lambda / (2 pi t^3)) * exp(lambda/mu) *
#' #            exp(-lambda t / (2 mu^2) - lambda / (2 t))
#' #
#' # the 1/t term carries lambda only, not mu.  Multiplying by exp(alpha t)
#' # therefore shifts only the coefficient of t: an exponentially tilted Inverse
#' # Gaussian is another Inverse Gaussian with the same lambda.  The required
#' # integral has the standard closed form
#' #
#' #   integral_0^t s^(-3/2) exp(-c/s - b s) ds
#' #     = (1/2) sqrt(pi/c) [ exp(-2 sqrt(bc)) erfc(sqrt(c/t) - sqrt(bt))
#' #                        + exp( 2 sqrt(bc)) erfc(sqrt(c/t) + sqrt(bt)) ]
#' #
#' # with b = lambda/(2 mu^2) - k_i  and  c = lambda/2.
#' #
#' # Folding exp(-k_i t) into the same exponential cancels k_i from the prefactor,
#' # which is what keeps the computation in range for every term.
#' #
#' # Validity
#' # --------
#' # b > 0 requires k_i < lambda / (2 mu^2).  Since CV^2 = mu/lambda for the IG,
#' # that threshold is 1/(2 mu CV^2) -- roughly 2/h for a one-hour mean absorption
#' # time at CV 0.5.  Fast distribution exponents routinely exceed it, so the
#' # b < 0 branch is the common case, not an edge case.  There sqrt(b) is
#' # imaginary, the two erfcx arguments become complex conjugates, and their sum
#' # is real: erfcx(z) = w(i z), the Faddeeva function.
#' #
#' # STATUS: the mathematics was verified numerically against adaptive quadrature
#' # to 10-15 significant figures, including the complex branch, the b = 0
#' # crossover, and mass balance.  This R code is a transcription of that verified
#' # algorithm and has NOT itself been executed.  Run the checks at the bottom
#' # before using it for anything.
#' # -----------------------------------------------------------------------------
#'
#'
#' #' Scaled complementary error function for real, non-negative x
#' #'
#' #' erfcx(x) = exp(x^2) * erfc(x), computed in log space so that large x
#' #' neither overflows nor underflows.  Base R only -- no dependency.
#' erfcx_real <- function(x) {
#'   # erfc(x) = 2 * pnorm(-x * sqrt(2))
#'   exp(x^2 + log(2) + stats::pnorm(-x * sqrt(2), log.p = TRUE))
#' }
#'
#'
#' #' Plasma concentration with Inverse Gaussian absorption
#' #'
#' #' @param t       numeric vector of times (same units as mu and the exponents)
#' #' @param A       coefficients of the unit disposition function
#' #' @param k       exponents of the unit disposition function (same length as A)
#' #' @param mu      mean absorption time
#' #' @param lambda  IG shape parameter (CV^2 of absorption time = mu / lambda)
#' #' @param dose    administered dose
#' #' @param F       bioavailability
#' #'
#' #' @return numeric vector of concentrations, same length as t
#' ig_absorption_conc <- function(t, A, k, mu, lambda, dose = 1, F = 1) {
#'
#'   stopifnot(length(A) == length(k), mu > 0, lambda > 0, all(is.finite(t)))
#'
#'   out <- numeric(length(t))
#'   pos <- t > 0
#'   if (!any(pos)) return(out)
#'
#'   tt <- t[pos]
#'   c0 <- lambda / 2
#'
#'   # Common prefactor exponent, independent of k_i.  This cancellation is what
#'   # keeps every branch bounded.
#'   E <- lambda / mu - c0 / tt - lambda * tt / (2 * mu^2)
#'   p <- sqrt(c0 / tt)
#'
#'   total <- numeric(length(tt))
#'
#'   for (i in seq_along(A)) {
#'
#'     b <- lambda / (2 * mu^2) - k[i]
#'
#'     if (b > 0) {
#'       # ---- real branch --------------------------------------------------
#'       q <- sqrt(b * tt)
#'       v <- p - q
#'       u <- p + q
#'
#'       # erfcx(u) is always bounded: u > 0.
#'       term_u <- 0.5 * exp(E) * erfcx_real(u)
#'
#'       # For v < 0, erfcx(v) overflows.  Use the reflection
#'       #   erfcx(v) = 2 exp(v^2) - erfcx(-v)
#'       # and note E + v^2 = lambda/mu - 2 sqrt(bc) - k_i t, which decays.
#'       ex <- 0.5 * exp(E) * erfcx_real(abs(v))
#'       big <- exp(lambda / mu - 2 * sqrt(b * c0) - k[i] * tt)
#'       term_v <- ifelse(v >= 0, ex, big - ex)
#'
#'       total <- total + A[i] * (term_u + term_v)
#'
#'     } else if (b < 0) {
#'       # ---- complex branch -----------------------------------------------
#'       # v = p - i q, u = conj(v), so erfcx(v) + erfcx(u) = 2 Re[erfcx(v)].
#'       # erfcx(v) = w(i v), and i v = q + i p.
#'       if (!requireNamespace("RcppFaddeeva", quietly = TRUE)) {
#'         stop("Package 'RcppFaddeeva' is required for exponents above the ",
#'              "threshold lambda / (2 * mu^2) = ", lambda / (2 * mu^2))
#'       }
#'       q <- sqrt(-b * tt)
#'       z <- complex(real = q, imaginary = p)
#'       total <- total + A[i] * exp(E) * Re(RcppFaddeeva::Faddeeva_w(z))
#'
#'     } else {
#'       # ---- b exactly zero: limiting (Levy) case --------------------------
#'       # sqrt(b t) = 0, so v = u = p and the bracket is 2 * erfcx(p).
#'       total <- total + A[i] * exp(E) * erfcx_real(p)
#'     }
#'   }
#'
#'   out[pos] <- F * dose * total
#'   out
#' }
#'
#'
#' #' Independent reference implementation by adaptive quadrature
#' #'
#' #' Deliberately written a different way: this evaluates the convolution
#' #' integral directly, so agreement with ig_absorption_conc() is evidence about
#' #' the closed form rather than a golden master of it.
#' ig_absorption_conc_quad <- function(t, A, k, mu, lambda, dose = 1, F = 1) {
#'
#'   ig_pdf <- function(s) {
#'     sqrt(lambda / (2 * pi * s^3)) * exp(-lambda * (s - mu)^2 / (2 * mu^2 * s))
#'   }
#'
#'   # IG mode -- used to split the integration range so the adaptive rule does
#'   # not step over a sharp absorption peak.
#'   mode <- mu * (sqrt(1 + 9 * mu^2 / (4 * lambda^2)) - 3 * mu / (2 * lambda))
#'
#'   sapply(t, function(ti) {
#'     if (ti <= 0) return(0)
#'     integrand <- function(s) {
#'       ig_pdf(s) * sapply(s, function(sj) sum(A * exp(-k * (ti - sj))))
#'     }
#'     breaks <- sort(unique(c(1e-14,
#'                             mode * c(0.3, 1, 3),
#'                             mu,
#'                             ti)))
#'     breaks <- breaks[breaks > 0 & breaks <= ti]
#'     breaks <- unique(c(1e-14, breaks, ti))
#'     tot <- 0
#'     for (j in seq_len(length(breaks) - 1)) {
#'       tot <- tot + stats::integrate(integrand, breaks[j], breaks[j + 1],
#'                              rel.tol = 1e-12, subdivisions = 500L)$value
#'     }
#'     F * dose * tot
#'   })
#' }
#'
#'
#' # -----------------------------------------------------------------------------
#' # Verification -- run these before trusting anything above.
#' # -----------------------------------------------------------------------------
#' if (FALSE) {
#'
#'   # A disposition function whose fastest exponent exceeds the threshold,
#'   # so both branches are exercised.
#'   A  <- c(0.0483, 0.0135, 0.00147)
#'   k  <- c(12.5,   1.02,   0.0759)      # per hour
#'   mu <- 1.0                            # mean absorption time, hours
#'   lam <- 4.0                           # threshold = lam / (2 mu^2) = 2 / hour
#'
#'   tt <- c(0.05, 0.2, 0.5, 1, 2, 5, 12, 24)
#'   a <- ig_absorption_conc(tt, A, k, mu, lam)
#'   b <- ig_absorption_conc_quad(tt, A, k, mu, lam)
#'   print(data.frame(t = tt, closed = a, quad = b,
#'                    rel_err = abs(a - b) / abs(b)))
#'
#'   # Mass balance: with no elimination, everything absorbed eventually.
#'   stopifnot(abs(ig_absorption_conc(500, 1, 0, mu = 1, lambda = 4) - 1) < 1e-10)
#'
#'   # Branch crossover: sweep k_i through the threshold.
#'   thr <- lam / (2 * mu^2)
#'   for (eps in c(1e-3, 1e-6, 0, -1e-6, -1e-3)) {
#'     kk <- thr - eps
#'     a <- ig_absorption_conc(c(0.5, 2, 10, 48), 1, kk, mu, lam)
#'     b <- ig_absorption_conc_quad(c(0.5, 2, 10, 48), 1, kk, mu, lam)
#'     message(sprintf("b = %+9.1e   max rel err = %.2e\n",
#'                 eps, max(abs(a - b) / abs(b))))
#'   }
#' }
