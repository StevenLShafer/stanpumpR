# Fit the oral pulses that stand for Desai 2016's Weibull absorption of
# isavuconazole (R/drugs_isavuconazole.R).  Run from the package root:
#   Rscript --vanilla data-raw/isavuconazolePulses.R [n]
# Drafted by Claude Code, 2026-11-28, on Steven L. Shafer's decision of that
# day to approximate the Weibull with pulses of the default oral form, as
# hydrochlorothiazide's is (R/drugs_hydrochlorothiazide.R).
#
# Desai 2016 (Table 5, text p. 3): dA1/dt = -WB A1, WB = KAMAX [1 -
# exp(-(RA t)^GAM)], t the time after the dose, KAMAX 1.08 /h, RA 0.72 /h, GAM
# 4.88, F 1.  The depot's rate constant tends to KAMAX, so the pulses are
# absorbed at exactly 1.08 /h, the Weibull's own asymptotic rate; only their
# fractions and delays are fitted.  The fit is MINIMAX against the plasma curve
# of the reference patient (V1 49.1 L, Q 26.6 L/h, Vp 390 L, CL 2.36 L/h),
# normalised by its peak, with hydrochlorothiazide's two limits each scaled to
# 1: 0.53% over the curve and 0.01% at the peak.  The grid is rebuilt for each
# candidate around its own pulse times, where the approximation kinks.

KAMAX <- 1.08; RA <- 0.72; GAM <- 4.88
V1 <- 49.1; Q <- 26.6; VP <- 390; CL <- 2.36
TMAX <- 48

# The exact system, by classical Runge-Kutta at h = 1/4000 h
exactCurve <- function(h = 1 / 4000) {
  wb <- function(t) KAMAX * (1 - exp(-(RA * t)^GAM))
  f <- function(t, s) {
    r <- wb(t) * s[1]
    c(-r, r - CL / V1 * s[2] - Q / V1 * s[2] + Q / VP * s[3],
      Q / V1 * s[2] - Q / VP * s[3])
  }
  t <- seq(0, TMAX, by = h); s <- c(1, 0, 0); out <- numeric(length(t))
  for (i in seq_along(t)) {
    out[i] <- s[2] / V1
    k1 <- f(t[i], s); k2 <- f(t[i] + h / 2, s + h / 2 * k1)
    k3 <- f(t[i] + h / 2, s + h / 2 * k2); k4 <- f(t[i] + h, s + h * k3)
    s <- s + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  list(t = t, C = out)
}

# One unit dose absorbed at KAMAX after a lag, two compartments, closed form
oralUnit <- function(t) {
  k10 <- CL / V1; k12 <- Q / V1; k21 <- Q / VP
  a <- k10 + k12 + k21
  l1 <- (a + sqrt(a^2 - 4 * k10 * k21)) / 2; l2 <- (a - sqrt(a^2 - 4 * k10 * k21)) / 2
  ka <- KAMAX
  A <- ka / V1 * (k21 - l1) / ((ka - l1) * (l2 - l1))
  B <- ka / V1 * (k21 - l2) / ((ka - l2) * (l1 - l2))
  C <- -(A + B)
  ifelse(t > 0, A * exp(-l1 * t) + B * exp(-l2 * t) + C * exp(-ka * t), 0)
}

pulseCurve <- function(t, fraction, delay)
  Reduce(`+`, Map(function(f, d) f * oralUnit(t - d), fraction, delay))

unpack <- function(p, n) {
  fraction <- exp(p[1:n]) / sum(exp(p[1:n]))
  delay <- cumsum(exp(p[(n + 1):(2 * n)]))
  list(fraction = fraction, delay = delay)
}

ex <- exactCurve()
peak <- max(ex$C); tPeak <- ex$t[which.max(ex$C)]
exactAt <- function(t) approx(ex$t, ex$C, t)$y

errors <- function(x) {
  grid <- sort(unique(c(seq(0, TMAX, by = 0.02),
                        unlist(lapply(x$delay, function(d) d + c(-1e-6, 0, 1e-6, 0.005, 0.01, 0.02))),
                        tPeak)))
  grid <- grid[grid >= 0 & grid <= TMAX]
  e <- pulseCurve(grid, x$fraction, x$delay) - exactAt(grid)
  pk <- max(pulseCurve(grid, x$fraction, x$delay))
  c(curve = max(abs(e)) / peak, peak = abs(pk / peak - 1))
}

objective <- function(p, n) {
  e <- errors(unpack(p, n))
  max(e[["curve"]] / 0.0053, e[["peak"]] / 0.0001)
}

# A smooth stand-in for the worst point (a p-norm), minimised first by BFGS,
# then the true minimax objective polished by Nelder-Mead; repeated from the
# quantiles of the Weibull input and from random perturbations of the best.
smooth <- function(p, n, pw = 40) {
  x <- unpack(p, n)
  grid <- sort(unique(c(seq(0, 12, by = 0.02), seq(12, TMAX, by = 0.25),
                        unlist(lapply(x$delay, function(d) d + c(0, 0.005, 0.01, 0.02))), tPeak)))
  grid <- grid[grid <= TMAX]
  pc <- pulseCurve(grid, x$fraction, x$delay)
  e <- c((pc - exactAt(grid)) / peak / 0.0053, (max(pc) / peak - 1) / 0.0001)
  if (!all(is.finite(e))) return(1e6)
  m <- max(abs(e)); m * mean((abs(e) / m)^pw)^(1 / pw)
}

fitPulses <- function(n, restarts = as.integer(Sys.getenv("RESTARTS", "12"))) {
  set.seed(n)
  wbInt <- function(t) KAMAX * (t - stats::integrate(function(u) exp(-(RA * u)^GAM), 0, t)$value)
  q <- sapply(seq(0.5, n - 0.5) / n, function(pq)
    stats::uniroot(function(t) 1 - exp(-wbInt(t)) - pq, c(1e-6, 20))$root)
  start <- c(rep(0, n), log(pmax(diff(c(0, q)) - c(0.3, rep(0, n - 1)), 0.02)))
  best <- list(value = Inf, par = start)
  for (it in seq_len(restarts)) {
    p0 <- if (it == 1) start else best$par + stats::rnorm(2 * n, sd = 0.3)
    lo <- c(rep(-8, n), rep(log(1e-3), n)); hi <- c(rep(8, n), rep(log(3), n))
    p0 <- pmin(pmax(p0, lo), hi)
    o <- stats::optim(p0, objective, n = n, method = "Nelder-Mead",
                      control = list(maxit = 20000, reltol = 1e-14))
    o <- tryCatch(stats::optim(pmin(pmax(o$par, lo), hi), smooth, n = n, method = "L-BFGS-B",
                               lower = lo, upper = hi, control = list(maxit = 2000, factr = 10)),
                  error = function(e) o)
    o <- stats::optim(o$par, objective, n = n, method = "Nelder-Mead",
                      control = list(maxit = 40000, reltol = 1e-14))
    if (is.finite(o$value) && o$value < best$value) best <- o
    cat(sprintf("  restart %d: %.4f (best %.4f)\n", it, o$value, best$value))
  }
  x <- unpack(best$par, n)
  list(n = n, fraction = x$fraction, delay = x$delay, errors = errors(x), score = best$value)
}

args <- commandArgs(TRUE)
ns <- if (length(args)) as.integer(args) else 4:8
for (n in ns) {
  r <- fitPulses(n)
  cat(sprintf("n = %d  score %.3f  curve %.4f%%  peak %.5f%%\n", n, r$score,
              100 * r$errors[["curve"]], 100 * r$errors[["peak"]]))
  cat("  fraction:", sprintf("%.6f", r$fraction), "\n")
  cat("  delay (min):", sprintf("%.4f", 60 * r$delay), "\n")
}
