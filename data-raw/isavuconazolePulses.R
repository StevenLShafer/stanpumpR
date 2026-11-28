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

# The plasma curve is LINEAR in the fractions once the delays are fixed, so
# only the delays are searched (Nelder-Mead, on log increments); for each set
# of delays the minimax fractions are solved by Lawson's iteratively
# reweighted least squares, with the fractions summing to 1 and the peak
# carried as one more row weighted by the ratio of the two limits.
lawson <- function(delay, iters = 200) {
  grid <- sort(unique(c(seq(0, 12, by = 0.01), seq(12, TMAX, by = 0.1),
                        unlist(lapply(delay, function(d) d + c(0, 0.0025, 0.005, 0.01, 0.02))))))
  grid <- grid[grid <= TMAX]
  M <- sapply(delay, function(d) oralUnit(grid - d)) / peak / 0.0053
  y <- exactAt(grid) / peak / 0.0053
  M <- rbind(M, sapply(delay, function(d) oralUnit(tPeak - d)) / peak / 0.0001)
  y <- c(y, 1 / 0.0001)
  n <- length(delay)
  # f_n = 1 - sum(f_1..f_{n-1})
  A <- M[, -n, drop = FALSE] - M[, n]; b <- y - M[, n]
  w <- rep(1, nrow(A))
  for (k in seq_len(iters)) {
    g <- stats::lm.wfit(A, b, w)$coefficients
    g[is.na(g)] <- 0
    r <- abs(b - A %*% g)
    w <- w * as.vector(r); w <- w / sum(w)
    if (any(!is.finite(w))) break
  }
  f <- c(g, 1 - sum(g))
  list(fraction = f, delay = delay)
}

score <- function(q, n) {
  delay <- cumsum(exp(q))
  if (any(delay > 10)) return(1e6)
  x <- lawson(delay, iters = 60)
  if (any(x$fraction < 0)) return(1e3 + sum(pmax(-x$fraction, 0)))
  e <- errors(x)
  max(e[["curve"]] / 0.0053, e[["peak"]] / 0.0001)
}

fitPulses <- function(n, restarts = as.integer(Sys.getenv("RESTARTS", "6"))) {
  set.seed(n)
  # start: delays spread over the Weibull's upstroke, 0.5 to 1.9 h
  start <- log(diff(c(0, seq(0.55, 1.9, length.out = n))))
  best <- list(value = Inf, par = start)
  for (it in seq_len(restarts)) {
    p0 <- if (it == 1) start else best$par + stats::rnorm(n, sd = 0.25)
    o <- stats::optim(p0, score, n = n, method = "Nelder-Mead",
                      control = list(maxit = 3000, reltol = 1e-10))
    if (o$value < best$value) best <- o
    cat(sprintf("  restart %d: %.4f (best %.4f)\n", it, o$value, best$value))
  }
  # The final, longer solve is what is shipped, so its score is the one
  # reported, and it must keep the fractions nonnegative as the search did
  # (Devin's review of PR #1018, 2026-11-28).
  x <- lawson(cumsum(exp(best$par)))
  if (any(x$fraction < 0)) stop("the final solve gave a negative fraction")
  e <- errors(x)
  list(n = n, fraction = x$fraction, delay = x$delay, errors = e,
       score = max(e[["curve"]] / 0.0053, e[["peak"]] / 0.0001))
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
