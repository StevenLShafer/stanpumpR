# -----------------------------------------------------------------------------
# Hydrochlorothiazide: can an inverse Gaussian replace its published input?
# -----------------------------------------------------------------------------
# Reproduces the measurement recorded in the header of R/ig_absorption.R
# ("HYDROCHLOROTHIAZIDE HAS NOW BEEN MEASURED, and the answer is no"): an
# inverse Gaussian input fitted to Van Wart 2013's plasma curve misses the
# documented standard by several-fold, where the eight pulses that
# R/drugs_hydrochlorothiazide.R uses meet it with room to spare.  Written by
# Claude Code, 2026-11-01, at Steven L. Shafer's request, so that the verdict
# can be checked rather than re-derived.
#
# Base R only, and it reads the shipped model rather than retyping it: the
# Van Wart constants and the pulse set come from R/drugs_hydrochlorothiazide.R,
# the kernel from R/ig_absorption.R.  Run from the repository root:
#   Rscript data-raw/ig-hydrochlorothiazide.R
# It takes a few minutes, writes the comparison figure to tempdir(), and stops
# with an error if any recorded number no longer reproduces.
#
# Units: HOURS throughout, as the source writes its input (its Weibull scale is
# 1.34 /h); the pulse delays, stored in minutes, are converted on reading.
#
# WHAT IS COMPARED.  Van Wart's input is not a Weibull DENSITY: the Weibull CDF
# is the depot's time-varying rate constant,
#     dA/dt = -(1 - exp(-(ka t)^gamma)) A,      ka 1.34 /h, gamma 2.84,
# so its input density is k(t) A(t), and the gut amount has a closed form
# through the incomplete gamma function.  Its plasma curve, convolved with the
# two-compartment disposition by quadrature, is the reference; it is checked
# here against an independent Runge-Kutta solution of the three-state system.
# Both approximations are then measured against it the way the standard
# defines the error -- the worst point as a percentage of the peak, and the
# peak itself -- pooled over the four renal functions the paper tabulates
# (creatinine clearance 120, 80, 50 and 30 mL/min), on a grid that includes
# every pulse time, where the pulse curve kinks.
#
# THE RESULT, which the checks at the end hold the script to:
#   - the published input has CV 0.629 and skew 1.843; an IG with that CV is
#     forced to skew 3 CV = 1.886, a 2% mismatch, so the moments agree;
#   - fitted to the standard's own two-limit objective (each error divided by
#     its limit, the worse of the two minimised, as the pulses were fitted),
#     the best IG reaches 6.6x the standard, where the pulses use 64% of it;
#     nine starts spread over means of 0.6 to 4 h and shapes of 0.8 to 40 all
#     converge to that same point;
#   - a search is evidence for a global minimum, not a proof of one, but if
#     6.6x is the global minimum it bounds every trade between the limits: an
#     IG held within its peak limit at all four renal functions would miss the
#     curve by at least 3.49%; one fitted to the curve alone reaches 1.05%
#     with the peak 0.95% out (95x);
#   - the error is the UPSTROKE, up to +3.5% of the peak before 1.5 h; from
#     1.5 h to 48 h it stays within 0.36% of the peak, under the curve limit
#     (0.56% relative to the local value at its worst); and after 48 h, when
#     less than 1e-17 of either dose is left to absorb, the relative error is
#     the ratio of the two inputs' moment generating functions at the terminal
#     exponent, less one, 0.06% at most, which the curves themselves match
#     from 48 h to 336 h.
# Four of these were first recorded wrongly: "within 0.5% relative" (read
# from seven time points), "0.92%, 92x" (one renal function), "forced to the
# exact peak, the curve is 2.10%, 4.0x" (one renal function, and an optimiser
# left to wander along the curve of IGs that all match the peak, so not a
# minimum of anything), and 6.6x as a proven bound on every trade (from four
# starts clustered in one valley and a grid too coarse to resolve it); the
# last two and the unchecked tail after 48 h were CodeRabbit's, on #668.  The
# dense grid, the four renal functions and the spread starts here are what
# corrected them, which is why this script checks itself.
# The reason is the shape, not the moments: Van Wart's rate constant is still
# ramping up from zero while an IG of the same mean is already absorbing.
# -----------------------------------------------------------------------------

source("R/ig_absorption.R")
source("R/drugs_hydrochlorothiazide.R")

LIM_CURVE <- 0.53      # % of peak, worst point on the curve (MDMA's standard)
LIM_PEAK  <- 0.01      # % of peak, at the peak
CRCLS     <- c(120, 80, 50, 30)   # mL/min, the renal functions Van Wart tabulates
KA        <- HCTZ_WEIBULL_SCALE
GAMMA     <- HCTZ_WEIBULL_SHAPE


# ---- The exact published input ----------------------------------------------

# Fraction of the dose still in the gut: A(t) = exp(-t + integral of
# exp(-(ka s)^gamma)), the integral being Gamma(1 + 1/g)/ka * P(1/g, (ka t)^g).
gutAmount <- function(t) {
  ifelse(t <= 0, 1,
         exp(-t + gamma(1 + 1 / GAMMA) / KA * pgamma((KA * t)^GAMMA, 1 / GAMMA)))
}
depotRate <- function(t) 1 - exp(-(KA * t)^GAMMA)                     # 1/h
inputRate <- function(t) ifelse(t <= 0, 0, depotRate(t) * gutAmount(t))  # 1/h

# Two-compartment plasma coefficients per unit dose, from the shipped constants.
disposition <- function(crcl) {
  cl  <- HCTZ_CL_R * (crcl / HCTZ_CRCL_REF)^HCTZ_CL_R_EXP + HCTZ_CL_NR
  k10 <- cl / HCTZ_V1; k12 <- HCTZ_Q / HCTZ_V1; k21 <- HCTZ_Q / HCTZ_V2
  b   <- k10 + k12 + k21
  l   <- c((b + sqrt(b^2 - 4 * k10 * k21)) / 2, (b - sqrt(b^2 - 4 * k10 * k21)) / 2)
  list(A = c(l[1] - k21, k21 - l[2]) / (HCTZ_V1 * (l[1] - l[2])), k = l,
       k10 = k10, k12 = k12, k21 = k21)
}

# The reference curve: the closed-form input convolved with the disposition.
cpExact <- function(t, d) {
  vapply(t, function(ti) {
    if (ti <= 0) return(0)
    f  <- function(s) inputRate(s) *
      vapply(s, function(sj) sum(d$A * exp(-d$k * (ti - sj))), numeric(1))
    br <- unique(c(1e-12, c(0.2, 0.5, 1, 1.5, 2, 3)[c(0.2, 0.5, 1, 1.5, 2, 3) < ti], ti))
    sum(vapply(seq_len(length(br) - 1), function(j)
      integrate(f, br[j], br[j + 1], rel.tol = 1e-12, subdivisions = 1000L)$value,
      numeric(1)))
  }, numeric(1))
}

# An independent check of that reference: RK4 on the three states, nothing
# closed form in it.
cpRK4 <- function(tout, d, h = 1 / 4000) {
  deriv <- function(t, y) {
    r <- depotRate(t) * y[1]
    c(-r, r - (d$k10 + d$k12) * y[2] + d$k21 * y[3], d$k12 * y[2] - d$k21 * y[3])
  }
  n <- ceiling(max(tout) / h); h <- max(tout) / n
  tg <- seq(0, max(tout), length.out = n + 1)
  y <- c(1, 0, 0); cp <- numeric(n + 1)
  for (i in seq_len(n)) {
    t  <- tg[i]
    k1 <- deriv(t, y);           k2 <- deriv(t + h / 2, y + h / 2 * k1)
    k3 <- deriv(t + h / 2, y + h / 2 * k2); k4 <- deriv(t + h, y + h * k3)
    y  <- y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    cp[i + 1] <- y[2] / HCTZ_V1
  }
  approx(tg, cp, xout = tout)$y
}


# ---- The two approximations ---------------------------------------------------

DELAY <- HCTZ_PULSE_DELAY / 60     # minutes -> hours
cpPulses <- function(t, d) {
  out <- numeric(length(t))
  for (j in seq_along(HCTZ_PULSE_FRACTION)) {
    u <- t - DELAY[j]; pos <- u > 0
    if (!any(pos)) next
    out[pos] <- out[pos] + HCTZ_PULSE_FRACTION[j] * HCTZ_KA *
      rowSums(outer(u[pos], seq_along(d$A), function(uu, i)
        d$A[i] * (exp(-d$k[i] * uu) - exp(-HCTZ_KA * uu)) / (HCTZ_KA - d$k[i])))
  }
  out
}
cpIG <- function(t, d, mu, lambda) igAbsorptionConc(t, d$A, d$k, mu, lambda)


# ---- Measurement --------------------------------------------------------------

# Dense early, plus a neighbourhood of every pulse time: a uniform grid steps
# over the kinks and flatters the pulses (R/drugs_hydrochlorothiazide.R).
GRID <- sort(unique(c(seq(0.005, 8, by = 0.005), seq(8.05, 48, by = 0.05),
                      as.vector(outer(DELAY, c(-2e-4, -1e-5, 0, 1e-5, 2e-4), "+")))))
GRID <- GRID[GRID > 0]

cat("Building the reference curves...\n")
REF <- lapply(CRCLS, function(crcl) {
  d  <- disposition(crcl)
  pk <- optimize(function(x) cpExact(x, d), c(0.3, 8), maximum = TRUE)
  list(d = d, ex = cpExact(GRID, d), peak = pk$objective, tpeak = pk$maximum)
})

# Worst point as % of peak, and the peak's own error as % of peak.
score <- function(fn, r) {
  pk <- optimize(function(x) fn(x, r$d), c(0.2, 10), maximum = TRUE)$objective
  c(curve = max(abs(fn(GRID, r$d) - r$ex)) / r$peak * 100,
    peak  = abs(pk - r$peak) / r$peak * 100)
}
# The standard's objective: each error over its own limit, the worse of the two,
# the worst renal function.  1 just meets the standard.
standardScore <- function(fn) {
  max(vapply(REF, function(r) {
    s <- score(fn, r); max(s[["curve"]] / LIM_CURVE, s[["peak"]] / LIM_PEAK)
  }, numeric(1)))
}
# Nelder-Mead from each start, on log parameters.  Returns the best, and where
# every start ended, which is the evidence about whether there is one minimum.
fitIG <- function(objective, starts) {
  runs <- lapply(starts, function(s)
    optim(s, objective, method = "Nelder-Mead",
          control = list(maxit = 3000, reltol = 1e-13)))
  values <- vapply(runs, `[[`, numeric(1), "value")
  best <- runs[[which.min(values)]]
  list(mu = exp(best$par[1]), lambda = exp(best$par[2]), value = best$value,
       ends = t(vapply(runs, function(o) exp(o$par), numeric(2))), values = values)
}


# ---- 1. The reference is right ------------------------------------------------

tt  <- c(0.25, 0.5, 1, 2, 3, 6, 12, 24, 48)
rk4 <- max(vapply(c(120, 30), function(crcl) {
  d <- disposition(crcl); a <- cpExact(tt, d); b <- cpRK4(tt, d)
  max(abs(a - b) / b)
}, numeric(1)))
cat(sprintf("Reference vs independent RK4: worst relative difference %.2e\n", rk4))


# ---- 2. The moments ------------------------------------------------------------

mom <- function(p) integrate(function(s) s^p * inputRate(s), 1e-12, 60,
                             rel.tol = 1e-11, subdivisions = 3000L)$value
m1 <- mom(1); sd <- sqrt(mom(2) - m1^2)
cv <- sd / m1
skew <- (mom(3) - 3 * m1 * sd^2 - m1^3) / sd^3
cat(sprintf("Published input: mean %.4f h, CV %.4f, skew %.4f; an IG at that CV must skew %.4f\n",
            m1, cv, skew, 3 * cv))


# ---- 3. The pulses meet the standard ------------------------------------------

pulses <- t(vapply(REF, function(r) score(cpPulses, r), numeric(2)))
cat(sprintf("Eight pulses: worst %.3f%% on the curve, %.4f%% at the peak (%.0f%% of the standard)\n",
            max(pulses[, "curve"]), max(pulses[, "peak"]),
            100 * standardScore(cpPulses)))


# ---- 4. The IG, fitted to the standard's own objective ------------------------

# Starts SPREAD over the whole plausible region, not clustered near the
# moments: four starts that all begin in one valley and all end there say
# nothing about whether another valley is lower.  (The first version did
# exactly that.  CodeRabbit, #668.)
cat("Fitting the inverse Gaussian from nine spread starts (the slow part)...\n")
starts <- lapply(asplit(expand.grid(mu = c(0.6, 1.6, 4.0), lambda = c(0.8, 4, 40)), 1),
                 function(s) log(c(s[["mu"]], s[["lambda"]])))
ig <- fitIG(function(p) {
  if (!all(is.finite(exp(p)))) return(1e9)
  standardScore(function(t, d) cpIG(t, d, exp(p[1]), exp(p[2])))
}, starts)
igScores <- t(vapply(REF, function(r)
  score(function(t, d) cpIG(t, d, ig$mu, ig$lambda), r), numeric(2)))
cat(sprintf("Best IG (mu %.4f h, lambda %.4f): worst %.3f%% on the curve, %.4f%% at the peak -> %.1fx the standard\n",
            ig$mu, ig$lambda, max(igScores[, "curve"]), max(igScores[, "peak"]),
            ig$value))


# ---- 5. The other trades -------------------------------------------------------

r120 <- REF[[1]]

# 6.6x as a bound, and how far it is one.  The fit minimised the larger of the
# two scaled errors, so IF ig$value is the global minimum, then at every
# (mu, lambda) one of them is at least that, and an IG held within its peak
# limit at every renal function would carry that much on the curve.  A search
# cannot prove a global minimum; what it can show is that starts spread across
# the region all reach the same point, which is evidence of a single valley.
# (Two earlier versions overclaimed here: one zeroed the peak at one renal
# function, an equation in two unknowns whose solutions form a curve, so the
# optimiser stopped wherever it had wandered; the next called 6.6x a bound on
# every trade on the strength of clustered starts and a grid too coarse to
# resolve the valley.  Both CodeRabbit, #668.)
converged <- sum(abs(ig$values - ig$value) <= 0.01 * ig$value)
cat(sprintf("%d of %d spread starts converge to that same optimum (worst end %.4f)\n",
            converged, length(ig$values), max(ig$values)))
cat(sprintf("If it is the global minimum, an IG held within its peak limit misses the curve by at least %.2f%%\n",
            ig$value * LIM_CURVE))

cc <- fitIG(function(p) {
  if (!all(is.finite(exp(p)))) return(1e9)
  max(vapply(REF, function(r) score(function(t, d) cpIG(t, d, exp(p[1]), exp(p[2])), r)[["curve"]],
             numeric(1)))
}, list(c(log(ig$mu), log(ig$lambda)), c(log(m1), log(m1 / cv^2))))
ccPeak <- max(vapply(REF, function(r)
  score(function(t, d) cpIG(t, d, cc$mu, cc$lambda), r)[["peak"]], numeric(1)))
cat(sprintf("IG fitted to the curve alone: curve %.3f%%, but the peak is out by %.2f%% (%.0fx its limit)\n",
            cc$value, ccPeak, ccPeak / LIM_PEAK))


# ---- 6. Where the error lives ---------------------------------------------------

# Over all four renal functions, as every other number here is.  Before
# 1.5 h, the largest signed error as a share of the peak; from 1.5 h to the
# grid's end at 48 h, the worst error as a share of the peak (what the
# standard limits) and relative to the local concentration (what a reader of
# the curve sees).
early <- GRID < 1.5; late <- GRID >= 1.5
upstroke <- max(vapply(REF, function(r)
  max((cpIG(GRID, r$d, ig$mu, ig$lambda)[early] - r$ex[early]) / r$peak * 100),
  numeric(1)))
lateErr <- t(vapply(REF, function(r) {
  e <- cpIG(GRID, r$d, ig$mu, ig$lambda)[late] - r$ex[late]
  c(peak = max(abs(e)) / r$peak * 100, rel = max(abs(e) / r$ex[late]) * 100)
}, numeric(2)))
latePeak <- max(lateErr[, "peak"]); lateRel <- max(lateErr[, "rel"])
cat(sprintf("Before 1.5 h the IG runs up to %+.2f%% of the peak high\n", upstroke))
cat(sprintf("From 1.5 h to 48 h its worst error is %.2f%% of the peak (limit %.2f%%), %.2f%% relative to the local value\n",
            latePeak, LIM_CURVE, lateRel))

# After 48 h.  By then less than 1e-17 of either dose is left to absorb (the
# IG's tail is the longer, 8.5e-18; the published depot's is 2.8e-21), so
# each curve is the sum of its disposition exponentials scaled by its input's
# moment generating function at each exponent; once the fast one has decayed,
# the relative error is the ratio of the two MGFs at the terminal exponent,
# less one.  The IG's MGF is closed form.  That limit says where the error
# ends up, not that it stays there between 48 h and infinity, so the curves
# themselves are also compared after 48 h, out to 336 h, which covers the
# elimination at the 42-h half-life of severe renal impairment.  (The limit
# alone was claimed for the whole tail at first.  CodeRabbit, #668.)
# The IG's remainder by quadrature of its own tail, with no absolute tolerance:
# the textbook 1 - F(t) returns exactly 0 here, F having rounded to 1, which a
# first draft of this check took for the answer.
leftAt48 <- max(gutAmount(48),
                integrate(function(s) igDensity(s, ig$mu, ig$lambda), 48, Inf,
                          rel.tol = 1e-10, abs.tol = 0)$value)
mgfIG <- function(k) exp(ig$lambda / ig$mu * (1 - sqrt(1 - 2 * ig$mu^2 * k / ig$lambda)))
mgfEx <- function(k) integrate(function(s) inputRate(s) * exp(k * s), 1e-12, 200,
                              rel.tol = 1e-12, subdivisions = 4000L)$value
TAIL <- c(48, 60, 72, 96, 120, 168, 240, 336)
tail48 <- t(vapply(REF, function(r) {
  lim <- (mgfIG(min(r$d$k)) / mgfEx(min(r$d$k)) - 1) * 100
  ex  <- cpExact(TAIL, r$d)
  rel <- (cpIG(TAIL, r$d, ig$mu, ig$lambda) - ex) / ex * 100
  c(limit = lim, offLimit = max(abs(rel - lim)), worst = max(abs(rel)))
}, numeric(3)))
tailRel <- tail48[, "limit"]
cat(sprintf("At 48 h at most %.1e of either dose is left to absorb\n", leftAt48))
cat(sprintf("After 48 h the relative error is %s%% by renal function, the curves matching that limit to %.1e points out to 336 h\n",
            paste(sprintf("%+.3f", tailRel), collapse = ", "), max(tail48[, "offLimit"])))
igC <- cpIG(GRID, r120$d, ig$mu, ig$lambda)     # for the figure


# ---- 7. The figure -------------------------------------------------------------

png(file.path(tempdir(), "hctz-weibull-vs-ig.png"), width = 1500, height = 1250, res = 150)
par(mfrow = c(3, 1), mar = c(4.2, 4.6, 2.6, 1.2))
ts <- seq(0.001, 6, by = 0.002)
igD <- igDensity(ts, ig$mu, ig$lambda)
pulseD <- vapply(ts, function(t) sum(HCTZ_PULSE_FRACTION * ifelse(t > DELAY,
                 HCTZ_KA * exp(-HCTZ_KA * (t - DELAY)), 0)), numeric(1))
plot(ts, inputRate(ts), type = "l", lwd = 3, ylim = c(0, 0.9),
     xlab = "hours after the dose", ylab = "fraction absorbed per hour",
     main = "Input shape: the published Van Wart depot vs an inverse Gaussian density")
lines(ts, igD, lwd = 2.5, col = "#C8102E", lty = 2)
lines(ts, pulseD, lwd = 1.6, col = "#0072B2", lty = 3)
legend("topright", bty = "n", lwd = c(3, 2.5, 1.6), lty = 1:3,
       col = c("black", "#C8102E", "#0072B2"),
       legend = c("published depot", sprintf("inverse Gaussian (mu %.2f h, lambda %.2f)",
                                             ig$mu, ig$lambda), "the eight pulses"))
sel <- GRID <= 24
plot(GRID[sel], r120$ex[sel], type = "l", lwd = 4,
     xlab = "hours after the dose", ylab = "plasma hydrochlorothiazide (per mg)",
     main = "Plasma curve, CLcr 120 mL/min")
lines(GRID[sel], igC[sel], lwd = 2.2, col = "#C8102E", lty = 2)
lines(GRID[sel], cpPulses(GRID, r120$d)[sel], lwd = 1.6, col = "#0072B2", lty = 3)
plot(GRID[sel], ((igC - r120$ex) / r120$peak * 100)[sel], type = "l", lwd = 2.5,
     col = "#C8102E", ylim = c(-1.2, 4),
     xlab = "hours after the dose", ylab = "error, % of peak",
     main = "Error against the exact published input")
lines(GRID[sel], ((cpPulses(GRID, r120$d) - r120$ex) / r120$peak * 100)[sel],
      lwd = 2.5, col = "#0072B2")
abline(h = 0, col = "grey60"); abline(h = c(-LIM_CURVE, LIM_CURVE), lty = 2, col = "grey35")
invisible(dev.off())
cat(sprintf("Figure: %s\n", file.path(tempdir(), "hctz-weibull-vs-ig.png")))


# ---- 8. Hold the script to the recorded numbers --------------------------------

near <- function(x, target, rel) abs(x - target) <= rel * abs(target)
# Every number the note in R/ig_absorption.R and the header above report.
checks <- c(
  "reference agrees with RK4 to 1e-9"           = rk4 < 1e-9,
  "input CV 0.629"                               = near(cv, 0.629, 0.002),
  "input skew 1.843"                             = near(skew, 1.843, 0.002),
  "an IG at that CV must skew 1.886"             = near(3 * cv, 1.886, 0.002),
  "pulses at 64% of the standard"                = near(standardScore(cpPulses), 0.64, 0.03),
  "pulses 0.341% on the curve"                   = near(max(pulses[, "curve"]), 0.341, 0.02),
  "best IG about 6.6x the standard"              = near(ig$value, 6.6, 0.05),
  "every spread start reaches that optimum"      = converged == length(ig$values),
  "if global, peak-held curve >= 3.49%"          = near(ig$value * LIM_CURVE, 3.49, 0.05),
  "IG fitted to the curve: curve about 1.05%"    = near(cc$value, 1.05, 0.05),
  "IG fitted to the curve: peak about 0.95%"     = near(ccPeak, 0.95, 0.05),
  "upstroke up to about +3.5% of the peak"       = near(upstroke, 3.49, 0.03),
  "1.5-48 h under the curve limit"               = latePeak < LIM_CURVE,
  "1.5-48 h about 0.36% of the peak"             = near(latePeak, 0.36, 0.05),
  "1.5-48 h about 0.56% relative"                = near(lateRel, 0.56, 0.05),
  "under 1e-17 of either dose left at 48 h"      = leftAt48 < 1e-17,
  "after 48 h, 0.06% relative at most"           = all(tail48[, "worst"] < 0.065),
  "after 48 h, the curves sit on the limit"      = all(tail48[, "offLimit"] < 1e-5)
)
if (!all(checks)) {
  stop("No longer reproduces the recorded result:\n  ",
       paste(names(checks)[!checks], collapse = "\n  "), call. = FALSE)
}
cat("All recorded numbers reproduce.\n")
