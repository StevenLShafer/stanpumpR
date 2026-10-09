# -----------------------------------------------------------------------------
# Temazepam: the fit behind R/drugs_temazepam.R
# -----------------------------------------------------------------------------
# Reproduces the four per-kilogram disposition parameters of stanpumpR's
# temazepam model from the published intravenous means of van Steveninck 1994
# (part II, Table I) and Halliday 1987 (Figure 1).  The derivation is written
# up in docs/temazepam.md; tests/testthat/test-drugs-temazepam.R checks that
# the shipped constants are this fit's least-squares minimum.
#
# Base R only.  Run from the repository root:
#   Rscript data-raw/temazepam-fit.R
# It takes a few seconds and stops with an error if the fit no longer rounds to
# the constants in R/drugs_temazepam.R.
#
# Units: time in minutes, doses in mg, concentrations in ng/mL (= ug/L),
# parameters per kg: V1 and V2 in L/kg, CL and Q in L/h/kg.
# -----------------------------------------------------------------------------

# ---- The data -------------------------------------------------------------

# van Steveninck AL et al., Clin Pharmacol Ther 1994;55:546-555, Table I.
# Nine volunteers, each infused at 0.8 mg/kg/h for up to 30 min on two
# occasions six months apart.  Each value below is the mean of the two
# occasions' means (occasion 1, occasion 2):
#   weight 66, 67 kg (text); dose 26.1, 25.6 mg; infusion 29, 28 min;
#   Cmax 964, 1028 ng/mL; AUC 0-3 h 1.4, 1.4; AUC 0-8 h 2.9, 2.7;
#   AUC 0-inf 6.4, 5.9 ug.h/mL; half-life 10.7, 10.4 h.
vs <- list(
  weight = 66.5,          # kg
  dose   = 25.85,         # mg
  tinf   = 28.5,          # min
  target = c(cmax = 996, auc3 = 1400, auc8 = 2800, aucInf = 6150,
             thalf = 10.55)   # ng/mL; ng.h/mL x 3; h
)

# Halliday NJ et al., Br J Anaesth 1987;59:465-467, Figure 1.  Eleven
# volunteers (mean 68 kg) given 20 mg intravenously over 20 s, venous samples
# from the other arm.  The paper has no table; the means were read from the
# figure by eye, averaging the propylene glycol and salicylate solutions,
# whose curves did not differ.
hal <- list(
  weight = 68,
  dose   = 20,
  tinf   = 20 / 60,
  time   = c(5, 10, 15, 30, 60, 90, 120),
  conc   = c(1250, 1010, 910, 810, 625, 500, 420)
)

# The constants shipped in R/drugs_temazepam.R (V1, CL, Q, V2 per kg)
shipped <- c(V1 = 0.2784, CL = 0.0661, Q = 0.1115, V2 = 0.5231)


# ---- The model --------------------------------------------------------------

# Rate constants (1/min) from per-kg parameters; weight cancels.
rates <- function(pk) {
  c(k10 = pk[["CL"]] / pk[["V1"]] / 60,
    k12 = pk[["Q"]]  / pk[["V1"]] / 60,
    k21 = pk[["Q"]]  / pk[["V2"]] / 60)
}

# Half-lives (h) of the two exponentials, distribution first.
halfLives <- function(pk) {
  k <- rates(pk)
  a <- sum(k)
  root <- sqrt(a^2 - 4 * k[["k10"]] * k[["k21"]])
  log(2) / (c(a + root, a - root) / 2) / 60
}

# Plasma concentration (ng/mL) at times t (min) after a constant-rate
# infusion of `dose` mg over `tinf` min into a patient of `weight` kg.  Exact
# two-compartment solution from the eigen-decomposition of the amount matrix.
infusionCp <- function(pk, weight, dose, tinf, t) {
  k <- rates(pk)
  M <- rbind(c(-(k[["k10"]] + k[["k12"]]), k[["k21"]]),
             c(k[["k12"]], -k[["k21"]]))
  e <- eigen(M)
  lambda <- e$values                       # both negative
  V <- e$vectors
  Vi <- solve(V)
  rate <- c(dose / tinf, 0)                # mg/min into the central compartment
  # During the infusion: x(t) = V diag((exp(lambda t) - 1) / lambda) V^-1 rate
  during <- function(tt) {
    g <- outer(tt, lambda, function(s, l) (exp(l * s) - 1) / l)
    as.vector(g %*% (V[1, ] * (Vi %*% rate)))
  }
  xEnd <- V %*% (((exp(lambda * tinf) - 1) / lambda) * (Vi %*% rate))
  after <- function(tt) {
    g <- exp(outer(tt - tinf, lambda))
    as.vector(g %*% (V[1, ] * (Vi %*% xEnd)))
  }
  amount <- ifelse(t <= tinf, during(pmin(t, tinf)), after(pmax(t, tinf)))
  amount / (pk[["V1"]] * weight) * 1000
}

# van Steveninck's five end points, computed the way the paper defines them:
# Cmax the highest concentration (the end of the infusion), AUC 0-3 h and
# 0-8 h by the trapezoid rule (here on a 0.5 min grid), AUC 0-inf as dose /
# clearance, and the terminal half-life.
grid <- seq(0, 480, by = 0.5)
vsEndPoints <- function(pk) {
  cp <- infusionCp(pk, vs$weight, vs$dose, vs$tinf, grid)
  trap <- function(upto) {
    i <- grid <= upto
    sum(diff(grid[i]) * (utils::head(cp[i], -1) + utils::tail(cp[i], -1)) / 2) / 60
  }
  c(cmax = max(cp), auc3 = trap(180), auc8 = trap(480),
    aucInf = vs$dose / (pk[["CL"]] * vs$weight) * 1000,
    thalf = halfLives(pk)[2])
}

halPoints <- function(pk) infusionCp(pk, hal$weight, hal$dose, hal$tinf, hal$time)

# Oral dose at 70 kg with the absorption the model ships (not fitted): Muller
# 1987's absorption half-life of 0.38 h, bioavailability 0.92 (label).  Used
# only to compare the candidate fits with the oral studies.
oralCp <- function(pk, dose, t, weight = 70) {
  k <- rates(pk)
  ka <- log(2) / (0.38 * 60)
  M <- rbind(c(-ka, 0, 0),
             c(ka, -(k[["k10"]] + k[["k12"]]), k[["k21"]]),
             c(0, k[["k12"]], -k[["k21"]]))
  e <- eigen(M)
  coef <- solve(e$vectors, c(0.92 * dose, 0, 0))
  amount <- as.vector(exp(outer(t, e$values)) %*% (e$vectors[2, ] * coef))
  amount / (pk[["V1"]] * weight) * 1000
}


# ---- The objective ----------------------------------------------------------

# Least squares on log(model / observed): the five van Steveninck end points
# and the seven Halliday points, each term weighted equally (wHal = 1).  Log
# ratios put a 10% miss on a peak and on a half-life on the same footing.
objective <- function(pk, wHal = 1) {
  sum(log(vsEndPoints(pk) / vs$target)^2) +
    wHal * sum(log(halPoints(pk) / hal$conc)^2)
}
objectiveVsOnly <- function(pk) sum(log(vsEndPoints(pk) / vs$target)^2)

# Nelder-Mead on log parameters (keeps them positive), restarted from its own
# answer until it stops moving.
fitFrom <- function(start, fn) {
  names(start) <- c("V1", "CL", "Q", "V2")
  f <- function(lp) fn(stats::setNames(exp(lp), names(start)))
  lp <- log(start)
  value <- Inf
  repeat {
    o <- stats::optim(lp, f, method = "Nelder-Mead",
                      control = list(maxit = 5000, reltol = 1e-12))
    if (value - o$value < 1e-12) break
    lp <- o$par
    value <- o$value
  }
  stats::setNames(exp(o$par), names(start))
}


# ---- Fit ----------------------------------------------------------------------

# 1. van Steveninck alone, from a generic start (V1 0.3, V2 0.7 L/kg; CL 1.05
#    mL/min/kg, Q 0.6 L/h/kg).
vsOnly <- fitFrom(c(0.30, 0.063, 0.60, 0.70), objectiveVsOnly)

# 2. Both studies, started from the van Steveninck fit.
joint <- fitFrom(vsOnly, objective)

# 3. Sensitivity to the weighting: Halliday's block at half and twice the
#    weight of van Steveninck's.
halfW   <- fitFrom(joint, function(pk) objective(pk, 0.5))
doubleW <- fitFrom(joint, function(pk) objective(pk, 2))


# ---- Report -------------------------------------------------------------------

show <- function(label, pk) {
  hl <- halfLives(pk)
  cat(sprintf(paste0("%-24s V1 %.4f  CL %.4f  Q %.4f  V2 %.4f   ",
                     "(CL %.2f mL/min/kg; half-lives %.2f, %.1f h; Vss %.2f L/kg)\n"),
              label, pk[["V1"]], pk[["CL"]], pk[["Q"]], pk[["V2"]],
              pk[["CL"]] * 1000 / 60, hl[1], hl[2], pk[["V1"]] + pk[["V2"]]))
}
cat("Per-kg parameters: V1, V2 L/kg; CL, Q L/h/kg\n")
show("van Steveninck alone", vsOnly)
show("joint (shipped)", joint)
show("joint, Halliday x 0.5", halfW)
show("joint, Halliday x 2", doubleW)

cat("\nThe joint fit, rounded to 4 decimals, against the data:\n")
pk <- round(joint, 4)
cat("van Steveninck   ",
    sprintf("%s %s / %s = %.2f", names(vs$target),
            formatC(vsEndPoints(pk), format = "f", digits = 1),
            vs$target, vsEndPoints(pk) / vs$target), sep = "\n  ")
cat("\nHalliday (min: model / read, ratio)",
    sprintf("%3d: %6.1f / %4d = %.2f", hal$time, halPoints(pk), hal$conc,
            halPoints(pk) / hal$conc), sep = "\n  ")
cat("\nvan Steveninck alone, against its own end points:",
    sprintf("%.3f", vsEndPoints(vsOnly) / vs$target), "\n")
cat("van Steveninck alone, against Halliday:",
    sprintf("%.2f", halPoints(vsOnly) / hal$conc), "\n")

# Not part of the fit: what each candidate predicts for 20 mg by mouth at 70
# kg (observed soft-gelatin peaks 362-708 ng/mL; Muller 1987 morning 510 at
# 1.0 h; see docs/temazepam.md).
cat("\n20 mg by mouth, 70 kg (not fitted):\n")
to <- seq(0, 300, by = 0.1)
for (fit in list(list("van Steveninck alone", vsOnly), list("joint (shipped)", shipped),
                 list("joint, Halliday x 0.5", halfW),
                 list("joint, Halliday x 2", doubleW))) {
  cp <- oralCp(fit[[2]], 20, to)
  cat(sprintf("  %-24s peak %3.0f ng/mL at %4.1f min; %3.0f at 2 h\n",
              fit[[1]], max(cp), to[which.max(cp)], cp[which.min(abs(to - 120))]))
}
cat(sprintf("Objective: %.6f at the optimum, %.6f at the shipped rounding\n",
            objective(joint), objective(shipped)))

stopifnot(isTRUE(all.equal(round(joint, 4), shipped, tolerance = 1e-12)))
cat("\nOK: the fit rounds to the constants in R/drugs_temazepam.R\n")
