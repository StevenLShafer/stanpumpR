# Trefentanil (R/drugs_trefentanil.R): Lemmens 1994's crossover-phase model,
# whose three-compartment volumes and clearances are DERIVED from the
# fractional coefficients and hybrid rate constants of the paper's Table V.
# Drafted by Claude Code, 2026-10-18, at the request of Steven L. Shafer.
#
# The expectations here are built from the published numbers INDEPENDENTLY of
# the drug file: the inversion is re-implemented below from Table V, checked
# against the paper's own per-subject Vss and clearances, and only then
# compared with what R/drugs_trefentanil.R carries.

# Lemmens HJM, Dyck JB, Shafer SL, Stanski DR. Clin Pharmacol Ther
# 1994;56(3):261-271, Table V, trefentanil, the five crossover subjects.
# Fractional coefficients A/B/C (summing to 1), hybrid rate constants in
# min^-1, central volume in L, and the paper's own derived values.
tv <- data.frame(
  A   = c(0.74,   0.88,   0.84,   0.94,   0.80),
  al  = c(0.60,   1.04,   0.62,   0.23,   0.52),
  B   = c(0.24,   0.11,   0.14,   0.04,   0.17),
  be  = c(0.070,  0.067,  0.045,  0.017,  0.049),
  Cc  = c(0.018,  0.013,  0.013,  0.018,  0.026),
  ga  = c(0.0053, 0.0044, 0.0069, 0.0050, 0.0065),
  Vc  = c(3.4,    1.8,    3.3,    5.1,    3.8),
  # as printed in the same table
  Vss   = c(36,    43,    28,    44,    32),
  CLc   = c(0.427, 0.340, 0.519, 0.510, 0.425),
  CLdis = c(1.16,  1.34,  1.26,  0.61,  1.20),
  # Table I, crossover phase
  wt    = c(77,    88,    78,    81,    75))

# Table VI, trefentanil, crossover means
T_HALF_KE0 <- 1.20     # min
EEG_EC50   <- 429      # ng/mL -- an EEG spectral-edge EC50, NOT a MEAC

REFERENCE_WEIGHT <- 70 # the library's reference man (docs/weight-adjustment.md)

# c(t)/Dose = (1/V1)(A e^-at + B e^-bt + C e^-gt) -> micro constants -> volumes
# and clearances.  Written out here rather than imported, so that the test does
# not share an implementation with the code it checks.
invertTriexponential <- function(A, a, B, b, Cc, g, V1) {
  Ap <- A / V1; Bp <- B / V1; Cp <- Cc / V1; S <- Ap + Bp + Cp
  k21k31  <- (Ap * b * g + Bp * a * g + Cp * a * b) / S
  k21pk31 <- (Ap * (b + g) + Bp * (a + g) + Cp * (a + b)) / S
  root <- sqrt(k21pk31^2 - 4 * k21k31)
  k21 <- (k21pk31 + root) / 2
  k31 <- (k21pk31 - root) / 2
  k10 <- a * b * g / (k21 * k31)
  Q <- (a + b + g) - k10 - k21 - k31                        # k12 + k13
  P <- (a*b + a*g + b*g) - k10 * (k21 + k31) - k21 * k31    # k12 k31 + k13 k21
  k12 <- (P - Q * k21) / (k31 - k21)
  k13 <- Q - k12
  list(v1 = V1, v2 = V1 * k12 / k21, v3 = V1 * k13 / k31,
       cl1 = V1 * k10, cl2 = V1 * k12, cl3 = V1 * k13)
}

perSubject <- lapply(seq_len(nrow(tv)), function(i)
  invertTriexponential(tv$A[i], tv$al[i], tv$B[i], tv$be[i],
                       tv$Cc[i], tv$ga[i], tv$Vc[i]))

test_that("the inversion reproduces every subject's own published values", {
  # This is what makes the derivation checkable: Table V prints Vss, the
  # elimination clearance and the distribution clearance for each subject, and
  # none of them is used to build the model.  The residual is the two
  # significant figures the coefficients are printed to.
  for (i in seq_len(nrow(tv))) {
    p <- perSubject[[i]]
    expect_equal(p$v1 + p$v2 + p$v3, tv$Vss[i],   tolerance = 0.02, info = i)
    expect_equal(p$cl1,              tv$CLc[i],   tolerance = 0.03, info = i)
    expect_equal(p$cl2 + p$cl3,      tv$CLdis[i], tolerance = 0.02, info = i)
    # physically admissible mammillary model
    expect_true(all(unlist(p) > 0), info = i)
  }
})

test_that("inverting the printed MEAN row would not have worked", {
  # Why the drug file inverts per subject and averages.  Averaging does not
  # commute with the non-linear inversion, so the mean coefficients carry
  # neither the mean volume nor the mean clearance.
  m <- invertTriexponential(0.84, 0.60, 0.14, 0.050, 0.017, 0.0056, 3.5)
  expect_gt(abs(m$v1 + m$v2 + m$v3 - 37) / 37, 0.05)      # 40.0 L against 37
  expect_gt(abs(m$cl1 - 0.444) / 0.444, 0.05)             # 0.482 against 0.444
  expect_gt(abs(m$cl2 + m$cl3 - 1.11) / 1.11, 0.15)       # 1.31 against 1.11
})

test_that("the averaged per-subject parameters land on the paper's means", {
  mean_of <- function(f) mean(vapply(perSubject, f, numeric(1)))
  expect_equal(mean_of(function(p) p$v1 + p$v2 + p$v3), 37,    tolerance = 0.02)
  expect_equal(mean_of(function(p) p$cl1),              0.444, tolerance = 0.02)
  expect_equal(mean_of(function(p) p$cl2 + p$cl3),      1.11,  tolerance = 0.02)
  expect_equal(mean(tv$wt), 79.8, tolerance = 1e-9)
})

test_that("the reference man carries the derived set, scaled from 79.8 to 70 kg", {
  mean_of <- function(f) mean(vapply(perSubject, f, numeric(1)))
  fv <- REFERENCE_WEIGHT / mean(tv$wt)          # volumes, linear in weight
  fc <- fv^0.75                                 # clearances, allometric
  expected <- list(
    v1  = mean_of(function(p) p$v1)  * fv,
    v2  = mean_of(function(p) p$v2)  * fv,
    v3  = mean_of(function(p) p$v3)  * fv,
    cl1 = mean_of(function(p) p$cl1) * fc,
    cl2 = mean_of(function(p) p$cl2) * fc,
    cl3 = mean_of(function(p) p$cl3) * fc)

  # The reference man's scaling factors are exactly 1, so he receives the
  # model's parameters unchanged, with the switch on or off.
  for (adj in c(TRUE, FALSE)) {
    actual <- trefentanil(70, 170, 35, "male", adjustToFFM = adj)$PK$default
    for (p in names(expected))
      expect_equal_rounded(expected[[p]], actual[[p]], tolerance = 1e-5)
  }
})

test_that("the published parameters are pinned at both switch positions", {
  # A patient away from the reference man, so the two scalings differ.
  ffm <- trefentanil(100, 180, 40, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(3.8920856,  ffm$v1)
  expect_equal_rounded(9.1631101,  ffm$v2)
  expect_equal_rounded(27.8780462, ffm$v3)
  expect_equal_rounded(0.4776520,  ffm$cl1)
  expect_equal_rounded(0.9545728,  ffm$cl2)
  expect_equal_rounded(0.2439792,  ffm$cl3)

  legacy <- trefentanil(100, 180, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(4.3609023,  legacy$v1)
  expect_equal_rounded(10.2668419, legacy$v2)
  expect_equal_rounded(31.2360639, legacy$v3)
  expect_equal_rounded(0.5686991,  legacy$cl1)
  expect_equal_rounded(1.1365277,  legacy$cl2)
  expect_equal_rounded(0.2904850,  legacy$cl3)
  # With the switch off the library scales both volumes and clearances
  # linearly in weight, so every ratio is 100/70.
  for (p in names(legacy))
    expect_equal_rounded(100 / 70,
      legacy[[p]] / trefentanil(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default[[p]])
})

test_that("ke0 is the published equilibration half-time, and tPeak is solved from it", {
  X <- getDrugPK("trefentanil", 70, 170, 35, "male")
  expect_equal_rounded(log(2) / T_HALF_KE0, X$PK$default$ke0)
  # The drug supplies ke0 and no tPeak, so getDrugPK() solves the true time to
  # peak effect from the ke0 and the disposition.
  expect_null(trefentanil(70, 170, 35, "male")$tPeak)
  expect_gt(X$tPeak, 1)
  expect_lt(X$tPeak, 4)
  expect_equal_rounded(2.057056308, X$tPeak, tolerance = 1e-6)
})

test_that("trefentanil is plasma only: the EEG EC50 is not a MEAC", {
  X <- trefentanil(70, 170, 35, "male")
  expect_equal(X$MEAC, 0)
  expect_equal(c(X$lowerTypical, X$typical, X$upperTypical), c(0, 0, 0))
  # The EEG EC50 is deliberately not carried as a concentration target: the
  # same table gives fentanyl 9.8 ng/mL where this library's fentanyl MEAC is
  # 0.6, so the two quantities are more than an order of magnitude apart.
  expect_gt(EEG_EC50 / getDrugDefaultsGlobal(FALSE)$MEAC[
    getDrugDefaultsGlobal(FALSE)$Drug == "fentanyl"], 100)

  row <- getDrugDefaultsGlobal(FALSE)
  row <- row[row$Drug == "trefentanil", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$Category, "Opioids")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC, row$endCe),
               c(0, 0, 0, 0, 0))
})

test_that("the simulated decrement curves reproduce the paper's Figure 6", {
  # Lemmens's headline simulation: the time for a 50% decrease in effect-site
  # concentration after holding it constant, against infusion duration.
  # Trefentanil recovers fastest at every duration.  The comparators come from
  # the library's OWN fentanyl and alfentanil models, which were fitted to
  # different subjects by different investigators, so this is an external
  # check rather than a restatement of the trefentanil parameters.
  noEvents <- data.frame(Time = numeric(0), Event = character(0))
  decrement <- function(drug, T, tail = 1200) {
    dose <- data.frame(Drug = drug, Time = c(0, T), Dose = c(1, 0),
                       Units = "Effect site target", stringsAsFactors = FALSE)
    w <- simulateDrugsWithCovariates(dose, noEvents, 70, 170, 33, "male",
                                     T + tail, FALSE)[[drug]]$wide
    ce <- w[["Effect Site"]]
    i0 <- max(which(w$Time <= T))
    j <- which(w$Time > T & ce <= ce[i0] / 2)
    if (!length(j)) return(NA_real_)
    w$Time[j[1]] - T
  }
  for (T in c(10, 60, 240, 600)) {
    tref <- decrement("trefentanil", T)
    expect_lt(tref, decrement("alfentanil", T))
    expect_lt(tref, decrement("fentanyl", T))
  }
  # Figure 6 shows the five subjects' trefentanil curves between about 15 and
  # 35 minutes at a 600 minute infusion; the mean-parameter model sits inside
  # that band.
  d600 <- decrement("trefentanil", 600)
  expect_gt(d600, 15)
  expect_lt(d600, 35)
})
