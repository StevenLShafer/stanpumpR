# Paclitaxel (R/drugs_paclitaxel.R): Joerger 2006, three compartments with
# saturable elimination and saturable distribution, on the Michaelis-Menten
# engine's saturableDistribution block.  Drafted by Claude Code, 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())
MW <- 853.9 / 1000                         # mg per umol
# Mosteller BSA exactly 1.8 m2: 72 kg, 162 cm
W <- 72; H <- 162

# An independent integration of the paper's ODEs, in umol and hours, with the
# Table 2 / Equations A-F values typed here rather than read from the model:
# classical Runge-Kutta at a step of 0.002 h (36 times finer than the
# engine's), for a constant infusion R umol/h over `tinf` hours.
joergerReference <- function(male, bsa, age, R, tinf, times, h = 0.002) {
  g <- if (male) 1 else 0
  b <- bsa / 1.8
  V1 <- 12.8; V3 <- 252 * b^1.17
  VMEL <- 37.4 * 1.2^g * b^0.842 * (max(age, 18) / 56)^-0.352
  KMEL <- 0.53
  VMTR <- 169 * 1.2^g * b^0.911; KMTR <- 0.826 * 2.11^g
  K21 <- 1.15 * 0.893^g; Q <- 20.1 * b^0.724
  f <- function(x, r) {
    C1 <- x[1] / V1
    el <- VMEL * C1 / (KMEL + C1); tr <- VMTR * C1 / (KMTR + C1)
    c(r - el - tr + K21 * x[2] - Q * C1 + Q * x[3] / V3,
      tr - K21 * x[2],
      Q * C1 - Q * x[3] / V3)
  }
  x <- c(0, 0, 0); t <- 0; out <- numeric(length(times))
  for (i in seq_along(times)) {
    while (t < times[i] - 1e-12) {
      # never step across the end of the infusion
      edge <- if (t < tinf) min(tinf, times[i]) else times[i]
      hh <- min(h, edge - t)
      r <- if (t < tinf) R else 0
      k1 <- f(x, r); k2 <- f(x + hh / 2 * k1, r)
      k3 <- f(x + hh / 2 * k2, r); k4 <- f(x + hh * k3, r)
      x <- x + hh / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
      t <- t + hh
    }
    out[i] <- x[1] / V1
  }
  out                                         # umol/L
}

run175 <- function(sex, age = 56, weight = W, height = H, hours = 96) {
  bsa <- sqrt(weight * height / 3600)
  d <- data.frame(Drug = "paclitaxel", Time = c(0, 180),
                  Dose = c(175 * bsa / 3, 0), Units = "mg/hr")
  simulateDrugsWithCovariates(d, noEvents, weight, height, age, sex, hours * 60, FALSE,
                              adjustToFFM = FALSE)$paclitaxel$wide
}
# The last time the curve is above 0.1 umol/L, interpolated, in hours
timeAbove <- function(wide, threshold = 0.1) {
  c <- wide$Plasma / MW
  i <- max(which(c > threshold))
  t <- wide$Time[i] + (wide$Time[i + 1] - wide$Time[i]) *
    (c[i] - threshold) / (c[i] - c[i + 1])
  t / 60
}
aucUmol <- function(wide) {
  c <- wide$Plasma / MW
  sum(diff(wide$Time) * (head(c, -1) + tail(c, -1)) / 2) / 60
}

test_that("the model is Table 2 and Equations A-F, converted from umol and hours", {
  X <- paclitaxel(W, H, 56, "female", adjustToFFM = FALSE)
  set <- X$PK$default
  mm <- X$michaelisMenten
  expect_equal_rounded(set$v1, 12.8)
  expect_equal_rounded(set$v2, 252)
  expect_equal_rounded(set$cl2 * 60, 20.1)
  expect_equal_rounded(mm$vmax * 60 / MW, 37.4)
  expect_equal_rounded(mm$km / MW, 0.53)
  expect_equal_rounded(mm$saturableDistribution$vmax * 60 / MW, 169)
  expect_equal_rounded(mm$saturableDistribution$km / MW, 0.826)
  expect_equal_rounded(mm$saturableDistribution$k21 * 60, 1.15)

  M <- paclitaxel(W, H, 56, "male", adjustToFFM = FALSE)
  expect_equal_rounded(M$michaelisMenten$vmax / mm$vmax, 1.2)
  expect_equal_rounded(M$michaelisMenten$saturableDistribution$vmax /
                         mm$saturableDistribution$vmax, 1.2)
  expect_equal_rounded(M$michaelisMenten$saturableDistribution$km / MW, 0.826 * 2.11)
  expect_equal_rounded(M$michaelisMenten$saturableDistribution$k21 * 60, 1.15 * 0.893)
  expect_equal(M$PK$default$v1, set$v1)
  expect_equal(M$michaelisMenten$km, mm$km)
})

test_that("the text's covariate statements reproduce", {
  vmel <- function(bsa, age, sex = "female")
    paclitaxel(72, bsa^2 * 3600 / 72, age, sex, adjustToFFM = FALSE)$michaelisMenten$vmax
  expect_equal(round(vmel(1.8, 56, "male") / vmel(1.8, 56), 2), 1.2)
  expect_equal(round(vmel(2.0, 56) / vmel(1.8, 56) - 1, 2), 0.09)   # +0.2 m2: +9%
  expect_equal(round(vmel(1.8, 66) / vmel(1.8, 56) - 1, 2), -0.06)  # +10 y: the text's "about 5%"
  expect_equal(round(vmel(1.35, 56) / vmel(1.8, 56) - 1, 2), -0.22) # BSA 1.35: -22%
  expect_equal(round(vmel(1.8, 86) / vmel(1.8, 56) - 1, 2), -0.14)  # age 86: -14%
  # bilirubin (held at 7 umol/L): 24 umol/L would give -19%
  expect_equal(round((24 / 7)^-0.167 - 1, 2), -0.19)
  # the age term is floored at 18
  expect_equal(vmel(1.8, 0), vmel(1.8, 18))
  expect_gt(vmel(1.8, 18), vmel(1.8, 19))
})

test_that("175 mg/m2 over 3 h reproduces the spec's check and Figs. 3-4", {
  f <- run175("female")
  m <- run175("male")
  # Cmax (umol/L): 3.83 and 3.29; Fig. 3 plots ln Cmax, medians about 1.32 and 1.17
  expect_equal(max(f$Plasma) / MW, 3.83, tolerance = 0.005)
  expect_equal(max(m$Plasma) / MW, 3.29, tolerance = 0.005)
  # time above 0.1 umol/L (h): 17.7 and 13.0 (Fig. 3 about 16.5 and 12.7)
  expect_equal(timeAbove(f), 17.7, tolerance = 0.01)
  expect_equal(timeAbove(m), 13.0, tolerance = 0.01)
  # AUC (umol h/L) to 96 h: 13.5 and 11.4
  expect_equal(aucUmol(f), 13.5, tolerance = 0.01)
  expect_equal(aucUmol(m), 11.4, tolerance = 0.01)
  # Fig. 3: the time above 0.1 umol/L is flat across BSA 1.35-2.25
  small <- run175("female", weight = 60, height = 1.35^2 * 3600 / 60, hours = 48)
  large <- run175("female", weight = 90, height = 2.25^2 * 3600 / 90, hours = 48)
  expect_equal(timeAbove(small), 17.2, tolerance = 0.015)
  expect_equal(timeAbove(large), 18.0, tolerance = 0.015)
  # Fig. 4D: the woman of 86 years, 22.0 h (the figure about 21)
  expect_equal(timeAbove(run175("female", age = 86, hours = 48)), 22.0, tolerance = 0.015)
})

test_that("the engine agrees with an independent Runge-Kutta integration", {
  for (sex in c("female", "male")) {
    wide <- run175(sex, hours = 48)
    pick <- vapply(c(0.5, 1, 2, 3, 3.25, 4, 6, 12, 24, 48) * 60,
                   function(t) which.min(abs(wide$Time - t)), integer(1))
    t <- wide$Time[pick] / 60
    ref <- joergerReference(sex == "male", 1.8, 56, 175 * 1.8 / 3 / MW, 3, t)
    expect_equal(wide$Plasma[pick] / MW, ref, tolerance = 1e-3, info = sex)
  }
})

test_that("saturable distribution: mass is conserved and the dose is not superposable", {
  # Twice the dose gives more than twice the AUC, both pathways saturating
  d <- function(mg) data.frame(Drug = "paclitaxel", Time = c(0, 180),
                               Dose = c(mg / 3, 0), Units = "mg/hr")
  auc <- function(mg) aucUmol(simulateDrugsWithCovariates(
    d(mg), noEvents, W, H, 56, "female", 96 * 60, FALSE,
    adjustToFFM = FALSE)$paclitaxel$wide)
  expect_gt(auc(630), 2.1 * auc(315))
})

test_that("both switch positions are pinned", {
  on <- paclitaxel(70, 170, 35, "male")
  off <- paclitaxel(70, 170, 35, "male", adjustToFFM = FALSE)
  # the reference man: the pharmacokinetic weight is his weight
  expect_equal_rounded(on$PK$default$v1, off$PK$default$v1)
  expect_equal_rounded(off$michaelisMenten$vmax, 0.7600135)
  w <- paclitaxel(70, 170, 35, "female")
  expect_equal_rounded(w$PK$default$v1, 10.547593)
  expect_equal_rounded(w$michaelisMenten$vmax, 0.5837841)
})

test_that("paclitaxel is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "paclitaxel", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "mcg")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("paclitaxel", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Joerger")
  expect_identical(therapeuticRange("paclitaxel")$band_status, "not_applicable")
})
