# PPX, ropivacaine's toxic metabolite: see the header of R/drugs_PPX.R.
#
# The pins are Sunder 2026 Table 2 (total PPX).  The curves are checked
# against the compartmental rate equations written out below -- the library
# ropivacaine's three compartments (switch off), its tissue depot, the
# transit compartment and PPX's one compartment -- solved by a matrix
# exponential (scaling and squaring of a Taylor series, base R), sharing no
# code with the package's closed form.  The same reference, with Sunder's
# three delay tanks, measures what carrying them as one costs.  The adult
# checks are Burm 2000 and Ekatodramis 2003, which the model was not fitted
# to.
#
# (Claude Code, 2026-11-17, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# exp(M) by scaling and squaring a Taylor series: no eigenvectors, so a chain
# of identical tanks (a repeated eigenvalue) is no trouble.
expmTaylor <- function(M) {
  s <- max(0, ceiling(log2(max(abs(M)) * nrow(M))) + 1)
  A <- M / 2^s
  E <- diag(nrow(M)); term <- E
  for (k in 1:30) { term <- term %*% A / k; E <- E + term }
  for (i in seq_len(s)) E <- E %*% E
  E
}

# States (mg): ropivacaine central, peripheral 1, peripheral 2, tissue depot,
# `tanks` delay compartments, PPX.  All of ropivacaine's elimination forms
# PPX, mass for mass, into the first tank; the tanks share Sunder's 4.9 min
# mean.  Inputs: a constant rate into the depot.
ppxSystem <- function(weight, tanks = 1) {
  r <- ropivacaine(weight, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  n <- 4 + tanks + 1
  K <- matrix(0, n, n)
  k10 <- r$cl1 / r$v1; k12 <- r$cl2 / r$v1; k13 <- r$cl3 / r$v1
  k21 <- r$cl2 / r$v2; k31 <- r$cl3 / r$v3
  K[1, 1] <- -(k10 + k12 + k13); K[1, 2] <- k21; K[1, 3] <- k31; K[1, 4] <- r$ka_RA
  K[2, 1] <- k12; K[2, 2] <- -k21
  K[3, 1] <- k13; K[3, 3] <- -k31
  K[4, 4] <- -r$ka_RA
  kt <- tanks / 4.9
  for (j in seq_len(tanks)) {
    i <- 4 + j
    K[i, i] <- -kt
    if (j == 1) K[i, 1] <- k10 else K[i, i - 1] <- kt
  }
  vPPX <- 351 * weight / 40
  clPPX <- 0.29 * (weight / 40)^0.75
  K[n, 4 + tanks] <- kt; K[n, n] <- -clPPX / vPPX
  list(K = K, n = n, v1 = r$v1, vPPX = vPPX)
}

# Ropivacaine and PPX (mg/L) at `at` minutes after tissue boluses at
# `times` and a depot rate (mg/min) from `rateFrom`.  The augmented system
# (a unit state driving the rate) keeps a constant input exact.
ppxReference <- function(weight, times, doses, at, rate = 0, rateFrom = Inf, tanks = 1) {
  S <- ppxSystem(weight, tanks)
  n <- S$n
  M <- rbind(cbind(S$K, c(0, 0, 0, 1, rep(0, n - 4))), 0)
  step <- function(x, h, r) {
    E <- expmTaylor(M * h)
    (E %*% c(x, r))[1:n]
  }
  x <- rep(0, n); t <- 0
  ev <- sort(unique(c(times[times < at], if (rateFrom < at) rateFrom)))
  for (tt in ev) {
    x <- step(x, tt - t, if (t >= rateFrom) rate else 0); t <- tt
    x[4] <- x[4] + sum(doses[times == tt])
  }
  x <- step(x, at - t, if (t >= rateFrom) rate else 0)
  c(ropivacaine = x[1] / S$v1, PPX = x[n] / S$vPPX)
}

engineAt <- function(DT, at, weight) {
  o <- simulateDrugsWithCovariates(DT[DT$Time < at, ], noEvents, weight, 170, 35, "male",
                                   at, FALSE, adjustToFFM = FALSE)
  r <- o$ropivacaine$wide; p <- o$PPX$wide
  c(ropivacaine = r$Plasma[nrow(r)], PPX = p$Plasma[nrow(p)])
}


test_that("PPX is Sunder 2026's total PPX, scaled by total body mass", {
  at40 <- PPX(40, 150, 12, "female")$PK$default
  expect_equal(at40$v1, 351)
  expect_equal(at40$cl1, 0.29)
  expect_equal(c(at40$cl2, at40$cl3), c(0, 0))
  # 70 kg: V x 70/40, CL x (70/40)^0.75 = 1.5215231 (by hand)
  at70 <- PPX(70, 170, 35, "male")$PK$default
  expect_equal(at70$v1, 614.25)
  expect_equal(at70$cl1, 0.29 * 1.5215231, tolerance = 1e-6)
  expect_equal(PPX(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default, at70)
  PK <- getDrugPK("PPX", 70, 170, 35, "male", getDrugDefaults("PPX"))$PK$default
  expect_equal(log(2) / PK$lambda_1 / 60, 16.08, tolerance = 0.001)
  expect_match(PPX(70, 170, 35, "male")$reference, "Anesthesiology 2026;144:90-102", fixed = TRUE)
})

test_that("the engine follows the rate equations, block and perineural infusion", {
  # Ekatodramis 2003's regimen at 75 kg: 225 mg at 0, 18 mg/h from 6 to 54 h.
  DT <- data.frame(Drug = "ropivacaine", Time = c(0, 360, 3240), Dose = c(225, 18, 0),
                   Units = c("mg RA", "mg/hr RA", "mg/hr RA"))
  for (at in c(10, 60, 360, 1440, 3240, 4000)) {
    ref <- if (at <= 3240) {
      ppxReference(75, 0, 225, at, rate = 18 / 60, rateFrom = 360)
    } else {
      NULL
    }
    if (is.null(ref)) next
    expect_equal(engineAt(DT, at, 75), ref, tolerance = 1e-6, label = paste("minute", at))
  }
})

test_that("one transit tank for Sunder's three costs under 0.1% of the peak", {
  times <- c(1:60, seq(62, 2880, 2))
  one   <- vapply(times, function(t) ppxReference(70, 0, 150, t, tanks = 1)[["PPX"]], 0)
  three <- vapply(times, function(t) ppxReference(70, 0, 150, t, tanks = 3)[["PPX"]], 0)
  expect_lt(max(abs(one - three)) / max(three), 0.001)
})

test_that("against two adult studies it was not fitted to, within one SD", {
  # Burm 2000: 50 mg then 20 or 30 mg/h epidural for 72 h, 71 and 82 kg:
  # 1.0 +/- 0.4 and 1.2 +/- 0.4 mg/L total PPX at the end.  Ekatodramis 2003:
  # 225 mg interscalene, 12 or 18 mg/h from 6 to 54 h, 83 and 75 kg: 0.35 +/-
  # 0.18 and 0.70 +/- 0.38.  The model's values are the header's.
  endPPX <- function(DT, at, weight) {
    o <- simulateDrugsWithCovariates(DT, noEvents, weight, 170, 60, "male", at, FALSE)
    w <- o$PPX$wide
    w$Plasma[nrow(w)]
  }
  burm <- function(rate) data.frame(Drug = "ropivacaine", Time = c(0, 0), Dose = c(50, rate),
                                    Units = c("mg RA", "mg/hr RA"))
  ekat <- function(rate) data.frame(Drug = "ropivacaine", Time = c(0, 360), Dose = c(225, rate),
                                    Units = c("mg RA", "mg/hr RA"))
  got <- c(endPPX(burm(20), 4320, 71), endPPX(burm(30), 4320, 82),
           endPPX(ekat(12), 3240, 83), endPPX(ekat(18), 3240, 75))
  expect_equal(got, c(0.708, 0.946, 0.371, 0.582), tolerance = 0.002)
  observed <- c(1.0, 1.2, 0.35, 0.70); sd <- c(0.4, 0.4, 0.18, 0.38)
  expect_true(all(abs(got - observed) <= sd))
})

test_that("the ropivacaine curve is unchanged by the metabolite", {
  PK <- getDrugPK("ropivacaine", 70, 170, 35, "male", getDrugDefaults("ropivacaine"))
  noMet <- PK
  noMet$PK$default$metabolite <- NULL
  noMet$metaboliteName <- NULL
  DT <- data.frame(Drug = "ropivacaine", Time = c(0, 0, 60, 300), Dose = c(20, 150, 10, 0),
                   Units = c("mg", "mg RA", "mg/hr RA", "mg/hr RA"))
  a <- simCpCe(DT, noEvents, PK, 600, FALSE)
  b <- simCpCe(DT, noEvents, noMet, 600, FALSE)
  common <- intersect(a$wide$Time, b$wide$Time)
  expect_gt(length(common), 50)
  expect_equal(a$wide$Plasma[match(common, a$wide$Time)],
               b$wide$Plasma[match(common, b$wide$Time)], tolerance = 1e-10)
})

test_that("PPX has no dosing unit, no effect site and no band, and a red label", {
  d <- getDrugDefaults("PPX")
  expect_true(is.na(d$Units) || d$Units == "")
  expect_true(is.na(d$Category) || d$Category == "")
  expect_equal(c(d$Lower, d$Upper, d$Typical, d$MEAC, d$endCe), rep(0, 5))
  expect_true("PPX" %in% TOXIC_METABOLITES)
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "ropivacaine", Time = 0, Dose = 150, Units = "mg RA"),
    noEvents, 70, 170, 35, "male", 600, FALSE)
  expect_equal(o$PPX$formedFrom, "ropivacaine")
  expect_true(all(is.na(o$PPX$wide$"Effect Site")))
})
