# Dabigatran after total hip replacement (Trocóniz 2007): a time-varying
# two-compartment oral model whose absorption and clearance change at 24 h.
# See the header of R/drugs_dabigatranHip.R and the phaseSwitch path in
# R/simCpCe.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published two-phase parameters at the reference subject", {
  m <- dabigatranHip(78.21, 170.5, 66.97, "male", adjustToFFM = FALSE,
                     creatinine = 0.964)
  # Shared disposition (Table III).
  expect_equal(m$PK$default$v1, 30.8, tolerance = 1e-9)   # V2/F central
  expect_equal(m$PK$default$v2, 136,  tolerance = 1e-9)   # V3/F peripheral
  expect_equal(m$PK$default$cl2 * 60, 13.6, tolerance = 1e-9)  # Q/F
  expect_equal(m$PK$default$tlag_PO, 0.4 * 60, tolerance = 1e-9)
  expect_equal(m$PK$default$v3, 1)
  expect_equal(m$PK$default$cl3, 0)
  # Phase 1 (days 0-1): CL/F 70.87 L/h at the median gastrin, ka 0.022 /h.
  expect_equal(m$PK$default$cl1 * 60, 43.4 * 1.633, tolerance = 1e-6)  # 70.87
  expect_equal(m$PK$default$ka_PO * 60, 0.022, tolerance = 1e-9)
  # Phase 2 (days 2-10): CL/F = 82.1 x (CrCl/76.17) x 1.294; ka at the median
  # serum creatinine and age is 0.265 x 0.637 x 0.553 = 0.0933 /h.
  crcl <- creatinineClearanceCG(78.21, 66.97, "male", 0.964)
  expect_equal(m$PK$day2$cl1 * 60, 82.1 * (crcl / 76.17) * 1.294, tolerance = 1e-6)
  expect_equal(m$PK$day2$ka_PO * 60,
               0.265 * (1 - 0.363) * (1 - 0.447), tolerance = 1e-6)  # 0.0933
  expect_equal(m$dosePhase$at, 24 * 60)
  expect_equal(m$dosePhase$late, "day2")
  expect_equal(m$MEAC, 0)
})

test_that("creatinine clearance raises the day-2 clearance; creatinine and age lower its ka", {
  base <- dabigatranHip(78, 170, 60, "male", adjustToFFM = FALSE, creatinine = 1.0)
  # Worse renal function (higher creatinine) -> lower CrCl -> lower day-2 CL/F.
  worse <- dabigatranHip(78, 170, 60, "male", adjustToFFM = FALSE, creatinine = 1.6)
  expect_lt(worse$PK$day2$cl1, base$PK$day2$cl1)
  # Higher creatinine and older age both lower the day-2 absorption rate.
  expect_lt(worse$PK$day2$ka_PO, base$PK$day2$ka_PO)
  older <- dabigatranHip(78, 170, 85, "male", adjustToFFM = FALSE, creatinine = 1.0)
  expect_lt(older$PK$day2$ka_PO, base$PK$day2$ka_PO)
  # Phase 1 absorption and gastrin-only clearance do not read these fields.
  expect_equal(worse$PK$default$ka_PO, base$PK$default$ka_PO)
  expect_equal(worse$PK$default$cl1, base$PK$default$cl1)
})

test_that("the day-2 absorption factors are held positive beyond the fitted range", {
  m <- dabigatranHip(78, 170, 60, "male", adjustToFFM = FALSE, creatinine = 5)
  expect_gt(m$PK$day2$ka_PO, 0)
})

test_that("body size scales the size-free parameters in both adjustToFFM positions", {
  # Switch off: published values, CrCl at total body weight.
  off <- dabigatranHip(90, 180, 60, "male", adjustToFFM = FALSE, creatinine = 1)
  expect_equal(off$PK$default$v1, 30.8, tolerance = 1e-9)
  expect_equal(off$PK$default$cl1 * 60, 43.4 * 1.633, tolerance = 1e-6)
  # Switch on: the library fat-free-mass factors scale v and CL.
  on  <- dabigatranHip(120, 170, 50, "male", creatinine = 1)
  vol <- 1.3049067; clr <- 1.2209126   # pinned in test-drugs-alfentanil.R
  expect_equal(on$PK$default$v1, 30.8 * vol, tolerance = 1e-5)
  expect_equal(on$PK$default$v2, 136 * vol, tolerance = 1e-5)
  expect_equal(on$PK$default$cl2 * 60, 13.6 * clr, tolerance = 1e-5)
  expect_equal(on$PK$default$cl1 * 60, 43.4 * 1.633 * clr, tolerance = 1e-5)
})

test_that("the plotted concentration is plasma only, with no effect site", {
  dose <- data.frame(Drug = "dabigatranHip", Time = 0, Dose = 150,
                     Units = "mg PO bid", stringsAsFactors = FALSE)
  X <- simulateDrugsWithCovariates(dose, noEvents, 78, 170, 66, "male",
                                   maximum = 10 * 1440, plotRecovery = FALSE,
                                   creatinine = 1.0)
  r <- X$dabigatranHip$results
  expect_true(all(is.na(r$Y[r$Site == "Effect Site"])))
  expect_gt(max(r$Y[r$Site == "Plasma"]), 0)
})

test_that("each dose carries its phase, and the two phases superpose exactly", {
  # Dose-time semantics: a dose given before 24 h runs entirely on the day-0-1
  # parameters, a dose given at or after 24 h on the day-2-10 parameters, and
  # the two are independent linear systems.  With both dose TIMES fixed as knots
  # (zero-dose rows), the two-dose curve must equal the sum of the two
  # single-dose curves on the shared grid, to numerical precision -- the
  # superposition the dosePhase split relies on.
  mk <- function(d1, d2) data.frame(
    Drug = "dabigatranHip", Time = c(0, 1500), Dose = c(d1, d2),
    Units = "mg PO", stringsAsFactors = FALSE)
  sim <- function(dt) simulateDrugsWithCovariates(
    dt, noEvents, 78, 170, 66, "male", maximum = 4 * 1440,
    plotRecovery = FALSE, creatinine = 1.0)$dabigatranHip$wide$Plasma
  both <- sim(mk(150, 150)); early <- sim(mk(150, 0)); late <- sim(mk(0, 150))
  expect_equal(both, early + late, tolerance = 1e-9)
  # the day-2 dose really is on the faster day-2 absorption, not day-0-1: its
  # own peak comes well before a phase-1 dose's would.
  expect_gt(max(late), max(early))
})

test_that("a single dose matches an independent integration of its phase", {
  # Absolute accuracy of one phase-2 dose: the processed PK run through the
  # standard oral engine on its own (exact) grid, against a midpoint (RK2)
  # integration of the two-compartment system in the engine's own units.  This
  # checks the day-2 disposition and absorption produce the right curve, apart
  # from the display grid the plot later resamples onto.
  dd  <- getDrugDefaultsGlobal()
  pk  <- getDrugPK("dabigatranHip", 78, 170, 66, "male",
                   dd[dd$Drug == "dabigatranHip", ], adjustToFFM = FALSE,
                   creatinine = 1.0)
  set <- pk$PK[["day2"]]
  v1 <- set$v1; k12 <- set$cl2 / v1; k21 <- set$cl2 / set$v2
  ka <- set$ka_PO; k10 <- set$cl1 / v1; lag <- set$tlag_PO
  dose <- data.frame(Drug = "dabigatranHip", Time = 0, Dose = 150, Units = "mg PO",
                     Bolus = FALSE, PO = TRUE, IM = FALSE, IN = FALSE, SL = FALSE,
                     RA = FALSE, RAslow = FALSE, RArate = FALSE, PO2 = FALSE,
                     stringsAsFactors = FALSE)
  eng <- advanceClosedFormPO_IM_IN(dose, set, 48 * 60, FALSE, 0)
  eng <- eng[eng$Time > 0, ]
  ode <- function(times, h = 0.02) {
    A1 <- 0; A2 <- 0; A3 <- 0; t <- 0; given <- FALSE
    out <- numeric(length(times)); j <- 1; tmax <- max(times)
    dv <- function(A1, A2, A3) c(-ka * A1, ka * A1 + k21 * A3 - (k10 + k12) * A2,
                                 k12 * A2 - k21 * A3)
    for (i in seq_len(ceiling(tmax / h))) {
      if (!given && t + h / 2 >= lag) { A1 <- A1 + 150; given <- TRUE }
      d1 <- dv(A1, A2, A3); Am <- c(A1, A2, A3) + h / 2 * d1
      d2 <- dv(Am[1], Am[2], Am[3])
      A1 <- A1 + h * d2[1]; A2 <- A2 + h * d2[2]; A3 <- A3 + h * d2[3]; t <- t + h
      while (j <= length(times) && t >= times[j] - h / 2) {
        out[j] <- A2 / v1; j <- j + 1      # Cp in mg/L, as the engine returns it
      }
    }
    out
  }
  ex <- ode(eng$Time)
  expect_lt(max(abs(eng$Cp - ex)) / max(ex), 0.005)
})
