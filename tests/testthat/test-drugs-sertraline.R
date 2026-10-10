# sertraline: see the header of R/drugs_sertraline.R for the model (Alhadab
# and Brundage 2020) and its four reductions.  Pins were worked out from the
# published numbers and the fat-free-mass factors (volumes x 1.3049066461,
# clearances x 1.2209126185 for the 120 kg man), not from the code under
# test; the absorption fit is re-derived here with optim().

noEvents <- data.frame(Time = numeric(0), Event = character(0))

sertralineDose <- function(time, dose, units = "mg PO") {
  data.frame(Drug = "sertraline", Time = time, Dose = dose, Units = units)
}

oralSertraline <- function(dt, maximum = 1440, adjustToFFM = FALSE) {
  simulateDrugsWithCovariates(dt, noEvents, 70, 171, 50, "male", maximum,
                              FALSE, adjustToFFM = adjustToFFM)$sertraline$wide
}

# Alhadab's single-dose F(D), D in mg
alhadabF <- function(D) 0.639 * D / (15.5 + D)


test_that("returns Alhadab's parameters with the fat-free-mass switch off", {
  actual <- sertraline(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 1200, v2 = 1350, v3 = 1,
      cl1 = 59.7 / 60, cl2 = 161 / 60, cl3 = 0,
      ka_PO = 0.4087317 / 60,
      bioavailability_PO = 0.639,
      tlag_PO = 1.4324891 * 60
    )),
    tPeak = 0,
    tPeakRoute = ROUTE_PO,
    MEAC = 0,
    typical = 50,
    upperTypical = 150,
    lowerTypical = 10,
    reference = actual$reference,
    oralSaturation = list(form = "rising", D50 = 15.5,
                          exampleDoses = c(25, 50, 100, 200))
  )
  expect_equal_rounded(actual, expected)
  # Size-free with the switch off
  expect_equal(sertraline(120, 170, 50, "male", adjustToFFM = FALSE)$PK,
               actual$PK)
})


test_that("scales to fat-free mass for a 120 kg man", {
  actual <- sertraline(120, 170, 50, "male")
  expected <- list(
    v1 = 1565.88797533,       # 1200 x 1.3049066461
    v2 = 1761.62397224,       # 1350 x 1.3049066461
    v3 = 1,
    cl1 = 1.21480805541,      # 59.7 / 60 x 1.2209126185
    cl2 = 3.27611552631,      # 161 / 60 x 1.2209126185
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})


test_that("the oralSaturation block is valid and reproduces F(D)", {
  x <- sertraline(70, 171, 50, "male")
  expect_identical(validateOralSaturation(x$oralSaturation, "sertraline"),
                   x$oralSaturation)
  doses <- c(5, 25, 50, 100, 200, 400)
  f <- x$PK$default$bioavailability_PO *
    oralSaturationFraction(doses, x$oralSaturation)
  expect_equal(f, alhadabF(doses))
  expect_equal(round(f, 3), c(0.156, 0.394, 0.488, 0.553, 0.593, 0.615))
})


test_that("the first-order absorption is the least-squares fit to ka(t)", {
  # Cumulative fraction absorbed under Alhadab's ka(t), 0-48 h on a 0.1 h
  # grid, fitted by 1 - exp(-ka (t - tlag)), zero before the lag.
  kat <- function(t) 0.855 * t^1.37 / (3.7^1.37 + t^1.37)
  t <- seq(0, 48, by = 0.1)
  cum <- vapply(t, function(x)
    if (x == 0) 0 else stats::integrate(kat, 0, x, rel.tol = 1e-12)$value,
    numeric(1))
  fa <- 1 - exp(-cum)
  pred <- function(p) ifelse(t > p[2], 1 - exp(-p[1] * (t - p[2])), 0)
  obj  <- function(p) sum((fa - pred(p))^2)
  fit  <- stats::optim(c(0.4, 1.4), obj, method = "Nelder-Mead",
                       control = list(reltol = 1e-14, maxit = 5000))
  fit  <- stats::optim(fit$par, obj, method = "BFGS",
                       control = list(reltol = 1e-16))
  expect_equal(fit$par[1], 0.4087317, tolerance = 1e-6)
  expect_equal(fit$par[2], 1.4324891, tolerance = 1e-6)

  d <- sertraline(70, 171, 50, "male")$PK$default
  expect_equal(d$ka_PO * 60, fit$par[1], tolerance = 1e-6)
  expect_equal(d$tlag_PO / 60, fit$par[2], tolerance = 1e-6)

  # Quality of the fit, in fraction absorbed
  err <- fa - pred(fit$par)
  expect_equal(sqrt(mean(err^2)), 0.01566, tolerance = 1e-3)
  expect_equal(max(abs(err)), 0.1083, tolerance = 1e-3)
  expect_equal(t[which.max(abs(err))], 1.4)
})


test_that("the simulation is the two-compartment oral solution with F(D)", {
  # Independent closed form: 100 mg at 0 and 50 mg at 12 h, each scaled by
  # its own F, after the lag, into V1 with first-order absorption.
  w <- oralSertraline(rbind(sertralineDose(0, 100), sertralineDose(720, 50)),
                      maximum = 2880)
  V1 <- 1200; Vp <- 1350; CL <- 59.7 / 60; Q <- 161 / 60
  ka <- 0.4087317 / 60; lag <- 1.4324891 * 60
  k10 <- CL / V1; k12 <- Q / V1; k21 <- Q / Vp
  a <- k10 + k12 + k21
  lam <- c(a + sqrt(a^2 - 4 * k10 * k21), a - sqrt(a^2 - 4 * k10 * k21)) / 2
  A <- c((k21 - lam[1]) / (lam[2] - lam[1]), (k21 - lam[2]) / (lam[1] - lam[2]))
  oral <- function(t, t0, D) {
    s <- pmax(t - t0 - lag, 0)
    1000 * ka * alhadabF(D) * D / V1 *
      (A[1] * (exp(-lam[1] * s) - exp(-ka * s)) / (ka - lam[1]) +
       A[2] * (exp(-lam[2] * s) - exp(-ka * s)) / (ka - lam[2]))
  }
  expect_equal(w$Plasma, oral(w$Time, 0, 100) + oral(w$Time, 720, 50),
               tolerance = 1e-8)
  expect_true(all(is.na(w$`Effect Site`)))
})


test_that("a 100 mg dose peaks at 5.5 h; terminal half-life 33.0 h", {
  # Literature single-dose Tmax 4.5-8.4 h.  Alhadab's ka(t) itself, solved
  # numerically, peaks at 25.9 ng/mL at 5.9 h; the first-order fit at 25.2
  # ng/mL at 5.5 h.
  w <- oralSertraline(sertralineDose(0, 100))
  expect_equal(max(w$Plasma), 25.18, tolerance = 1e-3)
  tmax <- w$Time[which.max(w$Plasma)] / 60
  expect_gt(tmax, 4.5)
  expect_lt(tmax, 8.4)

  # Terminal half-life from the eigenvalues: 33.0 h with the multiple-dose
  # V2 of 1350 L, 26.6 h with the single-dose 928 L (header, reduction 1).
  halfLife <- function(Vp) {
    k10 <- 59.7 / 1200; k12 <- 161 / 1200; k21 <- 161 / Vp
    a <- k10 + k12 + k21
    log(2) / ((a - sqrt(a^2 - 4 * k10 * k21)) / 2)
  }
  expect_equal(halfLife(1350), 32.962, tolerance = 1e-4)
  expect_equal(halfLife(928), 26.611, tolerance = 1e-4)
  pk <- getDrugPK("sertraline", 70, 171, 50, "male", adjustToFFM = FALSE)
  expect_equal(pk$PK$default$ke0, 0)                # plasma only
})


test_that("the multiple-dose V2 changes single-dose profiles only a little", {
  # Header, reduction 1: a single 100 mg dose with V2 1350 against 928 L.
  sim <- function(Vp, tH) {
    V1 <- 1200; CL <- 59.7; Q <- 161; ka <- 0.4087317; lag <- 1.4324891
    k10 <- CL / V1; k12 <- Q / V1; k21 <- Q / Vp
    a <- k10 + k12 + k21
    lam <- c(a + sqrt(a^2 - 4 * k10 * k21), a - sqrt(a^2 - 4 * k10 * k21)) / 2
    A <- c((k21 - lam[1]) / (lam[2] - lam[1]), (k21 - lam[2]) / (lam[1] - lam[2]))
    s <- pmax(tH - lag, 0)
    1000 * ka * alhadabF(100) * 100 / V1 *
      (A[1] * (exp(-lam[1] * s) - exp(-ka * s)) / (ka - lam[1]) +
       A[2] * (exp(-lam[2] * s) - exp(-ka * s)) / (ka - lam[2]))
  }
  tt <- seq(0, 24, by = 0.01)
  expect_equal(max(sim(1350, tt)) / max(sim(928, tt)), 0.9785, tolerance = 1e-3)
  expect_equal(sim(1350, 24) / sim(928, 24), 0.8686, tolerance = 1e-3)
  expect_equal(sim(1350, 72) / sim(928, 72), 1.0957, tolerance = 1e-3)
})


test_that("sertraline is offered orally only", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "sertraline"], ",")[[1]]
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "sertraline"]))
  expect_true(is.na(dd$Infusion.Units[dd$Drug == "sertraline"]))
})


test_that("the CSV row agrees with the drug function", {
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "sertraline", ]
  x   <- sertraline(70, 171, 50, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(row$Concentration.Units, "ng")
})
