# temazepam: see the header of R/drugs_temazepam.R for how the two
# compartments were fitted jointly to van Steveninck 1994's and Halliday
# 1987's intravenous means, and
# where the oral route and the band come from.  Pins were worked out by hand
# (Python, with the fat-free-mass formula and the matrix exponential written
# out again), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Amount-state matrix solution: depot, central, peripheral; time in minutes,
# rate in mg/min for an infusion.  ng/mL out.
twoComp <- function(t, p, D = 0, route = c("PO", "infusion"), Tinf = 0) {
  route <- match.arg(route)
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  if (route == "PO") {
    M <- rbind(c(-p$ka_PO, 0, 0), c(p$ka_PO, -(k10 + k12), k21), c(0, k12, -k21))
    e <- eigen(M)
    coef <- solve(e$vectors, c(D * p$bioavailability_PO, 0, 0))
    return(vapply(t, function(tt) if (tt <= 0) 0 else
      Re(sum(e$vectors[2, ] * coef * exp(e$values * tt))) / p$v1 * 1000, numeric(1)))
  }
  M <- rbind(c(-(k10 + k12), k21), c(k12, -k21))
  e <- eigen(M)
  ex <- function(tt) e$vectors %*% diag(exp(e$values * tt)) %*% solve(e$vectors)
  R <- c(D / Tinf, 0)
  xT <- solve(M, (ex(Tinf) - diag(2)) %*% R)
  vapply(t, function(tt) {
    x <- if (tt <= Tinf) solve(M, (ex(tt) - diag(2)) %*% R) else ex(tt - Tinf) %*% xT
    x[1] / p$v1 * 1000
  }, numeric(1))
}


test_that("the reference patient receives the fitted per-kilogram values", {
  # V1 0.2784, V2 0.5231 L/kg; CL 0.0661, Q 0.1115 L/h/kg; x 70 kg
  for (adjust in c(TRUE, FALSE)) {
    actual <- temazepam(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 19.488, v2 = 36.617, v3 = 1,
        cl1 = 0.0771166667, cl2 = 0.1300833333, cl3 = 0,
        ka_PO = 0.0304011921,
        bioavailability_PO = 0.92,
        tlag_PO = 0
      )),
      tPeak = 0,
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = 400,
      upperTypical = 600,
      lowerTypical = 250,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
})


test_that("per-kilogram scaling with the switch off, fat-free mass with it on", {
  off <- temazepam(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 33.408, v2 = 62.772, cl1 = 0.1322, cl2 = 0.223))
  on <- temazepam(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 25.4300207192, v2 = 47.7817666602,
                            cl1 = 0.0941527114, cl2 = 0.1588203831))
})


test_that("the joint fit against the two intravenous studies", {
  # van Steveninck (Table I): 25.85 mg over 28.5 min in the mean 66.5 kg
  # subject (switch off, so the per-kilogram values scale exactly).  Means
  # Cmax 996, AUC 0-3 1.4, AUC 0-8 2.8, AUC 0-inf 6.15 ug.h/mL, half-life
  # 10.55 h; the fit gives 1208, 1.976, 3.164, 5.881 and 10.78.
  p <- temazepam(66.5, 170, 21, "female", adjustToFFM = FALSE)$PK$default
  t <- seq(0, 480, by = 0.25)
  cp <- twoComp(t, p, 25.85, "infusion", 28.5)
  auc <- function(upto) { i <- t <= upto; sum(diff(t[i]) * (utils::head(cp[i], -1) + utils::tail(cp[i], -1)) / 2) / 60 }
  expect_equal(max(cp), 1208.3, tolerance = 2e-3)
  expect_equal(auc(180), 1975.7, tolerance = 2e-3)
  expect_equal(auc(480), 3163.5, tolerance = 2e-3)
  expect_equal(25.85 / (p$cl1 * 60) * 1000, 5880.8, tolerance = 1e-4)
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  a <- k10 + k12 + k21
  r <- c(a + sqrt(a^2 - 4 * k10 * k21), a - sqrt(a^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / r / 60, c(0.8810, 10.7757), tolerance = 1e-4)
  # Halliday (Figure 1, read by eye): 20 mg over 20 s at 68 kg
  h <- temazepam(68, 170, 22, "male", adjustToFFM = FALSE)$PK$default
  got <- twoComp(c(5, 10, 15, 30, 60, 90, 120), h, 20, "infusion", 1 / 3)
  expect_equal(got, c(1003.8, 952.7, 904.8, 778.0, 586.9, 455.9, 365.7),
               tolerance = 1e-3)
})


test_that("the simulation is the two-compartment oral solution, plasma only", {
  dt <- data.frame(Drug = "temazepam", Time = c(0, 1440), Dose = c(20, 20),
                   Units = "mg PO")
  w <- simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", 2880,
                                   FALSE)$temazepam$wide
  p <- temazepam(70, 170, 35, "male")$PK$default
  expected <- twoComp(w$Time, p, 20) + twoComp(w$Time - 1440, p, 20)
  expect_equal(w$Plasma, expected, tolerance = 1e-8)
  expect_true(all(is.na(w$"Effect Site")))
})


test_that("oral doses against the published single-dose and steady-state data", {
  p <- temazepam(70, 170, 35, "male")$PK$default
  t <- seq(0, 240, by = 0.1)
  c20 <- twoComp(t, p, 20)
  # 20 mg peaks at 545.5 ng/mL at 0.92 h (observed 362-708)
  expect_equal(max(c20), 545.46, tolerance = 1e-3)
  expect_equal(t[which.max(c20)] / 60, 0.9245, tolerance = 5e-3)
  # 30 mg nightly, day 7: 217.2 ng/mL at 9 h, 82.1 at 24 h (label 260, 75)
  ss <- function(tt) sum(twoComp(tt - 1440 * (0:6), p, 30))
  expect_equal(ss(6 * 1440 + 540), 217.20, tolerance = 1e-3)
  expect_equal(ss(7 * 1440), 82.13, tolerance = 1e-3)
})


test_that("time until 250 ng/mL is read against plasma", {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("temazepam", 70, 170, 35, "male", dd[dd$Drug == "temazepam", ])
  PK$endCe <- 250
  DT <- data.frame(Drug = "temazepam", Time = 0, Dose = 20, Units = "mg PO")
  w <- simCpCe(DT, noEvents, PK, 720, TRUE)$wide
  # 20 mg falls below 250 ng/mL 3.40 h after the dose
  at <- which(w$Time >= 90)[1]
  expect_equal((w$Time[at] + w$Recovery[at]) / 60, 3.3965, tolerance = 0.01)
})


test_that("temazepam is offered by mouth only, in Hypnotics and sedatives", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "temazepam"], ",")[[1]]
  expect_equal(units, c("mg PO", "mg PO qd"))
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "temazepam"]))
  expect_equal(dd$Category[dd$Drug == "temazepam"], "Hypnotics and sedatives")
})


test_that("the CSV row agrees with the drug function", {
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "temazepam", ]
  x   <- temazepam(70, 170, 35, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  expect_equal(row$endCe, 250)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Concentration.Units, "ng")
})
