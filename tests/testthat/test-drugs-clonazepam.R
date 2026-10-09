# clonazepam: see the header of R/drugs_clonazepam.R for the model (dos Santos
# 2009, tablets), why it replaced the specification's Kruizinga 2022, and why
# it is oral only with no effect site.  Pins were worked out from the
# published numbers by hand (Python, with the fat-free-mass formula written
# out again), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Two-compartment oral solution after a lag, from the amount-state matrix,
# independent of the engine's exponential coefficients.  Minutes, ng/mL out.
twoCompOral <- function(t, D, p) {
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  M <- rbind(c(-p$ka_PO, 0, 0),
             c(p$ka_PO, -(k10 + k12), k21),
             c(0, k12, -k21))
  e <- eigen(M)
  coef <- solve(e$vectors, c(D * p$bioavailability_PO, 0, 0))
  vapply(t, function(tt) {
    s <- tt - p$tlag_PO
    if (s <= 0) return(0)
    Re(sum(e$vectors[2, ] * coef * exp(e$values * s))) / p$v1 * 1000
  }, numeric(1))
}


test_that("the reference patient receives dos Santos's Table 1 values", {
  # Vc 141 L; k10 0.0207, k12 0.0725, k21 0.294 /h; ka 2.21 /h; lag 0.369 h.
  # CL/F = k10 Vc, Q/F = k12 Vc, V2/F = k12 Vc / k21.
  for (adjust in c(TRUE, FALSE)) {
    actual <- clonazepam(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 141, v2 = 34.7704081633, v3 = 1,
        cl1 = 0.048645, cl2 = 0.170375, cl3 = 0,
        ka_PO = 0.0368333333,
        bioavailability_PO = 1,
        tlag_PO = 22.14
      )),
      tPeak = 0,
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = 40,
      upperTypical = 70,
      lowerTypical = 20,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
})


test_that("size scales to fat-free mass with the switch on, not at all with it off", {
  # 120 kg, 170 cm, 50 y man: volumes x 1.3049066, clearances x 1.2209126
  on <- clonazepam(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 183.9918371001, v2 = 45.3721366999,
                            cl1 = 0.0593912943, cl2 = 0.2080129874))
  off <- clonazepam(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 141)
  expect_equal(off$cl1, 0.048645)
})


test_that("the simulation is the two-compartment oral solution, plasma only", {
  dt <- data.frame(Drug = "clonazepam", Time = c(0, 720), Dose = c(2, 1),
                   Units = "mg PO")
  w <- simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", 4320,
                                   FALSE)$clonazepam$wide
  p <- clonazepam(70, 170, 35, "male")$PK$default
  expected <- twoCompOral(w$Time, 2, p) + twoCompOral(w$Time - 720, 1, p)
  expect_equal(w$Plasma, expected, tolerance = 1e-8)
  expect_true(all(is.na(w$"Effect Site")))
})


test_that("2 mg tablets: peak, timing, exposure and half-life against the published data", {
  # Reference man, 2 mg: peak 12.51 ng/mL at 1.98 h; AUC 0-72 h 475
  # ng.h/mL; terminal half-life 42.2 h.  Observed after 2 mg tablets: peak
  # 13-17 ng/mL at 1-4 h; half-life 30-43 h (see the header).
  p <- clonazepam(70, 170, 35, "male")$PK$default
  t <- seq(0, 300, by = 0.1)
  cp <- twoCompOral(t, 2, p)
  expect_equal(max(cp), 12.51, tolerance = 1e-3)
  expect_equal(t[which.max(cp)] / 60, 1.98, tolerance = 5e-3)
  tt <- seq(0, 72 * 60, by = 1)
  auc <- sum(diff(tt) * utils::head(twoCompOral(tt, 2, p), -1)) / 60
  expect_equal(auc, 475, tolerance = 0.01)
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  a <- k10 + k12 + k21
  beta <- (a - sqrt(a^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / beta / 60, 42.23, tolerance = 1e-3)
  # 1 mg/day averages 14.3 ng/mL at steady state
  expect_equal(1000 / (p$cl1 * 1440), 14.276, tolerance = 1e-4)
})


test_that("clonazepam is offered by mouth only, in Hypnotics and sedatives", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "clonazepam"], ",")[[1]]
  expect_equal(units, c("mg PO", paste("mg PO", names(SCHEDULE_INTERVALS))))
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "clonazepam"]))
  expect_equal(dd$Category[dd$Drug == "clonazepam"], "Hypnotics and sedatives")
})


test_that("the CSV row agrees with the drug function", {
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "clonazepam", ]
  x   <- clonazepam(70, 170, 35, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(row$Concentration.Units, "ng")
})
