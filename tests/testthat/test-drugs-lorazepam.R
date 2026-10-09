# lorazepam: see the header of R/drugs_lorazepam.R for the model
# (Nielsen-Kudsk 1983 disposition, Greenblatt 1982 absorption, Greenblatt 2000
# effect site) and why Gonzalez 2017 and Swart 2004 are not used.  Pins were
# worked out from the published numbers by hand (Python, with the micro-
# constants and the fat-free-mass formula written out again), not from the
# code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

lorazepamRun <- function(dose, units, maximum = 1440) {
  dt <- data.frame(Drug = "lorazepam", Time = 0, Dose = dose, Units = units)
  simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", maximum,
                              FALSE)$lorazepam$wide
}

# Amount-state matrix solution: depot, central, peripheral.  ng/mL out.
twoComp <- function(t, D, p, route = c("IV", "PO", "IM")) {
  route <- match.arg(route)
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  ka <- if (route == "IV") 0 else p[[paste0("ka_", route)]]
  F  <- if (route == "IV") 1 else p[[paste0("bioavailability_", route)]]
  M <- rbind(c(-ka, 0, 0), c(ka, -(k10 + k12), k21), c(0, k12, -k21))
  x0 <- if (route == "IV") c(0, D, 0) else c(F * D, 0, 0)
  e <- eigen(M)
  coef <- solve(e$vectors, x0)
  vapply(t, function(tt) {
    Re(sum(e$vectors[2, ] * coef * exp(e$values * tt))) / p$v1 * 1000
  }, numeric(1))
}


test_that("the reference patient: micro-constants from Nielsen-Kudsk's means", {
  # V1 0.59 L/kg; CL 62.17 mL/kg/h; t1/2 alpha 0.31 h; t1/2 beta 14.10 h.
  for (adjust in c(TRUE, FALSE)) {
    actual <- lorazepam(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 41.3, v2 = 45.0007380466, v3 = 1,
        cl1 = 0.0725316667, cl2 = 0.7823654189, cl3 = 0,
        ka_PO = 0.0213276056, bioavailability_PO = 0.9, tlag_PO = 0,
        ka_IM = 0.0488131817, bioavailability_IM = 0.96, tlag_IM = 0
      )),
      tPeak = 26.3,
      MEAC = 0,
      typical = 51,
      upperTypical = 104,
      lowerTypical = 34,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
})


test_that("the derived model returns the published half-lives", {
  p <- lorazepam(70, 170, 35, "male")$PK$default
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  a <- k10 + k12 + k21
  r <- c(a + sqrt(a^2 - 4 * k10 * k21), a - sqrt(a^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / r / 60, c(0.31, 14.10), tolerance = 1e-10)
})


test_that("per-kilogram scaling with the switch off, fat-free mass with it on", {
  off <- lorazepam(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 70.8, v2 = 77.1441223657,
                            cl1 = 0.12434, cl2 = 1.3411978610))
  on <- lorazepam(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 53.8926444852, v2 = 58.7217621578,
                            cl1 = 0.0885548271, cl2 = 0.9551998122))
})


test_that("the time to peak effect gives back Greenblatt's 8.8 min equilibration", {
  pk <- getDrugPK("lorazepam", 70, 170, 35, "male")
  expect_equal(log(2) / pk$PK$default$ke0, 8.8, tolerance = 0.005)
})


test_that("each route is the two-compartment solution", {
  p <- lorazepam(70, 170, 35, "male")$PK$default
  for (r in list(c("mg", "IV"), c("mg PO", "PO"), c("mg IM", "IM"))) {
    w <- lorazepamRun(2, r[1])
    expect_equal(w$Plasma, twoComp(w$Time, 2, p, r[2]), tolerance = 1e-8,
                 info = r[1])
  }
})


test_that("the reference patient matches the label at each route", {
  p <- lorazepam(70, 170, 35, "male")$PK$default
  # 4 mg IV: about 70 ng/mL initially (label); 73.7 at 15 min
  expect_equal(twoComp(15, 4, p, "IV"), 73.69, tolerance = 1e-3)
  t <- seq(0, 300, by = 0.1)
  # 2 mg by mouth: about 20 ng/mL at about 2 h (label); 19.7 at 1.35 h
  po <- twoComp(t, 2, p, "PO")
  expect_equal(max(po), 19.72, tolerance = 1e-3)
  expect_equal(t[which.max(po)], 80.7, tolerance = 2e-3)
  # 4 mg IM: about 48 ng/mL (label); 53.4 at 38 min
  im <- twoComp(t, 4, p, "IM")
  expect_equal(max(im), 53.42, tolerance = 1e-3)
  expect_equal(t[which.max(im)], 37.7, tolerance = 2e-3)
})


test_that("lorazepam is offered IV, by mouth and IM, in Hypnotics and sedatives", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "lorazepam"], ",")[[1]]
  expect_true(all(c("mg", "mg/kg", "mg/hr", "mg PO", "mg IM") %in% units))
  expect_false(any(doseRoute(units) == ROUTE_IN))
  expect_equal(dd$Category[dd$Drug == "lorazepam"], "Hypnotics and sedatives")
})


test_that("the CSV row agrees with the drug function and Barr's C50s", {
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "lorazepam", ]
  x   <- lorazepam(70, 170, 35, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  # Barr 2001: C50 for Ramsay >= 2, 3, 4 = 34, 51, 104 ng/mL
  expect_equal(c(row$Lower, row$Typical, row$Upper), c(34, 51, 104))
  expect_equal(row$endCe, 51)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Concentration.Units, "ng")
})
