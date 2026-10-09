# diazepam: see the header of R/drugs_diazepam.R for the model (Hung 1996
# three-compartment kinetics, Buhrer 1990 effect site), why the
# specification's paediatric McCann model is not used, and how the oral and
# IM absorption rates were set.  Pins were worked out by hand (Python, with
# the fat-free-mass formula and the matrix exponential written out again),
# not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Amount-state matrix solution: depot, central, two peripherals.  Time in
# minutes; an infusion of D mg over Tinf min if route = "infusion".
threeComp <- function(t, p, D, route = c("IV", "PO", "IM", "infusion"), Tinf = 0) {
  route <- match.arg(route)
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k13 <- p$cl3 / p$v1
  k21 <- p$cl2 / p$v2; k31 <- p$cl3 / p$v3
  M <- rbind(c(-(k10 + k12 + k13), k21, k31), c(k12, -k21, 0), c(k13, 0, -k31))
  ex <- function(A, tt) { e <- eigen(A); Re(e$vectors %*% diag(exp(e$values * tt)) %*% solve(e$vectors)) }
  if (route == "infusion") {
    R <- c(D / Tinf, 0, 0)
    xT <- solve(M, (ex(M, Tinf) - diag(3)) %*% R)
    return(vapply(t, function(tt) {
      x <- if (tt <= Tinf) solve(M, (ex(M, tt) - diag(3)) %*% R) else ex(M, tt - Tinf) %*% xT
      x[1] / p$v1 * 1000
    }, numeric(1)))
  }
  if (route == "IV") {
    return(vapply(t, function(tt) (ex(M, tt) %*% c(D, 0, 0))[1] / p$v1 * 1000, numeric(1)))
  }
  ka <- p[[paste0("ka_", route)]]; F <- p[[paste0("bioavailability_", route)]]
  A <- rbind(c(-ka, 0, 0, 0), cbind(c(ka, 0, 0), M))
  vapply(t, function(tt) (ex(A, tt) %*% c(F * D, 0, 0, 0))[2] / p$v1 * 1000, numeric(1))
}


test_that("the reference patient receives Hung's Table I means", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- diazepam(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 3.43, v2 = 8.47, v3 = 87.51,
        cl1 = 0.027, cl2 = 1.103, cl3 = 0.335,
        ka_PO = 0.0346573590, bioavailability_PO = 0.94, tlag_PO = 0,
        ka_IM = 0.0138629436, bioavailability_IM = 1, tlag_IM = 0
      )),
      tPeak = 2.63,
      MEAC = 0,
      typical = 300,
      upperTypical = 600,
      lowerTypical = 150,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
})


test_that("size scales to fat-free mass with the switch on, not at all with it off", {
  on <- diazepam(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "v2", "v3", "cl1", "cl2", "cl3")],
                       list(v1 = 4.4758297961, v2 = 11.0525592925, v3 = 114.1923806002,
                            cl1 = 0.0329646407, cl2 = 1.3466666182, cl3 = 0.4090057272))
  off <- diazepam(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v3, 87.51)
  expect_equal(off$cl1, 0.027)
})


test_that("half-lives, and the time to peak effect gives back Buhrer's 1.6 min", {
  p <- diazepam(70, 170, 35, "male")$PK$default
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k13 <- p$cl3 / p$v1
  M <- rbind(c(-(k10 + k12 + k13), p$cl2 / p$v2, p$cl3 / p$v3),
             c(k12, -p$cl2 / p$v2, 0), c(k13, 0, -p$cl3 / p$v3))
  hl <- sort(log(2) / -eigen(M)$values)
  expect_equal(hl, c(1.30281, 24.00900, 2713.143), tolerance = 1e-5)
  pk <- getDrugPK("diazepam", 70, 170, 35, "male")
  expect_equal(log(2) / pk$PK$default$ke0, 1.6, tolerance = 0.005)
})


test_that("each route is the three-compartment solution", {
  p <- diazepam(70, 170, 35, "male")$PK$default
  for (r in list(c("mg", "IV"), c("mg PO", "PO"), c("mg IM", "IM"))) {
    dt <- data.frame(Drug = "diazepam", Time = 0, Dose = 10, Units = r[1])
    w <- simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", 1440,
                                     FALSE)$diazepam$wide
    expect_equal(w$Plasma, threeComp(w$Time, p, 10, r[2]), tolerance = 1e-8,
                 info = r[1])
  }
})


test_that("Mould 1995's early concentrations, against the published model", {
  # 0.1 and 0.2 mg/kg over 90 s in Mould's 80 kg men (8 and 16 mg), the
  # published parameters (Hung's subjects averaged 80.5 kg): 1030 and 2060
  # ng/mL at 3 min against 1120 and 2390 observed, AUC 0-3 h 30.1 and 60.2
  # against 33.9 and 66.6 ug.min/mL.
  p <- diazepam(80, 171, 33, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(threeComp(3, p, 8, "infusion", 1.5), 1030.25, tolerance = 1e-4)
  expect_equal(threeComp(3, p, 16, "infusion", 1.5), 2060.49, tolerance = 1e-4)
  t <- seq(0, 180, by = 0.05)
  cp <- threeComp(t, p, 8, "infusion", 1.5)
  auc <- sum(diff(t) * (utils::head(cp, -1) + utils::tail(cp, -1)) / 2) / 1000
  expect_equal(auc, 30.11, tolerance = 2e-3)
})


test_that("10 mg by mouth and IM match the observed mean peaks", {
  p <- diazepam(70, 170, 35, "male")$PK$default
  t <- seq(0, 150, by = 0.05)
  po <- threeComp(t, p, 10, "PO")
  expect_equal(max(po), 302.00, tolerance = 1e-3)     # Hogan 2020: 286-338
  expect_equal(t[which.max(po)], 27.41, tolerance = 2e-3)
  im <- threeComp(t, p, 10, "IM")
  expect_equal(max(im), 200.54, tolerance = 1e-3)     # Hung 1996: 199
  expect_equal(t[which.max(im)], 52.54, tolerance = 2e-3)
})


test_that("diazepam is offered IV, by mouth and IM, in Hypnotics and sedatives", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "diazepam"], ",")[[1]]
  expect_true(all(c("mg", "mg/kg", "mg/hr", "mg PO", "mg IM") %in% units))
  expect_false(any(doseRoute(units) == ROUTE_IN))
  expect_equal(dd$Category[dd$Drug == "diazepam"], "Hypnotics and sedatives")
})


test_that("the CSV row agrees with the drug function", {
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "diazepam", ]
  x   <- diazepam(70, 170, 35, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  expect_equal(row$endCe, 150)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Concentration.Units, "ng")
})
