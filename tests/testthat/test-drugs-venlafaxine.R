# Venlafaxine and desvenlafaxine (ODV), Wang 2022: see R/drugs_venlafaxine.R.
#
# The reference below is Figure 1 of the source written out as rate equations
# (depot, venlafaxine, ODV) in moles and solved by eigendecomposition, sharing
# no code with the package's closed form.  (Claude Code, 2026-10-10, at the
# request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))
MW_VEN <- 277.40; MW_ODV <- 263.38

# Concentrations (ng/mL) at `at` minutes after oral doses (mg) at `times` (min)
wangConcentrations <- function(times, doses, at, healthy = FALSE) {
  h <- function(x) x / 60
  ka <- h(0.63); fp <- 0.048
  cl <- h(80.9 * (if (healthy) 1 else 1 - 0.617)); v <- 628
  clm <- h(22.1); vm <- 238
  K <- matrix(0, 3, 3)
  K[1, 1] <- -ka
  K[2, 1] <- ka * (1 - fp); K[2, 2] <- -cl / v           # K23 is the only exit
  K[3, 1] <- ka * fp;       K[3, 2] <- cl / v; K[3, 3] <- -clm / vm
  e <- eigen(K)
  x <- c(0, 0, 0)
  for (i in seq_along(times)) if (times[i] < at) {
    E <- Re(e$vectors %*% diag(exp(e$values * (at - times[i]))) %*% solve(e$vectors))
    x <- x + as.vector(E %*% c(doses[i] / MW_VEN, 0, 0))     # mmol
  }
  c(parent = x[2] * MW_VEN / v, metabolite = x[3] * MW_ODV / vm) * 1000
}

plasmaAt <- function(out, drug, t) {
  r <- out[[drug]]$results
  r <- r[r$Site == "Plasma", ]
  stats::approx(r$Time, r$Y, t)$y
}


test_that("returns the published values at the reference patient", {
  actual <- venlafaxine(70, 170, 35, "male", adjustToFFM = FALSE)
  cl <- 80.9 * (1 - 0.617) / 60
  expected <- list(
    PK = list(default = list(
      v1 = 628, v2 = 1, v3 = 1, cl1 = cl, cl2 = 0, cl3 = 0,
      ka_PO = 0.63 / 60, bioavailability_PO = 1 - 0.048, tlag_PO = 0
    )),
    tPeak = 0, MEAC = 0, typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference,
    prodrug = FALSE,
    metabolite = list(name = "desvenlafaxine", kFormation = cl / 628,
                      firstPassFraction = 0.048, mwRatio = 263.38 / 277.40)
  )
  expect_equal_rounded(actual, expected)

  odv <- desvenlafaxine(70, 170, 35, "male", adjustToFFM = FALSE)
  expect_equal(odv$PK$default[c("v1", "cl1", "cl2")],
               list(v1 = 238, cl1 = 22.1 / 60, cl2 = 0))
  expect_identical(odv$reference, actual$reference)
})

test_that("K23 = CL/V reproduces the source's own healthy half-life", {
  # Wang 2022 Discussion: "The estimated half-life (5.4 h) ... of VEN in
  # healthy subjects".  With venlafaxine's only exit being conversion to ODV,
  # the half-life is ln 2 x V/CL.
  expect_equal(log(2) * 628 / 80.9, 5.4, tolerance = 0.01)
  # Patients: 14.0 h; ODV 7.5 h
  p <- venlafaxine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / (p$cl1 / p$v1) / 60, 14.04, tolerance = 0.005)
  m <- desvenlafaxine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / (m$cl1 / m$v1) / 60, 7.46, tolerance = 0.005)
})

test_that("the pair scales to fat-free mass identically for a 120 kg man", {
  # volumes x 1.3049067, clearances x 1.2209126
  p <- venlafaxine(120, 170, 50, "male")
  expect_equal(p$PK$default$v1, 628 * 1.3049067, tolerance = 1e-6)
  expect_equal(p$PK$default$cl1, 80.9 * 0.383 / 60 * 1.2209126, tolerance = 1e-6)
  expect_equal(p$metabolite$kFormation, p$PK$default$cl1 / p$PK$default$v1)
  m <- desvenlafaxine(120, 170, 50, "male")
  expect_equal(m$PK$default$v1, 238 * 1.3049067, tolerance = 1e-6)
  expect_equal(m$PK$default$cl1, 22.1 / 60 * 1.2209126, tolerance = 1e-6)
})

test_that("the engine reproduces Figure 1, single dose and repeated", {
  single <- simulateDrugsWithCovariates(
    data.frame(Drug = "venlafaxine", Time = 0, Dose = 75, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 2880, FALSE, adjustToFFM = FALSE)
  expect_true(all(c("venlafaxine", "desvenlafaxine") %in% names(single)))
  for (t in c(60, 180, 720, 1440, 2880 - 1e-6)) {
    ref <- wangConcentrations(0, 75, t)
    # 2e-3: the engine's output grid is interpolated linearly on the rising limb
    expect_equal(plasmaAt(single, "venlafaxine", t), unname(ref["parent"]),
                 tolerance = 2e-3, info = t)
    expect_equal(plasmaAt(single, "desvenlafaxine", t), unname(ref["metabolite"]),
                 tolerance = 2e-3, info = t)
  }

  bid <- simulateDrugsWithCovariates(
    data.frame(Drug = "venlafaxine", Time = 0, Dose = 75, Units = "mg PO bid"),
    noEvents, 70, 170, 35, "male", 10080, FALSE, adjustToFFM = FALSE)
  t <- 10080 - 1e-6
  ref <- wangConcentrations(seq(0, 10080 - 720, by = 720), rep(75, 14), t)
  expect_equal(plasmaAt(bid, "venlafaxine", t), unname(ref["parent"]), tolerance = 1e-4)
  expect_equal(plasmaAt(bid, "desvenlafaxine", t), unname(ref["metabolite"]), tolerance = 1e-4)
})

test_that("steady-state exposure follows from the clearances", {
  # Average over a dosing interval at steady state, 150 mg/day:
  #   venlafaxine  (1 - FP) x D / CL
  #   ODV          D x MWratio / CLM: every mole of venlafaxine becomes ODV
  D <- 150 / 1440                                   # mg/min
  cl <- 80.9 * 0.383 / 60; clm <- 22.1 / 60
  ven <- (1 - 0.048) * D / cl * 1000
  odv <- D * 263.38 / 277.40 / clm * 1000
  expect_equal(ven, 192.0, tolerance = 0.005)
  expect_equal(odv, 268.5, tolerance = 0.005)
  # ODV : venlafaxine 1.4 in patients (3.7 in healthy volunteers), and the
  # sum, 460 ng/mL, sits above the 100-400 ng/mL AGNP range at 150 mg/day,
  # as the patients' low venlafaxine clearance implies
  expect_equal(odv / ven, 1.40, tolerance = 0.01)
  expect_equal(odv + ven, 460.5, tolerance = 0.005)
})

test_that("venlafaxine is oral only and desvenlafaxine cannot be dosed", {
  dd <- getDrugDefaultsGlobal()
  expect_true(all(doseRoute(unlist(dd$Units[dd$Drug == "venlafaxine"])) == ROUTE_PO))
  expect_equal(dd$Category[dd$Drug == "venlafaxine"], "Antidepressants")
  expect_true(all(is.na(unlist(dd$Units[dd$Drug == "desvenlafaxine"]))))
  expect_true(is.na(dd$Category[dd$Drug == "desvenlafaxine"]))
})
