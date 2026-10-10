# Fluoxetine and norfluoxetine (Han 2025): see R/drugs_fluoxetine.R.
#
# The point of these tests is the one thing the model is good for: it must
# reproduce the steady-state troughs the source itself simulated (Table S4 of
# its Supplementary Materials).  The reference below is the closed-form
# superposition of one-compartment oral doses and a one-compartment
# metabolite, written out here from the published values and sharing no code
# with the package.  (Claude Code, 2026-10-10, at the request of
# Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Fluoxetine and norfluoxetine troughs (ng/mL), n daily doses of D mg, read
# just before the next dose.  Published units: L, L/h, h.
hanTrough <- function(D, male, n = 60) {
  ka <- 0.3; V <- 24.9; CL <- 2.91 * (if (male) 1.165 else 1)
  Vm <- 1.52; CLm <- 3.24
  k <- CL / V; km <- CLm / Vm
  tt <- 24 * (1:n)
  A <- D * ka / (V * (ka - k))
  e <- function(a, b, t) (exp(-a * t) - exp(-b * t)) / (b - a)
  parent <- sum(A * (exp(-k * tt) - exp(-ka * tt))) * 1000
  metab  <- sum(A * k * V / Vm * (e(k, km, tt) - e(ka, km, tt))) * 1000
  c(parent = parent, metabolite = metab)
}

simFluoxetine <- function(D, sex, days = 14) {
  simulateDrugsWithCovariates(
    data.frame(Drug = "fluoxetine", Time = 0, Dose = D, Units = "mg PO qd"),
    noEvents, 70, 170, 35, sex, days * 1440, FALSE, adjustToFFM = FALSE)
}

plasmaAt <- function(out, drug, t) {
  r <- out[[drug]]$results
  r <- r[r$Site == "Plasma", ]
  stats::approx(r$Time, r$Y, t)$y
}


test_that("returns the published values at the reference patient", {
  actual <- fluoxetine(70, 170, 35, "female", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 24.9, v2 = 1, v3 = 1,
      cl1 = 2.91 / 60, cl2 = 0, cl3 = 0,
      ka_PO = 0.3 / 60, bioavailability_PO = 1, tlag_PO = 0
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference,
    prodrug = FALSE,
    metabolite = list(name = "norfluoxetine", kFormation = 2.91 / 60 / 24.9,
                      firstPassFraction = 0, mwRatio = 1)
  )
  expect_equal_rounded(actual, expected)
  expect_equal(fluoxetine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default$cl1,
               2.91 * 1.165 / 60)

  met <- norfluoxetine(70, 170, 35, "female", adjustToFFM = FALSE)
  expect_equal(met$PK$default[c("v1", "cl1", "cl2")],
               list(v1 = 1.52, cl1 = 3.24 / 60, cl2 = 0))
  # no sex effect on the metabolite
  expect_equal(norfluoxetine(70, 170, 35, "male", adjustToFFM = FALSE)$PK,
               met$PK)
  expect_identical(met$reference, actual$reference)
})

test_that("the pair scales to fat-free mass identically for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  p <- fluoxetine(120, 170, 50, "male")
  expect_equal(p$PK$default$v1, 24.9 * 1.3049067, tolerance = 1e-6)
  expect_equal(p$PK$default$cl1, 2.91 * 1.165 / 60 * 1.2209126, tolerance = 1e-6)
  expect_equal(p$metabolite$kFormation, p$PK$default$cl1 / p$PK$default$v1)
  m <- norfluoxetine(120, 170, 50, "male")
  expect_equal(m$PK$default$v1, 1.52 * 1.3049067, tolerance = 1e-6)
  expect_equal(m$PK$default$cl1, 3.24 / 60 * 1.2209126, tolerance = 1e-6)
})

test_that("the printed parameters reproduce the source's simulated troughs (Table S4)", {
  # Han 2025 Table S4: median steady-state troughs of 1000 virtual patients,
  # 20 mg qd.  Matching them to within 3% is what shows the printed units
  # (L, L/h) are the ones the authors used.
  tableS4 <- list(female = c(parent = 85.97, metabolite = 78.90),
                  male   = c(parent = 58.81, metabolite = 63.70))
  for (sex in names(tableS4)) {
    ours <- hanTrough(20, sex == "male")
    expect_equal(unname(ours), unname(tableS4[[sex]]), tolerance = 0.03, info = sex)
  }
  # and Table S4 is exactly proportional to dose, as a linear model is
  expect_equal(unname(hanTrough(40, FALSE) / hanTrough(20, FALSE)), c(2, 2))
})

test_that("the engine gives the same troughs, with norfluoxetine on its own row", {
  for (sex in c("female", "male")) {
    out <- simFluoxetine(20, sex)
    expect_true(all(c("fluoxetine", "norfluoxetine") %in% names(out)))
    ref <- hanTrough(20, sex == "male")
    # just before the dose on day 14
    t <- 14 * 1440 - 1e-6
    expect_equal(plasmaAt(out, "fluoxetine", t), unname(ref["parent"]),
                 tolerance = 1e-3, info = sex)
    expect_equal(plasmaAt(out, "norfluoxetine", t), unname(ref["metabolite"]),
                 tolerance = 1e-3, info = sex)
  }
})

test_that("the time course is NOT physiological, and the tests say so", {
  # Half-lives of hours and minutes, against days on the label.  If these
  # ever change, the warnings in the drug file and help page must change too.
  p <- fluoxetine(70, 170, 35, "female", adjustToFFM = FALSE)$PK$default
  m <- norfluoxetine(70, 170, 35, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / (p$cl1 / p$v1) / 60, 5.93, tolerance = 0.01)
  expect_equal(log(2) / (m$cl1 / m$v1) / 60, 0.325, tolerance = 0.01)
  # Steady state is reached on the second day ...
  expect_gt(hanTrough(20, FALSE, n = 2)["parent"] / hanTrough(20, FALSE)["parent"], 0.99)
  # ... and the peak within the interval is several times the trough.
  out <- simFluoxetine(20, "female", days = 7)
  r <- out$fluoxetine$results
  r <- r[r$Site == "Plasma" & r$Time > 6 * 1440, ]
  expect_gt(max(r$Y) / min(r$Y), 3)
})

test_that("fluoxetine is oral only and norfluoxetine cannot be dosed", {
  dd <- getDrugDefaultsGlobal()
  expect_true(all(doseRoute(unlist(dd$Units[dd$Drug == "fluoxetine"])) == ROUTE_PO))
  expect_equal(dd$Category[dd$Drug == "fluoxetine"], "Antidepressants")
  expect_true(all(is.na(unlist(dd$Units[dd$Drug == "norfluoxetine"]))))
  expect_true(is.na(dd$Category[dd$Drug == "norfluoxetine"]))
  # no band on either: the AGNP range is for the sum
  expect_true(all(unlist(dd[dd$Drug %in% c("fluoxetine", "norfluoxetine"),
                            c("Lower", "Upper", "Typical")]) == 0))
})
