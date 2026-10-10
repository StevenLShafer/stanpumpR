# escitalopram: see the header of R/drugs_escitalopram.R.
#
# Every expectation is worked out from Liu et al. 2022's published numbers
# (CL/F 16.3 L/h, V/F 815 L, ka 0.6 /h; CYP2C19 x 0.847 intermediate,
# x 0.479 poor) and the fat-free-mass factors in docs/adding-a-drug.md, not by
# running the code under test.  The steady state is checked against the
# one-compartment oral closed form written out below.
#
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters at the reference patient", {
  actual <- escitalopram(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = 815, v2 = 1, v3 = 1,
      cl1 = 16.3 / 60, cl2 = 0, cl3 = 0,
      ka_PO = 0.6 / 60,
      bioavailability_PO = 1,
      tlag_PO = 0
    )),
    tPeak = 0,
    MEAC = 0,
    typical = 40,
    upperTypical = 80,
    lowerTypical = 15,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  expect_match(actual$reference, "Liu", fixed = TRUE)
  expect_match(actual$reference, "10.3389/fphar.2022.964758", fixed = TRUE)
  expect_match(actual$reference, "Hiemke", fixed = TRUE)
})

test_that("the switch off ignores weight, height, age and sex", {
  # No size covariate and the age effect is gated, so with the switch off the
  # published values hold for everyone.
  a <- escitalopram(45, 150, 85, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(a$v1, 815)
  expect_equal_rounded(a$cl1, 16.3 / 60)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  actual <- escitalopram(120, 170, 50, "male")
  expected <- list(v1 = 1063.49896, cl1 = 0.331681256)
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
  # placeholders unscaled
  expect_equal(actual$PK$default$v2, 1)
  expect_equal(actual$PK$default$cl2, 0)
})

test_that("CYP2C19 phenotype scales clearance only", {
  cl <- function(ph) escitalopram(70, 171, 50, "male", cyp2c19 = ph, adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(cl("poor")$cl1,         16.3 / 60 * 0.479)   # 0.130128333
  expect_equal_rounded(cl("intermediate")$cl1, 16.3 / 60 * 0.847)   # 0.230101667
  expect_equal_rounded(cl("normal")$cl1,       16.3 / 60)
  # Rapid and ultrarapid were not estimated: given the normal (EM) value
  expect_equal_rounded(cl("rapid")$cl1,        16.3 / 60)
  expect_equal_rounded(cl("ultrarapid")$cl1,   16.3 / 60)
  for (ph in CYP2C19_VALUES) expect_equal_rounded(cl(ph)$v1, 815)
  # The default is normal
  expect_equal(escitalopram(70, 171, 50, "male"), escitalopram(70, 171, 50, "male", cyp2c19 = "normal"))
  # and getDrugPK passes the phenotype through
  pm <- getDrugPK("escitalopram", 70, 171, 50, "male", getDrugDefaults("escitalopram"),
                  cyp2c19 = "poor", adjustToFFM = FALSE)
  expect_equal_rounded(pm$PK$default$cl1, 16.3 / 60 * 0.479)
})

test_that("an invalid cyp2c19 fails loudly", {
  expect_error(escitalopram(70, 171, 50, "male", cyp2c19 = "extensive"), "Invalid cyp2c19")
  expect_error(escitalopram(70, 171, 50, "male", cyp2c19 = c("poor", "normal")), "Invalid cyp2c19")
  expect_error(escitalopram(70, 171, 50, "male", cyp2c19 = NA), "Invalid cyp2c19")
})

test_that("escitalopram is offered orally only, because the parameters are apparent", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "escitalopram", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_false(any(grepl("min|hr", units)))
  expect_equal(escitalopram(70, 171, 50, "male")$PK$default$bioavailability_PO, 1)
  # The band mirrors the CSV (AGNP 2018); no MEAC, no threshold
  X <- escitalopram(70, 171, 50, "male")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC),
               c(X$lowerTypical, X$upperTypical, X$typical, X$MEAC))
  expect_equal(X$tPeak, 0)
})

test_that("half-life and steady state agree with the published parameters", {
  p <- escitalopram(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # ln 2 x 815 / 16.3 = 34.66 h
  expect_equal(log(2) * p$v1 / p$cl1 / 60, 34.657359, tolerance = 1e-7)

  # 10 mg once daily: Css,avg = 10 mg / 24 h / 16.3 L/h = 25.562 ng/mL.
  # One-compartment oral closed form at steady state, t after a dose:
  D <- 10; tau <- 1440; V <- 815; k <- 16.3 / 60 / 815; ka <- 0.01
  css <- function(t) D * ka / (V * (ka - k)) *
    (exp(-k * t) / (1 - exp(-k * tau)) - exp(-ka * t) / (1 - exp(-ka * tau))) * 1000
  avg <- stats::integrate(css, 0, tau, rel.tol = 1e-10)$value / tau
  expect_equal(avg, 10 / 24 / 16.3 * 1000, tolerance = 1e-7)
  expect_equal(avg, 25.562372, tolerance = 1e-7)

  # The engine, after four weeks (19 half-lives) of 10 mg qd, at the end of
  # the run: a trough, just before the next dose.
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "escitalopram", Time = 0, Dose = 10, Units = "mg PO qd"),
    noEvents, 70, 171, 50, "male", 28 * 1440, FALSE, adjustToFFM = FALSE)
  w <- o$escitalopram$wide
  expect_equal(utils::tail(w$Time, 1), 28 * 1440)
  expect_equal(utils::tail(w$Plasma, 1), css(tau), tolerance = 1e-5)
  # The trough and peak bracket the average
  expect_lt(css(tau), avg)
  expect_gt(max(w$Plasma[w$Time > 27 * 1440]), avg)
})
