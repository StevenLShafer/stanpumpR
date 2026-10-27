# linagliptin: see R/drugs_linagliptin.R (Retlich 2015), run by the numerical
# engine with rapidBinding, R/advanceMichaelisMenten.R.
noEvents <- data.frame(Time = numeric(0), Event = character(0))
nM <- LINAGLIPTIN_MOLECULAR_WEIGHT / 1000      # ng/mL per nmol/L

test_that("Table 4 for a 60-year-old man at 88 kg", {
  X <- linagliptin(88, 175, 60, "male", adjustToFFM = FALSE)
  pk <- X$PK$default
  mm <- X$michaelisMenten
  expect_equal(c(pk$v1, pk$v2), c(715, 1650))
  expect_equal(c(pk$cl1, pk$cl2) * 60, c(258, 412))
  expect_equal(mm$clLinear * 60, 258)
  expect_equal(mm$vmax, 0)
  expect_equal(pk$ka_PO * 60, 0.441)
  expect_equal(pk$bioavailability_PO, 1)
  rb <- mm$rapidBinding
  expect_equal(rb$bmaxCentral / nM, 4.97)
  expect_equal(rb$amaxPeripheral / nM, 1650)
  expect_equal(rb$kd / nM, 0.0652)
})

test_that("sex and age on Bmax,C, weight on F, each held to the studied range", {
  bmax <- function(...) linagliptin(...)$michaelisMenten$rapidBinding$bmaxCentral / nM
  expect_equal(bmax(88, 165, 60, "female", FALSE), 4.97 + 0.457)
  expect_equal(bmax(88, 175, 70, "male", FALSE), 4.97 * (1 + 0.0561))
  expect_equal(bmax(88, 175, 90, "male", FALSE), 4.97 * (1 + 0.00561 * 18))
  F <- function(w, adj = FALSE) linagliptin(w, 175, 60, "male", adj)$PK$default$bioavailability_PO
  expect_equal(F(110), 1 - 0.00958 * 22)
  expect_equal(F(200), 1 - 0.00958 * (132 - 88))
  expect_equal(F(40), 1 - 0.00958 * (57 - 88))
  # switch on: the pharmacokinetic weight
  size <- pkSizeFactors(110, 175, 60, "male", TRUE)
  expect_equal(F(110, TRUE), 1 - 0.00958 * (size$pkWeight - 88))
  expect_equal_rounded(F(88, TRUE), 1.0637743)
  # nothing else changes with weight
  expect_equal(linagliptin(130, 175, 60, "male", FALSE)$PK$default$v1, 715)
})

test_that("5 mg once daily reaches the paper's steady state", {
  PK <- getDrugPK("linagliptin", 88, 175, 60, "male", adjustToFFM = FALSE)
  d <- data.frame(Drug = "linagliptin", Time = 0, Dose = 5, Units = "mg PO qd")
  w <- simCpCe(d, noEvents, PK, 21 * MINS_PER_DAY, FALSE)$wide
  last <- w$Time >= 20 * MINS_PER_DAY
  peak <- max(w$Plasma[last]) / nM
  trough <- approx(w$Time, w$Plasma, 21 * MINS_PER_DAY - 1)$y / nM
  # paper: median 24 h after a regular steady-state dose 5.58 nmol/L (5.29 is
  # the trough after a missed dose); predictive-check median
  # peak about 9 nmol/L
  expect_equal(trough, 5.0, tolerance = 0.02)
  expect_equal(peak, 9.0, tolerance = 0.02)
  expect_equal_rounded(trough * nM, 2.3674141)
  expect_equal_rounded(peak * nM, 4.2697186)
  expect_true(all(is.na(w$`Effect Site`)))
  # less than dose proportional: the bound share falls as the dose rises
  ten <- simCpCe(transform(d, Dose = 10), noEvents, PK, 21 * MINS_PER_DAY, FALSE)$wide
  ratio <- approx(ten$Time, ten$Plasma, 21 * MINS_PER_DAY - 1)$y / (trough * nM)
  expect_gt(ratio, 1)
  expect_lt(ratio, 2)
})

test_that("units: oral, daily", {
  defaults <- getDrugDefaults("linagliptin")
  expect_equal(defaults$Units[[1]], c("mg PO", "mg PO qd"))
  expect_equal(defaults$Category, "Antidiabetic")
})
