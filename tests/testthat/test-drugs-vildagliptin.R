# vildagliptin: see R/drugs_vildagliptin.R (Landersdorfer 2012), run by the
# numerical engine with targetBinding, R/advanceMichaelisMenten.R.
noEvents <- data.frame(Time = numeric(0), Event = character(0))
nmol <- VILDAGLIPTIN_MOLECULAR_WEIGHT / 1000    # mcg per nmol

test_that("Table 1 with the switch off", {
  X <- vildagliptin(91, 166, 53, "male", adjustToFFM = FALSE)
  pk <- X$PK$default
  mm <- X$michaelisMenten
  expect_equal(c(pk$v1, pk$v2), c(22.2, 97.3))
  expect_equal(c(pk$cl1, pk$cl2) * 60, c(36.4, 40.1))
  expect_equal(mm$clLinear * 60, 36.4)
  expect_equal(c(mm$kaTransit, pk$ka_PO) * 60, c(1.26, 1.05))
  expect_equal(pk$bioavailability_PO, 0.772)
  tb <- mm$targetBinding
  expect_equal(c(tb$rmaxCentral, tb$rmaxPeripheral) / nmol, c(5, 13000))
  expect_equal(tb$kd / nmol, 71.9)
  expect_equal(c(tb$k2, tb$koff, tb$kdeg) * 60, c(23.4, 0.612, 0.110))
})

test_that("switch on: the library's fat-free-mass factors", {
  size <- pkSizeFactors(100, 170, 50, "female", TRUE, legacyVolume = 1)
  X <- vildagliptin(100, 170, 50, "female", TRUE)
  expect_equal(X$PK$default$v1, 22.2 * size$volume)
  expect_equal(X$michaelisMenten$clLinear * 60, 36.4 * size$clearance)
  expect_equal(X$michaelisMenten$targetBinding$rmaxPeripheral, 13000 * nmol * size$volume)
  expect_equal_rounded(vildagliptin(70, 170, 40, "male", TRUE)$PK$default$v1, 22.2000110)
})

test_that("clearance falls with the dose, as the paper reports", {
  PK <- getDrugPK("vildagliptin", 91, 166, 53, "male", adjustToFFM = FALSE)
  clearance <- function(mg) {
    d <- data.frame(Drug = "vildagliptin", Time = 0, Dose = mg, Units = "mg PO")
    w <- simCpCe(d, noEvents, PK, 14 * MINS_PER_DAY, FALSE)$wide
    auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
    0.772 * mg * 1000 / auc * 60        # L/h
  }
  expect_equal(clearance(100), 42.4, tolerance = 0.01)   # paper: 42.4 L/h
  expect_equal(clearance(0.001), 74, tolerance = 0.01)   # paper: about 74 L/h
})

test_that("a single 50 mg dose, and twice daily to steady state", {
  PK <- getDrugPK("vildagliptin", 91, 166, 53, "male", adjustToFFM = FALSE)
  d <- data.frame(Drug = "vildagliptin", Time = 0, Dose = 50, Units = "mg PO")
  w <- simCpCe(d, noEvents, PK, 2 * MINS_PER_DAY, FALSE)$wide
  expect_equal_rounded(max(w$Plasma), 205.513046)
  expect_equal_rounded(approx(w$Time, w$Plasma, 720)$y, 6.03329808)
  expect_true(all(is.na(w$`Effect Site`)))
  peak <- function(mg) {
    bid <- data.frame(Drug = "vildagliptin", Time = 0, Dose = mg, Units = "mg PO bid")
    w <- simCpCe(bid, noEvents, PK, 28 * MINS_PER_DAY, FALSE)$wide
    max(w$Plasma[w$Time >= 27 * MINS_PER_DAY])
  }
  # Figure 2's medians, read from its log scale: about 45 and 500 ng/mL
  expect_equal(peak(10), 45, tolerance = 0.15)
  expect_equal(peak(100), 500, tolerance = 0.1)
})

test_that("units: oral, once or twice daily", {
  defaults <- getDrugDefaults("vildagliptin")
  expect_equal(defaults$Units[[1]], c("mg PO", "mg PO qd", "mg PO bid"))
  expect_equal(defaults$Category, "Antidiabetic")
})
test_that("50 mg once daily matches the renal study's normal-function group within 20%", {
  # Scheen 2015 (Clin Pharmacokinet 54:1-21), Table 1, He et al.: 50 mg once
  # daily for 14 days, AUCtau 990 ng.h/mL, Cmax 251 ng/mL, normal function
  PK <- getDrugPK("vildagliptin", 80, 170, 55, "male", adjustToFFM = FALSE)
  d <- data.frame(Drug = "vildagliptin", Time = 0, Dose = 50, Units = "mg PO qd")
  w <- simCpCe(d, noEvents, PK, 14 * MINS_PER_DAY, FALSE)$wide
  last <- w$Time >= 13 * MINS_PER_DAY
  t <- w$Time[last]; cp <- w$Plasma[last]
  auc <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / 60
  expect_equal(auc, 990, tolerance = 0.2)
  expect_equal(max(cp), 251, tolerance = 0.2)
})
