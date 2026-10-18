# exenatide: see R/drugs_exenatide.R (Cirincione 2017), run by the numerical
# engine, R/advanceMichaelisMenten.R.
noEvents <- data.frame(Time = numeric(0), Event = character(0))
# creatinine that gives an MDRD eGFR of 90 for a 50-year-old man
scr90 <- (90 / (175 * 50^-0.203))^(1 / -1.154)

test_that("Table 2 at 84.8 kg, and the clearances the paper quotes", {
  X <- exenatide(84.8, 175, 50, "male", adjustToFFM = FALSE, creatinine = scr90)
  mm <- X$michaelisMenten
  pk <- X$PK$default
  expect_equal(pk$v1, 7.03)
  expect_equal(c(pk$v2, pk$cl2 * 60), c(7.04, 3.72))
  expect_equal(mm$vmax * 60 / 1000, 1.55)       # mcg/h
  expect_equal(mm$km, 567)                      # pg/mL
  expect_equal(mm$clLinear * 60, 4.58 * (90 / 80)^0.838)
  expect_equal(round(mm$clLinear * 60, 2), 5.06)   # paper: 5.06 L/h at eGFR 90
  expect_equal(pk$cl1, mm$clLinear + mm$vmax / mm$km)
  sc <- mm$saturableSC
  expect_equal(c(sc$fraction, sc$duration / 60, sc$bioavailability), c(0.628, 1.35, 1))
  expect_equal(sc$kaMax * 60 / 1000, 12.8)      # mcg/h
  expect_equal(sc$kmKa / 1000, 16.9)            # mcg
  # total clearance at 210 pg/mL: paper 5.1, 6.3 and 7.5 L/h at eGFR 50, 75, 100
  total <- function(egfr) 4.58 * (egfr / 80)^0.838 + mm$vmax * 60 / (567 + 210)   # ng/h over ng/L
  expect_equal(round(vapply(c(50, 75, 100), total, numeric(1)), 1), c(5.1, 6.3, 7.5))
})

test_that("weight acts on Vc only, held within the studied 52.6-162 kg", {
  ref <- exenatide(84.8, 175, 50, "male", FALSE, creatinine = 1)
  heavy <- exenatide(125, 175, 50, "male", FALSE, creatinine = 1)
  expect_equal(heavy$PK$default$v1, 7.03 * (125 / 84.8)^2.67)
  expect_equal(heavy$PK$default$v2, ref$PK$default$v2)
  expect_equal(heavy$michaelisMenten$clLinear, ref$michaelisMenten$clLinear)
  expect_equal(exenatide(40, 150, 50, "male", FALSE, creatinine = 1)$PK$default$v1,
               7.03 * (52.6 / 84.8)^2.67)
  expect_equal(exenatide(200, 175, 50, "male", FALSE, creatinine = 1)$PK$default$v1,
               7.03 * (162 / 84.8)^2.67)
  # switch on: the pharmacokinetic weight
  size <- pkSizeFactors(90, 175, 50, "male", TRUE)
  expect_equal(exenatide(90, 175, 50, "male", TRUE, creatinine = 1)$PK$default$v1,
               7.03 * (size$pkWeight / 84.8)^2.67)
  expect_equal_rounded(exenatide(75, 175, 50, "male", TRUE)$PK$default$v1, 4.99947)
})

test_that("renal function: MDRD eGFR from the creatinine, assumed normal when blank", {
  lo <- exenatide(80, 175, 60, "female", FALSE, creatinine = 2)$michaelisMenten
  expect_equal(lo$clLinear * 60, 4.58 * (egfrMDRD(60, "female", 2) / 80)^0.838)
  blank <- exenatide(80, 175, 60, "female", FALSE)$michaelisMenten
  assumed <- adultEquivalentCreatinine(NULL, 60, "female")
  expect_equal(blank$clLinear * 60, 4.58 * (egfrMDRD(60, "female", assumed) / 80)^0.838)
  expect_true("creatinine" %in% names(formals(exenatide)))
})

test_that("10 mcg in a 75 kg man at eGFR 90", {
  PK <- getDrugPK("exenatide", 75, 175, 50, "male", adjustToFFM = FALSE, creatinine = scr90)
  expect_equal(PK$tissueRoute, ROUTE_SC)
  d <- data.frame(Drug = "exenatide", Time = 0, Dose = 10, Units = "mcg SC")
  w <- simCpCe(d, noEvents, PK, 1440, FALSE)$wide
  i <- which.max(w$Plasma)
  # peak at the end of the zero-order phase; paper (typical 75 kg, with
  # between-subject variability): 287 pg/mL, observed tmax about 2 h
  expect_equal(w$Time[i], 81)
  expect_equal_rounded(w$Plasma[i], 263.277694)
  expect_equal_rounded(approx(w$Time, w$Plasma, 240)$y, 164.366866)
  expect_true(all(is.na(w$`Effect Site`)))
  # heavier patients peak lower: paper -18% at 100 kg, -34% at 125 kg
  peak <- function(wt) {
    PK <- getDrugPK("exenatide", wt, 175, 50, "male", adjustToFFM = FALSE, creatinine = scr90)
    max(simCpCe(d, noEvents, PK, 1440, FALSE)$wide$Plasma)
  }
  expect_equal(peak(100) / peak(75), 0.78, tolerance = 0.01)
  expect_equal(peak(125) / peak(75), 0.62, tolerance = 0.01)
})

test_that("twice daily: the doses share the depot and barely accumulate", {
  PK <- getDrugPK("exenatide", 75, 175, 50, "male", adjustToFFM = FALSE, creatinine = scr90)
  d <- data.frame(Drug = "exenatide", Time = 0, Dose = 10, Units = "mcg SC bid")
  w <- simCpCe(d, noEvents, PK, 3 * MINS_PER_DAY, FALSE)$wide
  first <- max(w$Plasma[w$Time < 720])
  last <- max(w$Plasma[w$Time > 2 * MINS_PER_DAY])
  expect_gt(last / first, 1)
  expect_lt(last / first, 1.06)
})

test_that("units: SC only, pg/mL", {
  defaults <- getDrugDefaults("exenatide")
  expect_equal(defaults$Units[[1]], c("mcg SC", "mcg SC bid"))
  expect_equal(defaults$Concentration.Units, "pg")
  expect_equal(defaults$Category, "Antidiabetic")
})
