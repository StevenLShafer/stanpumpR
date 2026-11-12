# Ganciclovir (R/drugs_ganciclovir.R): Caldés 2009's two compartments, with
# oral valganciclovir dosed as itself.  Drafted by Claude Code, 2026-11-12.
# Expectations from Table 3 of the source and the Cytovene and Valcyte
# labels, not from the drug file.

events <- data.frame(Time = double(), Event = character())
auc <- function(w, a, b) {
  s <- w[w$Time >= a & w$Time <= b, ]
  sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
}

test_that("Table 3's parameters, the clearance at the population's mean CLCR", {
  # a 70 kg, 55-year-old man whose Cockcroft-Gault clearance is 57 mL/min
  scr <- (140 - 55) * 70 / (72 * 57)
  p <- ganciclovir(70, 170, 55, "male", adjustToFFM = FALSE,
                   creatinine = scr)$PK$default
  expect_equal(p$cl1 * 60, 7.49)
  expect_equal(c(p$v1, p$v2, p$cl2 * 60), c(31.90, 32.0, 10.20))
  expect_equal(p$ka_PO * 60, 0.895)
  expect_equal(p$tlag_PO / 60, 0.382)
  expect_equal(p$bioavailability_PO, 0.594)
  # clearance is proportional to creatinine clearance
  p2 <- ganciclovir(70, 170, 55, "male", adjustToFFM = FALSE,
                    creatinine = 2 * scr)$PK$default
  expect_equal(p2$cl1 / p$cl1, 0.5)
})

test_that("valganciclovir doses are converted to ganciclovir at 0.720", {
  expect_true(all(valganciclovirUnits %in% allUnits))
  expect_true(all(paste(valganciclovirUnits, c("qd", "bid")) %in% scheduledUnits))
  expect_equal(doseRoute(valganciclovirUnits), ROUTE_PO)
  pk <- ganciclovir(70, 170, 35, "male")
  expect_equal(doseProductFactor(c("mg PO valganciclovir", "mg"), pk$doseProducts),
               c(0.720, 1))
  # the same mass of ganciclovir given orally either way gives the same curve
  # when the conversion is undone
  run <- function(dose, units) simulateDrugsWithCovariates(
    data.frame(Drug = "ganciclovir", Time = 0, Dose = dose, Units = units),
    events, 70, 170, 35, "male", 1440, FALSE)$ganciclovir$wide
  a <- run(900, "mg PO valganciclovir"); b <- run(900 * 0.720, "mg PO")
  expect_equal(a$Plasma, b$Plasma)
})

test_that("the labels' exposures", {
  # Cytovene: clearance 3.52 mL/min/kg (14.8 L/h at 70 kg); 5 mg/kg over 1 h,
  # AUC 22.1-26.8 mg.h/L
  p <- ganciclovir(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(p$cl1 * 60, 3.52 * 70 * 60 / 1000, tolerance = 0.02)
  d <- data.frame(Drug = "ganciclovir", Time = c(0, 60), Dose = c(5, 0),
                  Units = "mg/kg/hr")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 3 * 1440,
                                   FALSE)$ganciclovir$wide
  expect_gt(auc(w, 0, 3 * 1440), 22.1); expect_lt(auc(w, 0, 3 * 1440), 26.8)
  # Valcyte Table 13: 900 mg once, creatinine clearance 51-70 mL/min, AUC 49.5
  scr <- (140 - 35) * 70 / (72 * 60)
  d <- data.frame(Drug = "ganciclovir", Time = 0, Dose = 900,
                  Units = "mg PO valganciclovir")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 7 * 1440,
                                   FALSE, adjustToFFM = FALSE,
                                   creatinine = scr)$ganciclovir$wide
  expect_equal(auc(w, 0, 7 * 1440), 49.5, tolerance = 0.03)
})

test_that("pregnancy raises the creatinine clearance 1.5-fold when creatinine is blank", {
  a <- ganciclovir(70, 165, 30, "female")$PK$default$cl1
  b <- ganciclovir(70, 165, 30, "female", pregnant = TRUE)$PK$default$cl1
  expect_equal(b / a, 1.5)
  c1 <- ganciclovir(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default$cl1
  c0 <- ganciclovir(70, 165, 30, "female", creatinine = 0.6)$PK$default$cl1
  expect_equal(c1, c0)
})
