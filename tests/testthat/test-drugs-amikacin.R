# Amikacin (R/drugs_amikacin.R): Tod 1998's two compartments, clearance
# linear in Cockcroft-Gault creatinine clearance.  Drafted by Claude Code,
# 2026-11-12.  Expectations from the source (PMC105554), not the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Tod 1998's parameters with the switch off", {
  set <- amikacin(65, 170, 50, "male", adjustToFFM = FALSE, creatinine = 1)$PK$default
  crcl <- (140 - 50) * 65 / (72 * 1)
  expect_equal(set$cl1, (0.367 * crcl * 60 / 1000 + 1.40) / 60)
  expect_equal(set$v1, 8.92)
  expect_equal(set$cl2, 4.43 / 60)
  expect_equal(set$v2, 11.4)
})

test_that("no renal function leaves the 1.40 L/h intercept", {
  expect_equal(amikacin(65, 170, 50, "male", adjustToFFM = FALSE,
                        creatinine = 1e6)$PK$default$cl1, 1.40 / 60, tolerance = 1e-4)
})

test_that("20 mg/kg gives a peak near Tod's measured first-dose peaks", {
  # Table 2: first-dose 1-h peaks after 20 mg/kg over 30 min, mean 75.4,
  # median 66.7 (43-170) mg/L
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "amikacin", Time = c(0, 30), Dose = c(40, 0), Units = "mg/kg/hr"),
    events, 65, 170, 50, "male", 240, FALSE, adjustToFFM = FALSE, creatinine = 0.9
  )$amikacin$wide
  at1h <- approx(wide$Time, wide$Plasma, 60)$y
  expect_true(at1h > 43 && at1h < 120)
})

test_that("the MIC line and the MBC band, on total drug", {
  mic <- antibioticMic("amikacin")
  expect_equal(c(mic$MIC, mic$FreeFraction, mic$Threshold), c(4, 0.82, 4.9))
  expect_equal(unname(antibioticMbcBand("amikacin")$plotted), signif(c(2, 4, 16) / 0.82, 2))
})
