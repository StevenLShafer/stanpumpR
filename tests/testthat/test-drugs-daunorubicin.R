# Daunorubicin (R/drugs_daunorubicin.R): Bogason 2011.  Drafted by Claude
# Code, 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())

test_that("Table 2's basic model: half-lives 0.60 and 11.5 h, Vss 1493 L", {
  set <- getDrugPK("daunorubicin", 70, 170, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$cl1 * 60, 115)
  expect_equal_rounded(set$v1, 373)
  expect_equal_rounded(set$cl2 * 60, 252)
  expect_equal_rounded(set$v2, 1120)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2) / 60)
  expect_equal(round(halfLives, 2), c(0.60, 11.52), tolerance = 0.01)
})

test_that("108 mg over 1 h: about 358, 66 and 20 nmol/L at 1, 5 and 24 h", {
  d <- data.frame(Drug = "daunorubicin", Time = c(0, 60), Dose = c(108, 0), Units = "mg/hr")
  w <- simulateDrugsWithCovariates(d, noEvents, 70, 170, 61, "male", 25 * 60, FALSE,
                                   adjustToFFM = FALSE)$daunorubicin$wide
  at <- function(h) w$Plasma[which.min(abs(w$Time - h * 60))] / 527.52 * 1000   # nmol/L
  expect_equal(c(at(1), at(5), at(24)), c(358, 66, 20), tolerance = 0.05)
})

test_that("daunorubicin is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "daunorubicin", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("daunorubicin", 70, 170, 60, "male")
  expect_match(PK$reference, "Bogason")
  expect_identical(therapeuticRange("daunorubicin")$band_status, "not_applicable")
})
