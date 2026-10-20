# apomorphine: see R/drugs_apomorphine.R (the FDA pharmacometrics review of
# NDA 21-264, as used by Nasser 2024).

test_that("the review's estimates, switch off", {
  m <- apomorphine(70, 170, 65, "male", FALSE)
  p <- getDrugPK("apomorphine", 70, 170, 65, "male", adjustToFFM = FALSE)$PK[[1]]
  expect_equal(m$PK$default$cl1 * 60, 191)
  expect_equal(m$PK$default$v1, 153)
  expect_equal(m$PK$default$ka_SC * 60, 14.9)
  expect_equal(m$ke0 * 60, 5.36)
  # carried on the tissue depot, as every SC drug is
  expect_equal(p$ka_RA * 60, 14.9, tolerance = 1e-6)
  dd <- getDrugDefaultsGlobal()
  expect_identical(dd$Units[dd$Drug == "apomorphine"][[1]], "mg SC")
})

test_that("Nasser's typical SC peak: 11 minutes and 5.2 ng/mL per mg", {
  k <- 191 / 153; ka <- 14.9
  tmax <- log(ka / k) / (ka - k)
  expect_equal_rounded(10.9, tmax * 60, tolerance = 0.05)
  cmax <- 1000 / 153 * ka / (ka - k) * (exp(-k * tmax) - exp(-ka * tmax))
  expect_equal_rounded(5.20, cmax, tolerance = 0.005)
  res <- pdSimulate(data.frame(Drug = "apomorphine", Time = 0, Dose = 1, Units = "mg SC"),
                    maximum = 240)
  cp <- pdPeak(res)
  expect_equal(cp[["C"]], cmax, tolerance = 0.01)
  expect_lt(abs(cp[["t"]] * 60 - tmax * 60), 2)
  # dose proportional (Nasser Table 3: 21.13 at 4 mg)
  res4 <- pdSimulate(data.frame(Drug = "apomorphine", Time = 0, Dose = 4, Units = "mg SC"),
                     maximum = 240)
  expect_equal(pdPeak(res4)[["C"]], 4 * cp[["C"]], tolerance = 1e-6)
})

test_that("the motor effect peaks about 24 minutes after a dose", {
  res <- pdSimulate(data.frame(Drug = "apomorphine", Time = 0, Dose = 4, Units = "mg SC"),
                    maximum = 240)
  ce <- pdPeak(res, "Effect Site")
  expect_gt(ce[["t"]] * 60, 20); expect_lt(ce[["t"]] * 60, 30)
  # 4 mg takes the typical patient's UPDRS III from 24.3 to below half
  updrs <- APOMORPHINE_B0 * (1 - ce[["C"]]^3 / (ce[["C"]]^3 + APOMORPHINE_EC50^3))
  expect_lt(updrs, APOMORPHINE_B0 / 2)
  expect_equal_rounded(21.26, getDrugPK("apomorphine", 70, 170, 60, "male",
                                        adjustToFFM = FALSE)$tPeak, tolerance = 0.01)
})

test_that("the threshold is the EC50 and the band van Laar's MEC to MTC", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "apomorphine", ]
  expect_equal(row$endCe, APOMORPHINE_EC50)
  expect_equal(c(row$Lower, row$Upper), c(APOMORPHINE_MEC, APOMORPHINE_MTC))
  expect_identical(row$Concentration.Units, "ng")
  # size factors with the switch on, none with it off
  size <- pkSizeFactors(110, 175, 65, "male", TRUE)
  on <- apomorphine(110, 175, 65, "male", TRUE)$PK$default
  expect_equal(on$v1, 153 * size$volume)
  expect_equal(on$cl1 * 60, 191 * size$clearance)
})
