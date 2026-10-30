# levodopa: see R/drugs_levodopa.R (Othman & Dutta 2014, Table 2 and the FDA
# review of NDA 203952; the effect site from Mao 2013, Table 1).

test_that("Table 2 at 70 kg, switch off", {
  p <- levodopa(70, 170, 64, "male", FALSE)$PK$default
  expect_equal(p$cl1 * 60, 24.8)
  expect_equal(p$v1, 58.5)
  expect_equal(p$cl2 * 60, 6.8)
  expect_equal(p$v2, 72.9)
  expect_equal(p$ka_PO * 60, 2.4)
  expect_true(p$transit_PO)
  expect_equal(levodopa(70, 170, 64, "male", FALSE)$ke0 * 60, 1.80)
})

test_that("weight on the central volume only, exponent 1", {
  heavy <- levodopa(105, 170, 64, "male", FALSE)$PK$default
  expect_equal(heavy$v1, 58.5 * 105 / 70)
  expect_equal(heavy$cl1 * 60, 24.8)
  expect_equal(heavy$v2, 72.9)
  # switch on: the pharmacokinetic weight on Vc, the library factors on the rest
  size <- pkSizeFactors(105, 170, 64, "male", TRUE)
  on <- levodopa(105, 170, 64, "male", TRUE)$PK$default
  expect_equal(on$v1, 58.5 * size$pkWeight / 70)
  expect_equal(on$cl1 * 60, 24.8 * size$clearance)
  expect_equal(on$v2, 72.9 * size$volume)
})

test_that("the plasma peaks at about 80 minutes and the motor effect about 40 later", {
  res <- pdSimulate(data.frame(Drug = "levodopa", Time = 0, Dose = 200, Units = "mg PO"),
                    maximum = 480, age = 64)
  cp <- pdPeak(res); ce <- pdPeak(res, "Effect Site")
  expect_gt(cp[["t"]] * 60, 70); expect_lt(cp[["t"]] * 60, 90)
  expect_gt(ce[["t"]] * 60 - cp[["t"]] * 60, 25)
  expect_lt(ce[["t"]] * 60 - cp[["t"]] * 60, 50)
  # a 200 mg tablet's peak sits inside the AGNP band (1.0-3.5 mcg/mL, Hart 2026)
  expect_gt(cp[["C"]], 1.0); expect_lt(cp[["C"]], 3.5)
  # the time to peak effect after a bolus is solved from ke0
  expect_equal_rounded(57.69885, getDrugPK("levodopa", 70, 170, 64, "male", adjustToFFM = FALSE)$tPeak,
                       tolerance = 1e-5)
})

test_that("the threshold and band are Mao's EC50 and the AGNP update 2026 range", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "levodopa", ]
  expect_equal(row$endCe, round(LEVODOPA_UPDRS_EC50, 2))
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(1, 3.5, 2.25))
  expect_identical(row$Concentration.Units, "mcg")
})
