# levodopaSimon: see R/drugs_levodopaSimon.R (Simon 2016, Table 2 and Table 3,
# model 2).

test_that("Table 2 and the effect site at 70 kg, switch off", {
  m <- levodopaSimon(70, 170, 65, "male", FALSE)
  p <- m$PK$default
  expect_equal(p$ka_PO * 60, 1.86)
  expect_equal(p$cl1 * 60, 36.6)
  expect_equal(p$v1, 42.9)
  expect_equal(p$cl2, 0)
  expect_equal(p$tlag_PO, 0)
  expect_equal(m$ke0 * 60, 1.37)
})

test_that("allometric weight: exponent 0.75 on CL/F, 1 on V/F", {
  p <- levodopaSimon(63, 170, 65, "male", FALSE)$PK$default
  expect_equal(p$cl1 * 60, 36.6 * (63 / 70)^0.75)
  expect_equal(p$v1, 42.9 * 63 / 70)
  # switch on: the pharmacokinetic weight
  w <- pkSizeFactors(110, 170, 65, "male", TRUE)$pkWeight
  on <- levodopaSimon(110, 170, 65, "male", TRUE)$PK$default
  expect_equal(on$cl1 * 60, 36.6 * (w / 70)^0.75)
  expect_equal(on$v1, 42.9 * w / 70)
})

test_that("the plasma peak at 46 minutes, the effect site about an hour later", {
  # the study's mean dose at its median weight
  res <- pdSimulate(data.frame(Drug = "levodopaSimon", Time = 0, Dose = 380, Units = "mg PO"),
                    maximum = 300, weight = 63, age = 65)
  cp <- pdPeak(res); ce <- pdPeak(res, "Effect Site")
  # the one-compartment peak, log(ka/k)/(ka - k) = 0.765 h whatever the dose,
  # found to within the plot's time grid
  k <- 36.6 * (63 / 70)^0.75 / (42.9 * 63 / 70)
  expect_equal_rounded(0.765, log(1.86 / k) / (1.86 - k), tolerance = 0.005)
  expect_lt(abs(cp[["t"]] - log(1.86 / k) / (1.86 - k)), 0.1)
  expect_equal_rounded(5.02, cp[["C"]], tolerance = 0.01)
  expect_gt(ce[["t"]] - cp[["t"]], 0.4)
  expect_lt(ce[["t"]] - cp[["t"]], 0.8)
  # the time to peak effect after a bolus, solved from ke0
  expect_equal_rounded(54.98, getDrugPK("levodopaSimon", 70, 170, 60, "male",
                                        adjustToFFM = FALSE)$tPeak, tolerance = 0.01)
})

test_that("the threshold is the UPDRS EC50, the band the AGNP range", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "levodopaSimon", ]
  expect_equal(row$endCe, round(LEVODOPA_SIMON_UPDRS_EC50, 2))
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(1, 3.5, 2.25))
  expect_identical(row$Concentration.Units, "mcg")
  # the dyskinesia concentration is 4.45 times the motor one, as the paper says
  expect_equal(LEVODOPA_SIMON_DYSK_EC50 / LEVODOPA_SIMON_UPDRS_EC50, 4.45, tolerance = 0.001)
})

test_that("Simon's patients need more levodopa than Mao's for the same effect", {
  # a 200 mg tablet: the effect site stays above Simon's threshold for a
  # shorter time than above Mao's
  dt <- function(d) data.frame(Drug = d, Time = 0, Dose = 200, Units = "mg PO")
  above <- function(res, thr) {
    s <- pdSite(res, "Effect Site")
    diff(range(s$Time[s$Y >= thr]))
  }
  simon <- above(pdSimulate(dt("levodopaSimon"), maximum = 480, weight = 75, height = 175, age = 68),
                 LEVODOPA_SIMON_UPDRS_EC50)
  mao <- above(pdSimulate(dt("levodopa"), maximum = 480, weight = 75, height = 175, age = 68),
               LEVODOPA_UPDRS_EC50)
  expect_lt(simon, mao / 2)
})
