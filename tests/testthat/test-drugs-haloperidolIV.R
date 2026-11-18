# haloperidolIV: see R/drugs_haloperidolIV.R (Li 2022), absolute IV values.
# The route split from oral haloperidol is pinned here too.

test_that("IV haloperidol receives Li's absolute values", {
  # Li 2022 Table 1: 80 kg, BMI 27, 67 years, 12 men and 10 women.
  k <- cohortFactorsIndependent(list(weight = 80, bmi = 27, age = 67, men = 12, women = 10))
  for (adjust in c(FALSE)) {
    actual <- haloperidolIV(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 1490, v2 = 1, v3 = 1,
        cl1 = 51.7 / 60, cl2 = 0, cl3 = 0
      )),
      tPeak = 0, ke0 = 0.213 / 60, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
  ref <- haloperidolIV(70, 170, 35, "male")$PK$default
  expect_equal_rounded(ref$v1, 1490 / k$volume)
  expect_equal_rounded(ref$cl1, 51.7 / 60 / k$clearance)
  on <- haloperidolIV(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on$v1, 1490 * 1.3049067 / k$volume)
  expect_equal_rounded(on$cl1, 51.7 / 60 * 1.2209126 / k$clearance)
  # t1/2 = 0.693 x 1490 / 51.7 = 20.0 h
  expect_equal(log(2) * 1490 / 51.7, 19.98, tolerance = 1e-3)
})

test_that("each haloperidol offers only its own route", {
  dd <- getDrugDefaultsGlobal()
  po <- dd$Units[dd$Drug == "haloperidol"][[1]]
  iv <- dd$Units[dd$Drug == "haloperidolIV"][[1]]
  expect_true(all(doseRoute(po) == ROUTE_PO))
  expect_true(all(doseRoute(iv) == ROUTE_IV))
  expect_null(haloperidolIV(70, 170, 35, "male")$PK$default$ka_PO)
})
