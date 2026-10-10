# haloperidolIV: see R/drugs_haloperidolIV.R (Li 2022), absolute IV values.
# The route split from oral haloperidol is pinned here too.

test_that("IV haloperidol receives Li's absolute values", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- haloperidolIV(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 1490, v2 = 1, v3 = 1,
        cl1 = 51.7 / 60, cl2 = 0, cl3 = 0
      )),
      tPeak = 0, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
  on <- haloperidolIV(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on$v1, 1490 * 1.3049067)
  expect_equal_rounded(on$cl1, 51.7 / 60 * 1.2209126)
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
