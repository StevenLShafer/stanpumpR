# haloperidol (oral, Pilla Reddy 2013) and haloperidolIV (Li 2022): two drugs,
# because apparent oral parameters cannot be turned into absolute IV ones.

test_that("oral haloperidol receives the published apparent values", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- haloperidol(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 669, v2 = 2500, v3 = 1,
        cl1 = 88 / 60, cl2 = 233 / 60, cl3 = 0,
        ka_PO = 0.236 / 60, bioavailability_PO = 1, tlag_PO = 0
      )),
      tPeak = 0, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
  on <- haloperidol(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on$v2, 2500 * 1.3049067)
  expect_equal_rounded(on$cl1, 88 / 60 * 1.2209126)
})

test_that("oral haloperidol's half-lives are 1.3 and 31 h", {
  x <- haloperidol(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, 0, x$cl2 / x$v2, 0)
  hl <- sort(log(2) / (r[r > 0] * 60))
  expect_equal(hl, c(1.26, 31.1), tolerance = 0.01)
})
