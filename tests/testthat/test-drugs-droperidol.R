# droperidol (IV, Cooper 2018) and droperidolIM (Foo 2016): separate drugs,
# because the IM parameters are apparent and the IV ones absolute.

test_that("IV droperidol receives Cooper's absolute values", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- droperidol(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 3.16, v2 = 16.9, v3 = 1,
        cl1 = 15.3 / 60, cl2 = 9.66 / 60, cl3 = 0
      )),
      tPeak = 0, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
  on <- droperidol(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on$v2, 16.9 * 1.3049067)
  expect_equal_rounded(on$cl2, 9.66 / 60 * 1.2209126)
})

test_that("IV droperidol's terminal half-life is Cooper's 2.0 h", {
  x <- droperidol(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, 0, x$cl2 / x$v2, 0)
  expect_equal(max(log(2) / (r[r > 0] * 60)), 2.0, tolerance = 0.02)
})

test_that("IV droperidol keeps its early-time warning", {
  expect_equal(DROPERIDOL_UNOBSERVED_MIN, 25)
  expect_equal(antipsychoticProfile("droperidol")$unobservedMinutes, 25)
  expect_match(droperidol(70, 170, 35, "male")$reference, "first 25 min")
})
