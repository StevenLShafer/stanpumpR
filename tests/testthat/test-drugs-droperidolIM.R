# droperidolIM: see R/drugs_droperidolIM.R (Foo 2016), apparent IM values.
# The route split from IV droperidol is pinned here too.

test_that("IM droperidol receives Foo's apparent values", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- droperidolIM(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 73.6, v2 = 79.8, v3 = 1,
        cl1 = 41.9 / 60, cl2 = 71.5 / 60, cl3 = 0,
        ka_IM = 10 / 60, bioavailability_IM = 1, tlag_IM = 0
      )),
      tPeak = 0, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
  on <- droperidolIM(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on$v1, 73.6 * 1.3049067)
  expect_equal_rounded(on$cl1, 41.9 / 60 * 1.2209126)
})

test_that("IM droperidol's half-lives match Foo's medians of 0.32 and 3.0 h", {
  x <- droperidolIM(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, 0, x$cl2 / x$v2, 0)
  hl <- sort(log(2) / (r[r > 0] * 60))
  expect_equal(hl, c(0.31, 3.0), tolerance = 0.03)
})

test_that("each droperidol offers only its own route", {
  dd <- getDrugDefaultsGlobal()
  iv <- dd$Units[dd$Drug == "droperidol"][[1]]
  im <- dd$Units[dd$Drug == "droperidolIM"][[1]]
  expect_true(all(doseRoute(iv) == ROUTE_IV))
  expect_true(all(doseRoute(im) == ROUTE_IM))
})
