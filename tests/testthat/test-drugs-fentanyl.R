# Bae J et al., Br J Anaesth 2020;125:976-985, Table 2 (read in full).

test_that("the reference man gets Table 2's values in both switch positions", {
  # 70 kg, 170 cm: the pharmacokinetic weight is 70 kg, so every factor is 1
  expected <- list(v1 = 10.1, v2 = 26.5, v3 = 206,
                   cl1 = 0.704, cl2 = 2.38, cl3 = 1.49)
  for (ffm in c(TRUE, FALSE)) {
    actual <- fentanyl(70, 170, 35, "male", adjustToFFM = ffm)$PK$default
    expect_equal_rounded(actual[names(expected)], expected)
  }
})

test_that("volumes scale as (WT/70)^1.23 and clearances as (WT/70)^0.313", {
  x <- fentanyl(100, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$v1,  10.1  * (100 / 70)^1.23)
  expect_equal(x$v3,  206   * (100 / 70)^1.23)
  expect_equal(x$cl1, 0.704 * (100 / 70)^0.313)
  expect_equal(x$cl3, 1.49  * (100 / 70)^0.313)
})

test_that("the switch on evaluates the weight terms at the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y man: pharmacokinetic weight 70 x FFM / 54.48 kg
  w <- pkSizeFactors(120, 170, 50, "male")$pkWeight
  x <- fentanyl(120, 170, 50, "male")$PK$default
  expect_equal(x$v1,  10.1  * (w / 70)^1.23)
  expect_equal(x$cl1, 0.704 * (w / 70)^0.313)
  expect_lt(w, 120)
  # A woman of the same weight has a lower pharmacokinetic weight
  expect_lt(fentanyl(70, 170, 35, "female")$PK$default$v1, 10.1)
})

test_that("the effect site, MEAC and band are unchanged", {
  x <- fentanyl(70, 170, 35, "male")
  expect_equal(x$tPeak, 3.6)
  expect_equal(x$MEAC, 0.6)
  expect_equal(c(x$lowerTypical, x$typical, x$upperTypical), c(0.48, 0.72, 1.2))
  expect_match(x$reference, "Bae J et al., Br J Anaesth 2020")
})

test_that("the model stays finite at the Patient Profile's extremes", {
  for (p in list(c(0.5, 40, 0), c(300, 220, 90), c(3, 50, 0.1))) {
    for (s in c("male", "female")) for (ffm in c(TRUE, FALSE)) {
      x <- unlist(fentanyl(p[1], p[2], p[3], s, adjustToFFM = ffm)$PK$default)
      expect_true(all(is.finite(x) & x > 0))
    }
  }
})
