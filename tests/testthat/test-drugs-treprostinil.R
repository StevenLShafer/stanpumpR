# Treprostinil (R/drugs_treprostinil.R): the FDA pharmacometric review of NDA
# 21-272, two compartments scaled by ideal body weight.  Drafted by Claude
# Code, 2026-11-15.  Expectations from the review's Table 2 and equations (1)
# and (2), not from the drug file.

devine <- function(h, male) (if (male) 50 else 45.5) + 2.3 * (h / 2.54 - 60)

test_that("Table 2 at 70 kg ideal body weight", {
  # Devine's 70 kg man is 5 ft 8.7 in: 60 + 20/2.3 inches
  h <- (60 + 20 / 2.3) * 2.54
  set <- treprostinil(85, h, 50, "male")$PK$default
  expect_equal_rounded(set$cl1, 29.7 / 60)
  expect_equal_rounded(set$v1, 13.6)
  expect_equal_rounded(set$cl2, 25.3 / 60)
  expect_equal_rounded(set$v2, 37.1)
})

test_that("clearances by (IBW/70)^1.52, volumes by IBW/70; total weight does not enter", {
  f <- devine(160, FALSE) / 70
  set <- treprostinil(90, 160, 60, "female")$PK$default
  expect_equal_rounded(set$cl1, 29.7 * f^1.52 / 60)
  expect_equal_rounded(set$cl2, 25.3 * f^1.52 / 60)
  expect_equal_rounded(set$v1, 13.6 * f)
  expect_equal_rounded(set$v2, 37.1 * f)
  expect_equal(treprostinil(50, 160, 60, "female")$PK$default, set)
  # both switch positions give the same model
  expect_equal(treprostinil(90, 160, 60, "female", adjustToFFM = FALSE)$PK$default, set)
  # a 120 kg, 170 cm, 50 y man: ideal body weight 65.94 kg, whatever the switch
  set <- treprostinil(120, 170, 50, "male")$PK$default
  expect_equal_rounded(set$v1, 13.6 * devine(170, TRUE) / 70)
})

test_that("below 5 feet the pharmacokinetic weight stands in for Devine's", {
  set <- treprostinil(40, 140, 40, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$v1, 13.6 * 40 / 70)
  expect_true(set$cl1 > 0)
})

test_that("ng/kg/min is an intravenous rate", {
  expect_true(all(c("ng/kg/min", "ng/min") %in% infusionUnits))
  expect_true(all(isRateUnit(c("ng/kg/min", "ng/min"))))
  expect_true(all(doseRoute(c("ng/kg/min", "ng/min")) == ROUTE_IV))
})

test_that("the label's steady state (not fitted)", {
  # "an average dose of 9.3 ng/kg/min ... approximately 2,000 ng/L"; this
  # model, 70 kg and 170 cm: 9.3 x 70 ng/min over its clearance
  set <- treprostinil(70, 170, 35, "male")$PK$default
  css <- 9.3 * 70 / 1000 / set$cl1       # mcg/L = ng/mL
  expect_equal(round(css, 1), 1.4)
  expect_gt(css, 1); expect_lt(css, 2.5)
})
