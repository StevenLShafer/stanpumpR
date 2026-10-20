# pimavanserin: see R/drugs_pimavanserin.R (FDA Pharmacometrics Review of NDA
# 207318, Table 6).

test_that("Table 6 at the median patient, per mg of tartrate", {
  p <- pimavanserin(70, 168, 71, "male", FALSE)$PK$default
  b <- 34 / 40
  expect_equal(p$cl1 * MINS_PER_DAY / b, 587.093)
  expect_equal(p$v1 / b, 2021.41)
  expect_equal(p$ka_PO * MINS_PER_DAY, 7.4322)
  expect_equal(p$tlag_PO / MINS_PER_DAY, 0.0707)
})

test_that("height and age factors", {
  cl <- function(h, a) pimavanserin(70, h, a, "male", FALSE)$PK$default$cl1
  expect_equal(cl(180, 71) / cl(168, 71), 1 + 0.00851 * 12)
  expect_equal(cl(168, 81) / cl(168, 71), 1 - 0.007558 * 10)
})

test_that("34 mg of base a day averages the sponsor's 68 ng/mL at steady state", {
  res <- pdSimulate(pdRegimen("pimavanserin", "mg PO", 34, 1440, 30), maximum = 30 * 1440,
                    height = 168, age = 71)
  expect_equal(pdAUC(res, 29 * 1440, 30 * 1440) / 24, 40 / 587.093 * 1000, tolerance = 0.005)
  expect_equal(pdTerminalHalfLife("pimavanserin", 70, 168, 71, "male", adjustToFFM = FALSE),
               57.3, tolerance = 0.005)
})

test_that("the single-dose peak is late (13.5 h), and 81% of it is reached by 6 h", {
  s <- pdSite(pdSimulate(pdRegimen("pimavanserin", "mg PO", 34, 1440, 1), maximum = 1440,
                         height = 168, age = 71))
  expect_equal(s$Time[which.max(s$Y)] / 60, 13.5, tolerance = 0.05)
  at6 <- stats::approx(s$Time, s$Y, 360)$y
  expect_equal(at6 / max(s$Y), 0.81, tolerance = 0.03)
})

test_that("the height term is held at 140 cm and above, so a child's clearance stays positive", {
  cl <- function(h) pimavanserin(3.5, h, 0, "male", FALSE)$PK$default$cl1
  expect_equal(cl(50), cl(PIMAVANSERIN_MIN_HEIGHT))
  expect_gt(cl(50), 0)
})
