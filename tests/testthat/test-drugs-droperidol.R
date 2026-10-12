# droperidol (IV, Lehmann 1988 pooled) and droperidolIM (Foo 2016): separate
# drugs, because the IM parameters are apparent and the IV ones absolute.
# The pins are the published curves themselves, not the code's constants.

noEvents <- data.frame(Time = numeric(0), Event = character(0))
ivDroperidol <- function(dt, maximum) {
  simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", maximum, FALSE,
                              adjustToFFM = FALSE)$droperidol$wide
}
at <- function(w, t) sapply(t, function(x) w$Plasma[which.min(abs(w$Time - x))])

test_that("the reference patient receives the pooled Lehmann constants", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- droperidol(70, 170, 35, "male", adjustToFFM = adjust)
    v1 <- 10.75
    expected <- list(
      PK = list(default = list(
        v1 = v1, v2 = v1 * 0.2298 / 0.07393, v3 = v1 * 0.06160 / 0.01355,
        cl1 = v1 * 0.06015, cl2 = v1 * 0.2298, cl3 = v1 * 0.06160
      )),
      tPeak = 0, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
  on <- droperidol(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on$v1, 10.75 * 1.3049067)
  expect_equal_rounded(on$cl1, 10.75 * 0.06015 * 1.2209126)
})

test_that("it reproduces each of Lehmann's mean curves within 20% from 6 min", {
  # Before 6 min the three published curves disagree among themselves (their
  # fast exponents run from 0.30 to 0.54 /min), and the pooled model is up to
  # 32% off the 15 mg curve at 1 to 3 min.
  lehmann <- list(
    `5`  = list(C = c(697, 41.5, 26.5), L = c(0.54, 0.028, 0.0047)),
    `10` = list(C = c(661, 73.1, 56.2), L = c(0.30, 0.025, 0.0053)),
    `15` = list(C = c(853, 83.2, 87.0), L = c(0.38, 0.031, 0.0053))
  )
  times <- c(6, 10, 15, 20, 30, 45, 60, 90, 120, 180, 240, 300, 360)
  for (d in names(lehmann)) {
    w <- ivDroperidol(data.frame(Drug = "droperidol", Time = 0,
                                 Dose = as.numeric(d), Units = "mg"), 360)
    pub <- sapply(times, function(t) sum(lehmann[[d]]$C * exp(-lehmann[[d]]$L * t)))
    expect_true(all(abs(at(w, times) / pub - 1) < 0.20), info = d)
  }
})

test_that("Gupta's 24 h infusion of 125 mcg/h reaches about 3.2 ng/mL", {
  w <- ivDroperidol(data.frame(Drug = "droperidol", Time = c(0, 1440),
                               Dose = c(0.125, 0), Units = "mg/hr"), 1440)
  # Gupta 1992: plateau about 3.2 ng/mL (intravenous Cmax 3.2 +/- 0.6)
  expect_equal(at(w, 1440), 3.2, tolerance = 0.1)
})

test_that("clearance sits among the adult studies; Vss matches Fischler's", {
  x <- droperidol(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  cl <- x$cl1 * 1000                                # mL/min
  expect_gt(cl, 563)                                # Cooper NCA, 33.8 L/h
  expect_lt(cl, 798)                                # Lehmann mean of individuals
  expect_equal((x$v1 + x$v2 + x$v3) / 66.4, 1.40, tolerance = 0.01)  # Fischler, L/kg
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, x$cl3 / x$v1, x$cl2 / x$v2, x$cl3 / x$v3)
  expect_equal(sort(log(2) / r[r > 0]), c(1.71, 24.0, 134.6), tolerance = 0.01)
})
