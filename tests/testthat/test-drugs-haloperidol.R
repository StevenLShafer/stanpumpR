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
      tPeak = 0, ke0 = 0.213 / 60, MEAC = 0,
      typical = 3, upperTypical = 5, lowerTypical = 1,
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

test_that("CYP2D6 scales clearance by Park's *10/*10 ratio", {
  cl <- function(p) haloperidol(70, 170, 35, "male", cyp2d6 = p,
                                adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(cl("normal"), 88)
  expect_equal(cl("intermediate"), 88 * 0.77)   # 3.6 / 4.7 L/h/kg, Park 2006
  expect_equal(cl("poor"), 88 * 0.77)           # not studied: nearest value
  expect_equal(cl("ultrarapid"), 88)
  expect_equal(3.6 / 4.7, 0.77, tolerance = 0.01)
  expect_error(haloperidol(70, 170, 35, "male", cyp2d6 = "fast"), "Invalid cyp2d6")
  # Only clearance changes
  a <- haloperidol(70, 170, 35, "male", cyp2d6 = "poor", adjustToFFM = FALSE)$PK$default
  expect_equal(a$v1, 669)
  expect_equal(a$cl2, 233 / 60)
})

test_that("Lim's ke0 is supplied, so the effect site lags plasma by hours", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "haloperidol", Time = 0, Dose = 5, Units = "mg PO"),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 170, 35, "male", 2880, FALSE, adjustToFFM = FALSE
  )$haloperidol$wide
  tp <- w$Time[which.max(w$Plasma)]
  te <- w$Time[which.max(w[["Effect Site"]])]
  expect_gt(te - tp, 60)
})
