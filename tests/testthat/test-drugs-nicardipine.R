# Nicardipine (R/drugs_nicardipine.R): two compartments per kilogram from the
# mean disposition parameters of Modi 1993's seven healthy men.
# Drafted by Claude Code, 2026-10-19.

# Modi NB et al., J Pharm Sci 1993;82:705-713, Table I
CL <- 0.920; V1 <- 0.291; VSS <- 1.13; Q <- 0.826   # per kg, L and L/h

test_that("the parameters are Table I's means, per kilogram", {
  PK <- getDrugPK("nicardipine", 74, 180, 23, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(V1 * 74, set$v1)
  expect_equal_rounded((VSS - V1) * 74, set$v2)
  expect_equal_rounded(CL / 60 * 74, set$cl1)
  expect_equal_rounded(Q / 60 * 74, set$cl2)
  expect_equal(PK$tPeak, 0)
  # Half-lives 6.4 min and 1.45 h
  expect_equal(round(log(2) / set$lambda_1, 1), 6.4)
  expect_equal(round(log(2) / set$lambda_2 / 60, 2), 1.45)
})

test_that("the distribution clearance row reproduces each subject's distribution time", {
  # Table I, subjects 1-7: CL, V, Vss, C_d and the printed distribution time.
  # Subject 3's CL is printed 0.833; 0.883 satisfies every identity below and
  # makes the mean the printed 0.920 (0.833 gives 0.914), so 0.833 is a
  # transposition.
  cl <- c(.975, 1.15, .883, 1.09, .795, .795, .763)
  v  <- c(.372, .262, .121, .329, .374, .252, .329)
  vss <- c(2.03, .791, .565, .836, 1.56, .955, 1.20)
  cd <- c(.737, .919, 1.27, .851, .548, .733, .723)
  td <- c(.655, .293, .157, .356, .760, .423, .557)
  tdOf <- function(CL, V1, V2, Q) {
    k10 <- CL / V1; k12 <- Q / V1; k21 <- Q / V2
    s <- k10 + k12 + k21; p <- k10 * k21
    a <- (s + sqrt(s^2 - 4 * p)) / 2; b <- (s - sqrt(s^2 - 4 * p)) / 2
    log(a / b) / (a - b)
  }
  model <- mapply(tdOf, cl, v, vss - v, cd)
  expect_true(all(abs(model - td) < 0.005))
  # and V = CL x MRT(sampling), Vss = CL x MRT(body), to rounding
  expect_true(all(abs(v / cl - c(.382, .229, .138, .303, .470, .316, .431)) < 0.005))
  expect_true(all(abs(vss / cl - c(2.08, .691, .640, .770, 1.97, 1.20, 1.57)) < 0.01))
  expect_equal(round(mean(cl), 2), 0.92)
})

test_that("weight scales every parameter, or the pharmacokinetic weight", {
  size <- pkSizeFactors(116, 181, 50, "male", TRUE)
  on <- nicardipine(116, 181, 50, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(V1 * size$pkWeight, on$v1)
  expect_equal_rounded(CL / 60 * size$pkWeight, on$cl1)
})

test_that("nicardipine is a plasma-only vasoactive drug", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "nicardipine", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(c(row$MEAC, row$Lower, row$Upper), c(0, 0, 0))
})
