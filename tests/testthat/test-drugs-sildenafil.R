# Sildenafil (R/drugs_sildenafil.R): the healthy-volunteer model of the FDA's
# review of NDA 22-473 (Revatio injection).  Drafted by Claude Code,
# 2026-11-15.  Expectations are computed here from Table 9, not from the drug
# file.

CL <- 29.5; V1 <- 73.2; V2 <- 33.3; Q <- 5.32; KA <- 25.6; TLAG <- 0.485
F20 <- 0.355; FEXP <- 0.289

events <- data.frame(Time = double(), Event = character())
run <- function(dose, units, maximum = 1500) simulateDrugsWithCovariates(
  data.frame(Drug = "sildenafil", Time = 0, Dose = dose, Units = units),
  events, 70, 170, 35, "male", maximum, FALSE, adjustToFFM = FALSE)$sildenafil$wide
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)

test_that("the parameters are Table 9's, in minutes (switch off)", {
  X <- sildenafil(90, 185, 60, "female", adjustToFFM = FALSE)
  set <- X$PK$default
  expect_equal(set$v1, V1)
  expect_equal(set$v2, V2)
  expect_equal(set$cl1, CL / 60)
  expect_equal(set$cl2, Q / 60)
  expect_equal(set$ka_PO, KA / 60)
  expect_equal(set$tlag_PO, TLAG * 60)
  expect_equal(set$bioavailability_PO, F20)
  expect_equal(X$oralSaturation,
               list(form = "power", Dref = 20, exponent = FEXP))
})

test_that("Vss and the half-lives against the label", {
  set <- getDrugPK("sildenafil", 70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  # label: Vss 105 L, terminal half-life "about 4 hours"
  expect_equal(set$v1 + set$v2, 106.5)
  expect_equal(round(log(2) / set$lambda_2 / 60, 1), 5.5)
})

test_that("oral bioavailability is the review's power of the dose", {
  ivAuc <- auc(run(20, "mg", 6000)) / 20
  for (dose in c(20, 50)) {
    ratio <- auc(run(dose, "mg PO", 6000)) / dose / ivAuc
    expect_equal(ratio, F20 * (dose / 20)^FEXP, tolerance = 0.01)
  }
  # the review's own check: 46% at 50 mg (41% measured)
  expect_equal(round(F20 * (50 / 20)^FEXP, 2), 0.46)
})

test_that("nothing reaches the plasma before the lag", {
  w <- run(20, "mg PO")
  expect_equal(max(w$Plasma[w$Time < TLAG * 60 - 0.01]), 0)
})

test_that("REVAKI-1 (not fitted): healthy clearance, a lower AUC than in surgery", {
  # 10 mg IV: dose / CL = 339 ng.h/mL here; REVAKI-1's 568 in patients of 70
  expect_equal(10000 / CL, 339, tolerance = 0.001)
  expect_lt(auc(run(10, "mg", 6000)) / 60, 568)
})

test_that("switch on: fat-free mass scaling", {
  # 120 kg, 170 cm, 50 y man: fat-free-mass factor 1.3049067
  f <- 1.3049067
  set <- sildenafil(120, 170, 50, "male")$PK$default
  expect_equal_rounded(set$v1, V1 * f)
  expect_equal_rounded(set$v2, V2 * f)
  expect_equal_rounded(set$cl1, CL / 60 * f^0.75)
  expect_equal_rounded(set$cl2, Q / 60 * f^0.75)
})
