# Selexipag and ACT-333679 (R/drugs_selexipag.R, R/drugs_ACT333679.R):
# Krause 2017's joint two-plus-two-compartment model.  Drafted by Claude Code,
# 2026-11-15.  Expectations are computed here from Table 1, not from the drug
# files; the curves are checked against an independent integration of the
# source's differential equations.

TLAG <- 0.67; KA <- 0.71; VP <- 12.9; CL <- 19.1; K12 <- 0.09; K21 <- 0.06
KMET <- 0.67; VM <- 4.65; K34 <- 1.04; K43 <- 0.18; KM <- 0.49

events <- data.frame(Time = double(), Event = character())

test_that("Table 1 at 70 kg, switch off; CL/F excludes conversion", {
  set <- selexipag(70, 165, 50, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(set$v1, VP)
  expect_equal(set$cl1 * 60, CL + KMET * VP)          # 27.7 L/h in all
  expect_equal(set$cl2 * 60 / set$v1, K12)
  expect_equal(set$cl2 / set$v2, K21 / 60)
  expect_equal(set$ka_PO, KA / 60)
  expect_equal(set$tlag_PO, TLAG * 60)
  m <- ACT333679(70, 165, 50, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(m$v1, VM)
  expect_equal(m$cl1 * 60 / m$v1, KM)
  expect_equal(m$cl2 * 60 / m$v1, K34)
  expect_equal(m$cl2 * 60 / m$v2, K43)
  # men: km x exp(0.15) = 0.57 /h (Table 2)
  male <- ACT333679(70, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(male$cl1 * 60 / male$v1, 2), 0.57)
})

test_that("the weight terms (switch off: total weight)", {
  set <- selexipag(96, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  vp <- VP * (96 / 70)^1.2
  expect_equal(set$v1, vp)
  expect_equal(set$cl1 * 60, CL * (96 / 70)^0.61 + KMET * vp)
  m <- ACT333679(96, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(m$v1, VM * (96 / 70)^0.88)
})

test_that("switch on: the pharmacokinetic weight", {
  w <- 1.3049067                 # 120 kg, 170 cm, 50 y man
  set <- selexipag(120, 170, 50, "male")$PK$default
  expect_equal_rounded(set$v1, VP * w^1.2)
  m <- ACT333679(120, 170, 50, "male")$PK$default
  expect_equal_rounded(m$v1, VM * w^0.88)
})

test_that("both curves match an independent integration of the source's model", {
  dose <- 400                    # mcg, so ng/mL in litres
  sim <- simulateDrugsWithCovariates(
    data.frame(Drug = "selexipag", Time = 0, Dose = dose, Units = "mcg PO"),
    events, 70, 165, 40, "female", 2880, FALSE, adjustToFFM = FALSE)
  # Euler-free: the linear system's matrix exponential, in hours
  A <- matrix(0, 5, 5)           # gut, Cp, P2, Cm, M2 as amounts
  A[1, 1] <- -KA
  A[2, 1] <- KA; A[2, 2] <- -(CL / VP + KMET + K12); A[2, 3] <- K21
  A[3, 2] <- K12; A[3, 3] <- -K21
  A[4, 2] <- KMET; A[4, 4] <- -(KM + K34); A[4, 5] <- K43
  A[5, 4] <- K34; A[5, 5] <- -K43
  e <- eigen(A); V <- e$vectors; Vi <- solve(V)
  at <- function(h) {
    if (h <= TLAG) return(c(0, 0))
    x <- Re(V %*% diag(exp(e$values * (h - TLAG))) %*% Vi %*% c(dose, 0, 0, 0, 0))
    c(x[2] / VP, x[4] / VM)
  }
  # compared at the simulation's own time points, not interpolated
  wp <- sim$selexipag$wide; wm <- sim$ACT333679$wide
  for (h in c(1, 2, 4, 8, 24)) {
    i <- which.min(abs(wp$Time - h * 60))
    j <- which.min(abs(wm$Time - h * 60))
    expect_equal(wp$Plasma[i], at(wp$Time[i] / 60)[1], tolerance = 1e-6)
    expect_equal(wm$Plasma[j], at(wm$Time[j] / 60)[2], tolerance = 1e-6)
  }
})

test_that("the AUC ratio is the label's 3- to 4-fold", {
  expect_equal(round(KMET * VP / (KM * VM), 1), 3.8)
})

test_that("ACT333679 cannot be dosed; selexipag is oral only", {
  dd <- getDrugDefaultsGlobal()
  expect_true(all(is.na(unlist(dd$Units[dd$Drug == "ACT333679"]))))
  expect_true(is.na(dd$Category[dd$Drug == "ACT333679"]))
  expect_true(all(doseRoute(c("mcg PO", "mcg PO bid")) == ROUTE_PO))
})
