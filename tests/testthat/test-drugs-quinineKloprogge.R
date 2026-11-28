# Quinine, adults (R/drugs_quinineKloprogge.R): Kloprogge 2014, Table 2, and
# the quinine group (R/modelSelection.R).  Drafted by Claude Code, 2026-11-28.
# Expectations are computed here from the paper, not from the drug file.

salt <- 2 * 324.42 / 782.96

test_that("Kloprogge 2014, Table 2, switch off", {
  for (w in c(44, 56.5, 71, 100)) {
    set <- quinineKloprogge(w, 160, 25, "female", adjustToFFM = FALSE)$PK$default
    expect_equal(set$cl1 * 60, 10.4 * (w / 56.5)^(2 / 3))
    expect_equal(set$cl2 * 60, 10.7 * (w / 56.5)^(2 / 3))
    expect_equal(set$v1, 174 * w / 56.5)
    expect_equal(set$v2, 54.3 * w / 56.5)
    expect_equal(set$ka_PO * 60, 0.817)
    expect_equal(set$tlag_PO, 0)
    expect_equal(set$bioavailability_PO, salt)
    expect_equal(round(salt, 4), 0.8287)
  }
})

test_that("switch on: the weight terms read the pharmacokinetic weight", {
  pkW <- pkSizeFactors(80, 175, 45, "male")$pkWeight
  set <- quinineKloprogge(80, 175, 45, "male")$PK$default
  expect_equal(set$v1, 174 * pkW / 56.5)
  expect_equal(set$cl1 * 60, 10.4 * (pkW / 56.5)^(2 / 3))
})

test_that("half-life against the paper's post hoc median (15.3 h)", {
  set <- quinineKloprogge(56.5, 160, 25, "female", FALSE)$PK$default
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  a <- k10 + k12 + k21
  expect_equal(round(log(2) / ((a - sqrt(a^2 - 4 * k10 * k21)) / 2) / 60, 1), 16.2)
})

test_that("the group chooses Kloprogge for adults and teenagers, Le Jouan for young children", {
  pick <- function(w, h, a, s) quinine(w, h, a, s, adjustToFFM = FALSE)$modelChosen
  expect_identical(pick(56.5, 160, 25, "female"), "quinineKloprogge")
  expect_identical(pick(80, 175, 45, "male"), "quinineKloprogge")    # nearest by years
  expect_identical(pick(45, 155, 14, "female"), "quinineKloprogge")
  expect_identical(pick(15, 95, 3, "male"), "quinineLeJouan")
  expect_identical(pick(30, 135, 10, "female"), "quinineLeJouan")
  X <- quinine(56.5, 160, 25, "female", adjustToFFM = FALSE)
  expect_equal(X$PK$default$cl1,
               quinineKloprogge(56.5, 160, 25, "female", FALSE)$PK$default$cl1)
})

test_that("the trial's regimen through the engine", {
  ev <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "quinine", Time = 0, Dose = 10, Units = "mg/kg PO tid"), ev,
    56.5, 160, 25, "female", 7 * 1440, FALSE, adjustToFFM = FALSE)$quinine$wide
  h <- w$Time / 60
  k <- h <= 8
  auc8 <- sum(diff(h[k]) * (head(w$Plasma[k], -1) + tail(w$Plasma[k], -1)) / 2)
  expect_equal(round(auc8, 1), 13.2)                       # paper's post hoc 26.6
  expect_equal(round(max(w$Plasma[h >= 160]), 1), 6.2)     # paper's post hoc 4.06
})
