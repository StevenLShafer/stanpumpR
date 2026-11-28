# Tecovirimat (R/drugs_tecovirimat.R): Leeds 2013's human model, Table 1.
# Drafted by Claude Code, 2026-11-28.  Expectations are computed here from the
# paper and the label, not from the drug file.

leeds <- function(w, female) list(
  cl = 41.15 * (w / 78.4)^0.75, q = 36.79 * (w / 78.4)^0.75,
  vc = (if (female) 281.51 else 217.44) * w / 78.4, vp = 413.53 * w / 78.4)

halfLives <- function(set) {
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  a <- k10 + k12 + k21
  log(2) / (c(a + sqrt(a^2 - 4 * k10 * k21), a - sqrt(a^2 - 4 * k10 * k21)) / 2) / 60
}

test_that("Leeds 2013, Table 1, switch off", {
  for (w in c(45, 78.4, 130)) for (sex in c("male", "female")) {
    set <- tecovirimat(w, 170, 40, sex, adjustToFFM = FALSE)$PK$default
    ref <- leeds(w, sex == "female")
    expect_equal(set$cl1, ref$cl / 60)
    expect_equal(set$cl2, ref$q / 60)
    expect_equal(set$v1, ref$vc)
    expect_equal(set$v2, ref$vp)
    expect_equal(set$ka_PO, 1.06 / 60)
    expect_equal(set$tlag_PO, 1.46 * 60)
    expect_equal(set$bioavailability_PO, 1)
    expect_equal(c(set$v3, set$cl3), c(1, 0))
  }
})

test_that("switch on: the weight terms read the pharmacokinetic weight", {
  pkW <- pkSizeFactors(120, 170, 50, "female")$pkWeight
  set <- tecovirimat(120, 170, 50, "female")$PK$default
  ref <- leeds(pkW, TRUE)
  expect_equal(set$cl1, ref$cl / 60)
  expect_equal(set$v1, ref$vc)
  expect_equal(set$v2, ref$vp)
  expect_equal_rounded(tecovirimat(70, 170, 35, "male")$PK$default$v1, 194.142857)
})

test_that("the paper's half-lives: 16.7 h in men, 17.4 h in women", {
  m <- tecovirimat(78.4, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  f <- tecovirimat(78.4, 165, 40, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(round(halfLives(m)[2], 1), 16.7)
  expect_equal(round(halfLives(f)[2], 1), 17.4)
})

test_that("600 mg every 12 hours against the label's steady state", {
  steady <- function(w) {
    dt <- data.frame(Drug = "tecovirimat", Time = 0, Dose = 600, Units = "mg PO bid")
    events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
    x <- simulateDrugsWithCovariates(dt, events, w, 175, 40, "male", 14 * 1440, FALSE,
                                     adjustToFFM = FALSE)$tecovirimat$wide
    h <- x$Time / 60
    k <- h >= 13 * 24 & h <= 14 * 24
    auc <- sum(diff(h[k]) * (head(x$Plasma[k], -1) + tail(x$Plasma[k], -1)) / 2)
    last <- h >= 14 * 24 - 12 & h <= 14 * 24
    c(auc = auc, cmax = max(x$Plasma[last]), trough = min(x$Plasma[last]))
  }
  # Label 12.3: AUC(0-24) 29,816 ng.h/mL under 120 kg; 19,500 over 120 kg
  s <- steady(78.4)
  expect_equal(round(s[["auc"]], -1), 29170)
  expect_equal(round(s[["cmax"]]), 2155)       # label 2159
  expect_equal(round(s[["trough"]]), 610)      # label 845
  expect_lt(abs(s[["auc"]] / 29816 - 1), 0.03)
  b <- steady(130)
  expect_lt(abs(b[["auc"]] / 19500 - 1), 0.03)
})

test_that("no band, no effect site", {
  X <- tecovirimat(78.4, 175, 40, "male")
  expect_equal(c(X$tPeak, X$MEAC, X$typical, X$lowerTypical, X$upperTypical), rep(0, 5))
})
