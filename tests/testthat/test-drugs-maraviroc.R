# Maraviroc (R/drugs_maraviroc.R): Chan 2008, Tables 4 and 5.  Drafted by
# Claude Code, 2026-11-28.  Expectations are computed here from the paper,
# not from the drug file.

fabs <- function(d) d^1.39 / (51.2^1.39 + d^1.39)
ev <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))

test_that("Chan 2008, Table 4, switch off", {
  for (d in c(100, 300, 600)) {
    set <- maraviroc(70, 170, 30, "male", adjustToFFM = FALSE, dose = d)$PK$default
    expect_equal(set$cl1 * 60, 59.59 * 0.662 + 12)
    expect_equal(round(set$cl1 * 60, 2), 51.45)
    expect_equal(set$v1, 132)
    expect_equal(set$v2, 277)
    expect_equal(set$cl2 * 60, 16.4)
    expect_equal(set$tlag_PO, 0.198 * 60)
    expect_equal(set$ka_PO * 60, 0.277 * d^0.173)
    expect_equal(set$bioavailability_PO, fabs(d) * (1 - 0.662))
  }
  # Table 5: ka and F at each dose (fasted, non-Asian)
  tab5 <- data.frame(d = c(100, 150, 300, 600, 900, 1200),
                     ka = c(0.614, 0.659, 0.743, 0.838, 0.899, 0.944),
                     F = c(0.242, 0.276, 0.311, 0.327, 0.332, 0.334))
  for (i in seq_len(nrow(tab5))) {
    set <- maraviroc(70, 170, 30, "male", FALSE, dose = tab5$d[i])$PK$default
    expect_equal(round(set$ka_PO * 60, 3), tab5$ka[i])
    expect_equal(round(set$bioavailability_PO, 3), tab5$F[i])
  }
})

test_that("age on CLic: 27.4% greater at 60, floored at 18", {
  at <- function(age) maraviroc(70, 170, age, "male", FALSE)$PK$default$cl2 * 60
  expect_equal(round(at(60) / at(30) - 1, 3), 0.274)
  expect_equal(at(5), at(18))
  expect_equal(at(0), at(18))
})

test_that("dose held to the studied 100-1200 mg; 300 mg by default", {
  ka <- function(d) maraviroc(70, 170, 30, "male", FALSE, dose = d)$PK$default$ka_PO
  expect_equal(ka(10), ka(100))
  expect_equal(ka(5000), ka(1200))
  expect_equal(maraviroc(70, 170, 30, "male", FALSE)$PK$default$ka_PO, ka(300))
  expect_true(maraviroc(70, 170, 30, "male")$doseDependent)
})

test_that("switch on: every parameter scaled by fat-free mass, F unchanged", {
  size <- pkSizeFactors(100, 160, 50, "female")
  set <- maraviroc(100, 160, 50, "female", dose = 300)$PK$default
  off <- maraviroc(100, 160, 50, "female", FALSE, dose = 300)$PK$default
  expect_equal(set$v1, 132 * size$volume)
  expect_equal(set$cl1, off$cl1 * size$clearance)
  expect_equal(set$cl2, off$cl2 * size$clearance)
  expect_equal(set$bioavailability_PO, off$bioavailability_PO)
  expect_equal(maraviroc(70, 170, 35, "male", dose = 300)$PK$default$v1, 132)
})

test_that("Table 5's single-dose AUCs, through the dose-dependent engine", {
  for (d in c(100, 150, 300, 600)) {
    w <- simulateDrugsWithCovariates(
      data.frame(Drug = "maraviroc", Time = 0, Dose = d, Units = "mg PO"), ev,
      70, 170, 30, "male", 6 * 1440, FALSE, adjustToFFM = FALSE)$maraviroc$wide
    h <- w$Time / 60
    auc <- sum(diff(h) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
    # Table 5: 471, 805, 1815, 3817 ng.h/mL (AUC to infinity; 6 days holds
    # all but a few ng.h/mL of it)
    expect_equal(auc, c(`100` = 471, `150` = 805, `300` = 1815, `600` = 3817)[[as.character(d)]],
                 tolerance = 0.005)
  }
})

test_that("each dose size is its own system: 150 mg then 600 mg", {
  both <- simulateDrugsWithCovariates(
    data.frame(Drug = "maraviroc", Time = c(0, 720), Dose = c(150, 600), Units = "mg PO"),
    ev, 70, 170, 30, "male", 3 * 1440, FALSE, adjustToFFM = FALSE)$maraviroc$wide
  # The same two dose rows, one of them 0, so that every run has the same time
  # line as the pair
  one <- function(doses) simulateDrugsWithCovariates(
    data.frame(Drug = "maraviroc", Time = c(0, 720), Dose = doses, Units = "mg PO"),
    ev, 70, 170, 30, "male", 3 * 1440, FALSE, adjustToFFM = FALSE)$maraviroc$wide
  a <- one(c(150, 0)); b <- one(c(0, 600))
  expect_equal(a$Time, both$Time)
  expect_equal(b$Time, both$Time)
  expect_lt(max(abs(both$Plasma - (a$Plasma + b$Plasma))), 1e-9 * max(both$Plasma))
})

test_that("300 mg twice daily at steady state (label Table 11)", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "maraviroc", Time = 0, Dose = 300, Units = "mg PO bid"), ev,
    70, 170, 30, "male", 10 * 1440, FALSE, adjustToFFM = FALSE)$maraviroc$wide
  h <- w$Time / 60
  k <- h >= 228 & h <= 240
  auc <- sum(diff(h[k]) * (head(w$Plasma[k], -1) + tail(w$Plasma[k], -1)) / 2)
  expect_equal(round(auc), 1815)                 # label: 1513-2908
  expect_equal(max(w$Plasma[k]), 345, tolerance = 0.005)   # label: 266-888
  expect_equal(min(w$Plasma[k]), 39.0, tolerance = 0.01)   # label: 33.6-60
})

test_that("pregnancy: F x 0.72 (Colbers 2015), nothing else", {
  on  <- maraviroc(70, 165, 30, "female", dose = 300, pregnant = TRUE)
  off <- maraviroc(70, 165, 30, "female", dose = 300)
  expect_equal(on$PK$default$bioavailability_PO / off$PK$default$bioavailability_PO, 0.72)
  expect_equal(on$PK$default$cl1, off$PK$default$cl1)
  expect_match(on$reference, "Colbers 2015")
})

test_that("no band, no effect site", {
  X <- maraviroc(70, 170, 30, "male")
  expect_equal(c(X$tPeak, X$MEAC, X$typical, X$lowerTypical, X$upperTypical), rep(0, 5))
})
