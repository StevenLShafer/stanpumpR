# Mexiletine (R/drugs_mexiletine.R): Vozeh 1982's one compartment.  Drafted
# by Claude Code, 2026-11-13.  Expectations from Table 1 and the Discussion
# of the source, not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table 1, proportional to body weight", {
  p <- mexiletine(70, 170, 65, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(p$cl1 * 60, 0.38 * 70)
  expect_equal(p$v1, 5.3 * 70)
  expect_equal(p$ka_PO * 60, 3.1)
  expect_equal(p$tlag_PO / 60, 0.3)
  # the paper's half-life at 70 kg, 9.7 h
  expect_equal(log(2) * p$v1 / p$cl1 / 60, 9.7, tolerance = 0.005)
})

test_that("the paper's own predictions after 500 mg and on 250 mg every 8 h", {
  d <- data.frame(Drug = "mexiletine", Time = 0, Dose = 500, Units = "mg PO")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 65, "male", 1440, FALSE,
                                   adjustToFFM = FALSE)$mexiletine$wide
  at <- function(h) w$Plasma[which.min(abs(w$Time - h * 60))]
  expect_equal(at(1.5), 1.25, tolerance = 0.03)
  expect_equal(at(6), 0.95, tolerance = 0.05)
  d <- data.frame(Drug = "mexiletine", Time = 0, Dose = 250, Units = "mg PO tid")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 65, "male", 5 * 1440, FALSE,
                                   adjustToFFM = FALSE)$mexiletine$wide
  s <- w[w$Time >= 4 * 1440 & w$Time <= 4 * 1440 + 480, ]
  avg <- sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 480
  expect_equal(avg, 1.2, tolerance = 0.03)
})

test_that("the band is the paper's 0.8-2 mg/L", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "mexiletine", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0.8, 2, 1.4))
})

test_that("switch on, a 120-kg man: proportional to the FFM weight, by hand", {
  pkW <- 70 * ffmRatioMaleIndependent(120, 170, 50)
  p <- mexiletine(120, 170, 50, "male")$PK$default
  expect_equal_rounded(5.3 * pkW, p$v1)
  expect_equal_rounded(0.38 * pkW / 60, p$cl1)
})
