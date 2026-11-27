# Dolutegravir (R/drugs_dolutegravir.R): Zhang 2015's one-compartment model,
# Table 3.  Drafted by Claude Code, 2026-11-27.  Expectations are computed
# here from the paper and the label, not from the drug file.

zhang <- function(w, age, female = FALSE) {
  f <- if (female) 1.21 else 1
  list(cl = 0.901 * (w / 70)^0.438 * (max(age, 18) / 40)^0.193 / f,
       v = 17.4 * (w / 70)^0.768 / f)
}

test_that("Zhang 2015, Table 3, switch off", {
  for (w in c(39, 70, 135)) for (a in c(18, 40, 68)) {
    set <- dolutegravir(w, 170, a, "male", adjustToFFM = FALSE)$PK$default
    ref <- zhang(w, a)
    expect_equal(set$cl1, ref$cl / 60)
    expect_equal(set$v1, ref$v)
  }
  set <- dolutegravir(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1 * 60, 0.901)
  expect_equal(set$v1, 17.4)
  expect_equal(set$ka_PO, 2.24 / 60)
  expect_equal(set$tlag_PO, 0.263 * 60)
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$cl2, 0)
  expect_equal(set$cl3, 0)
})

test_that("the paper's own ranges of CL/F and V/F (p. 507)", {
  cl <- function(w, a) dolutegravir(w, 170, a, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60
  v <- function(w) dolutegravir(w, 170, 40, "male", adjustToFFM = FALSE)$PK$default$v1
  # "for the range of weights ... (39-135 kg), CL/F ranged from 0.697-1.20 L/h
  # ... and V/F ranged from 11.1-28.8 L"
  expect_equal(round(c(cl(39, 40), cl(135, 40)), 3), c(0.697, 1.201))
  expect_equal(round(c(v(39), v(135)), 1), c(11.1, 28.8))
  # "for the range of age ... (18-68 years), CL/F ranged from 0.772-0.998 L/h"
  expect_equal(round(c(cl(70, 18), cl(70, 68)), 3), c(0.772, 0.998))
})

test_that("women: bioavailability x 1.21, entered on the apparent parameters", {
  m <- dolutegravir(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  f <- dolutegravir(70, 170, 40, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(m$cl1 / f$cl1, 1.21)
  expect_equal(m$v1 / f$v1, 1.21)
  expect_equal(f$ka_PO, m$ka_PO)
})

test_that("the age term is held at 18 years and stays finite at age 0", {
  a0 <- dolutegravir(10, 80, 0, "male", adjustToFFM = FALSE)$PK$default
  a18 <- dolutegravir(10, 80, 18, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(a0$cl1, a18$cl1)
  expect_gt(a0$cl1, 0)
})

test_that("switch on: weight at the pharmacokinetic weight", {
  pkW <- pkSizeFactors(120, 170, 50, "male")$pkWeight
  set <- dolutegravir(120, 170, 50, "male")$PK$default
  expect_equal(set$cl1, zhang(pkW, 50)$cl / 60)
  expect_equal(set$v1, zhang(pkW, 50)$v)
  ref <- dolutegravir(70, 170, 35, "male")$PK$default
  expect_equal(ref$cl1, zhang(70, 35)$cl / 60)
})

test_that("50 mg once daily at steady state against Table 4 and a closed form", {
  dt <- data.frame(Drug = "dolutegravir", Time = 0, Dose = 50, Units = "mg PO qd")
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 40, "male", 10 * 1440, FALSE,
                                   adjustToFFM = FALSE)$dolutegravir$wide
  last <- w[w$Time >= 9 * 1440, ]
  # Independent: a one-compartment oral model at steady state, tau 24 h
  ka <- 2.24; k <- 0.901 / 17.4; tau <- 24
  css <- function(t) 50 * ka / (17.4 * (ka - k)) *
    (exp(-k * t) / (1 - exp(-k * tau)) - exp(-ka * t) / (1 - exp(-ka * tau)))
  # Day 10 is steady state to 1e-5 (half-life 13.4 h)
  t <- (last$Time - 9 * 1440) / 60 - 0.263
  ok <- t > 0 & t < 23.7
  expect_equal(last$Plasma[ok], css(t[ok]), tolerance = 1e-4)
  peak <- optimize(css, c(0, 24), maximum = TRUE)
  expect_equal(round(peak$objective, 2), 3.72)       # Table 4, SPRING-2: 3.69
  expect_equal(round(peak$maximum + 0.263, 1), 1.8)  # Table 4: 2.0 h
  expect_equal(round(css(24 - 0.263), 2), 1.21)      # 24 h after a dose; Table 4: 1.10
  expect_equal(round(50 / 0.901, 1), 55.5)           # Table 4: 53.6 mcg.h/mL
  expect_equal(round(log(2) * 17.4 / 0.901, 1), 13.4)  # label: about 14 h
  expect_equal(round(max(last$Plasma), 2), 3.72)
})

test_that("no band, no effect site", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "dolutegravir", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$MEAC, row$endCe), c(0, 0, 0, 0, 0))
  expect_identical(therapeuticRange("dolutegravir")$band_status, "none_established")
  expect_equal(dolutegravir(70, 170, 40, "male")$tPeak, 0)
})
