# Colistimethate (CMS) and the colistin it forms (R/drugs_colistimethate.R,
# R/drugs_colistin.R): Garonzik 2011.  Drafted by Claude Code, 2026-11-16.

noEvents <- data.frame(Time = double(), Event = character())

# The creatinine that gives a chosen Garonzik creatinine clearance
creatinineFor <- function(crcl, weight, height, age, sex) {
  w <- colistimethateWeight(weight, height, age, sex, FALSE)
  uniroot(function(s) colistimethateCrCL(w, height, age, sex, s) - crcl,
          c(0.05, 50), tol = 1e-12)$root
}

test_that("the dose basis is the source's own conversion times colistin's share of CMS", {
  # 150 mg CBA ~ 400 mg CMS sodium (Garonzik's Methods); colistin A
  # C53H100N16O13 over CMS sodium C58H105N16Na5O28S5 (the label's formula)
  aw <- c(C = 12.011, H = 1.008, N = 14.007, O = 15.999, Na = 22.990, S = 32.06)
  cms <- sum(aw[c("C", "H", "N", "O", "Na", "S")] * c(58, 105, 16, 28, 5, 5))
  col <- sum(aw[c("C", "H", "N", "O")] * c(53, 100, 16, 13))
  expect_equal(round(cms, 1), 1749.8)    # the label: "approximate molecular weight of 1750"
  expect_equal(round(col, 1), 1169.5)
  expect_equal(COLISTIMETHATE_BASIS, 400 / 150 * col / cms, tolerance = 1e-4)
  expect_equal(round(COLISTIMETHATE_BASIS, 3), 1.782)
  # the source's equation 10 asks 30 mg CBA/day per mg/L at a CrCL of 0,
  # where the model needs 24 x 2.19 of its own units: 1.75
  expect_equal(24 * 2.19 / 30, COLISTIMETHATE_BASIS, tolerance = 0.02)
})

test_that("Table 2: CMS and colistin parameters, in the model's units", {
  for (sex in c("male", "female")) {
    pk <- getDrugPK("colistimethate", 60, 165, 70, sex, adjustToFFM = FALSE)
    s <- pk$PK$default
    f <- COLISTIMETHATE_BASIS
    crcl <- colistimethateCrCL(60, 165, 70, sex, NULL)
    expect_equal(s$v1 * f, 11.9 * 60 / 60)
    expect_equal(s$v2 * f, 18.7)
    expect_equal(s$cl2 * 60 * f, 7.98)
    expect_equal(s$cl1 * 60 * f, 0.0613 * crcl + 1.90)
    # all of the nonrenal clearance forms colistin
    expect_identical(s$metabolite$name, "colistin")
    m <- getDrugPK("colistin", 60, 165, 70, sex, adjustToFFM = FALSE)$PK$default
    expect_equal(m$v1 * f, 45.1)
    expect_equal(m$cl1 * 60 * f, 0.0147 * crcl + 2.19)
  }
  # the switch: V1 reads the pharmacokinetic weight
  on  <- getDrugPK("colistimethate", 90, 170, 35, "male", adjustToFFM = TRUE)$PK$default
  off <- getDrugPK("colistimethate", 90, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  pkw <- pkSizeFactors(90, 170, 35, "male", TRUE)$pkWeight
  expect_equal(on$v1 * COLISTIMETHATE_BASIS, 11.9 * pkw / 60)
  expect_equal(off$v1 * COLISTIMETHATE_BASIS, 11.9 * 90 / 60)
  expect_equal(round(c(on$v1, off$v1, on$cl1 * 60, off$cl1 * 60), 4),
               c(round(11.9 * pkw / 60 / COLISTIMETHATE_BASIS, 4), 10.0152, 5.0335, 5.2751))
})

test_that("creatinine clearance is Cockcroft-Gault per 1.73 m2, held within 0-169", {
  expect_equal(colistimethateCrCL(70, 170, 35, "male", 1),
               creatinineClearanceCG(70, 35, "male", 1) * 1.73 / bsaMosteller(70, 170))
  expect_equal(colistimethateCrCL(80, 190, 20, "male", 0.2), COLISTIN_CRCL_MAX)
  expect_equal(colistimethateCrCL(70, 170, 60, "male", 30),
               creatinineClearanceCG(70, 60, "male", 30) * 1.73 / bsaMosteller(70, 170))
  # a raised creatinine lowers CMS clearance and colistin's apparent clearance
  hi <- getDrugPK("colistimethate", 70, 170, 60, "male", creatinine = 4)$PK$default
  lo <- getDrugPK("colistimethate", 70, 170, 60, "male", creatinine = 0.8)$PK$default
  expect_lt(hi$cl1, lo$cl1)
})

test_that("terminal half-lives against Garonzik's medians by renal function", {
  hl <- function(crcl) {
    s <- creatinineFor(crcl, 60, 165, 70, "male")
    p <- getDrugPK("colistimethate", 60, 165, 70, "male", adjustToFFM = FALSE, creatinine = s)$PK$default
    m <- getDrugPK("colistin", 60, 165, 70, "male", adjustToFFM = FALSE, creatinine = s)$PK$default
    c(cms = log(2) / min(p$lambda_1, p$lambda_2) / 60, colistin = log(2) / m$lambda_1 / 60)
  }
  # Garonzik: CMS 11, 5.6, 4.6 h; colistin 13, 13, 9.1 h (below 10, 11-69,
  # above 70 mL/min/1.73 m2)
  expect_equal(round(hl(5), 1),   c(cms = 10.7, colistin = 13.8))
  expect_equal(round(hl(40), 1),  c(cms = 6.0, colistin = 11.3))
  expect_equal(round(hl(100), 1), c(cms = 3.8, colistin = 8.5))
})

test_that("the curves match an independent solution of equations 1-3", {
  s <- 2
  d <- data.frame(Drug = "colistimethate", Time = c(0, 30, 720, 750),
                  Dose = c(300, 0, 300, 0), Units = "mg/hr")
  sim <- simulateDrugsWithCovariates(d, noEvents, 70, 170, 60, "male", 2 * 1440, FALSE,
                                     adjustToFFM = FALSE, creatinine = s)
  crcl <- colistimethateCrCL(70, 170, 60, "male", s)
  # the source's equations in its own units: the dose in mg CBA times the basis
  f <- COLISTIMETHATE_BASIS
  V1 <- 11.9 * 70 / 60; V2 <- 18.7; Q <- 7.98 / 60
  CLT <- (0.0613 * crcl + 1.90) / 60; CLF <- 1.90 / 60
  V3 <- 45.1; CLC <- (0.0147 * crcl + 2.19) / 60
  rate <- function(t) if ((t %% 720) < 30 && t < 750) 300 / 60 * f else 0
  # the rate is held over each step, which never crosses a switch
  deriv <- function(r, y) {
    c1 <- y[1] / V1
    c(r - Q * (c1 - y[2] / V2) - CLT * c1, Q * (c1 - y[2] / V2), CLF * c1 - CLC * y[3] / V3)
  }
  p <- sim$colistimethate$wide
  m <- sim$colistin$wide
  # compared at the library's own time points nearest these, away from the
  # infusions' switches
  checks <- p$Time[vapply(c(60, 240, 700, 900, 1440, 2800),
                          function(x) which.min(abs(p$Time - x)), integer(1))]
  rk4 <- function(y, t, h) {
    r <- rate(t + h / 2)
    k1 <- deriv(r, y); k2 <- deriv(r, y + h / 2 * k1)
    k3 <- deriv(r, y + h / 2 * k2); k4 <- deriv(r, y + h * k3)
    y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  stops <- sort(unique(c(checks, 30, 720, 750)))
  y <- c(0, 0, 0)
  t <- 0
  ref <- matrix(NA_real_, length(checks), 2)
  for (stop in stops) {
    while (t < stop - 1e-9) {
      h <- min(0.05, stop - t)
      y <- rk4(y, t, h)
      t <- t + h
    }
    t <- stop
    i <- match(stop, checks)
    if (!is.na(i)) ref[i, ] <- c(y[1] / V1, y[3] / V3)
  }
  expect_equal(p$Plasma[match(checks, p$Time)], ref[, 1], tolerance = 1e-7)
  expect_equal(m$Plasma[match(checks, m$Time)], ref[, 2], tolerance = 1e-7)
})

test_that("Garonzik's own example: 225 mg CBA a day at a CrCL of 40 gives about 2.5 mg/L", {
  # Table 3 footnote a: a 55-kg patient with a CrCL of 40 mL/min/1.73 m2,
  # targeting 2.5 mg/L, gets 225 mg CBA/day (the algorithm, which the model
  # gives as 2.63 at steady state: an implementation check, not independent)
  s <- creatinineFor(40, 55, 165, 60, "male")
  d <- data.frame(Drug = "colistimethate", Time = 0, Dose = 112.5, Units = "mg bid")
  sim <- simulateDrugsWithCovariates(d, noEvents, 55, 165, 60, "male", 8 * 1440, FALSE,
                                     adjustToFFM = FALSE, creatinine = s)
  m <- sim$colistin$wide
  last <- m[m$Time >= 7 * 1440, ]
  cssAvg <- sum(diff(last$Time) * (head(last$Plasma, -1) + tail(last$Plasma, -1)) / 2) / 1440
  expected <- 225 * COLISTIMETHATE_BASIS / 24 * 1.90 / (1.90 + 0.0613 * 40) / (2.19 + 0.0147 * 40)
  expect_equal(round(expected, 2), 2.63)
  expect_equal(cssAvg, expected, tolerance = 0.01)
})

test_that("colistimethate is an antibiotic forming the metabolite-only colistin, with no line or band", {
  dd <- getDrugDefaultsGlobal()
  p <- dd[dd$Drug == "colistimethate", ]
  m <- dd[dd$Drug == "colistin", ]
  expect_identical(p$Category, "Antibiotics")
  expect_identical(p$Concentration.Units, "mcg")
  expect_true(all(c("mg", "mg/hr", "mg bid") %in% p$Units[[1]]))
  expect_equal(c(p$Lower, p$Upper, p$Typical, p$MEAC, p$endCe), rep(0, 5))
  expect_true(is.na(m$Category) || !nzchar(m$Category))
  expect_equal(c(m$Lower, m$Upper, m$Typical, m$MEAC, m$endCe), rep(0, 5))
  expect_false("colistin" %in% antibioticMicTable()$Drug)
  expect_identical(therapeuticRange("colistimethate")$band_status, "not_applicable")
  expect_identical(therapeuticRange("colistin")$band_status, "none_established")
  expect_match(getDrugPK("colistimethate", 70, 170, 35, "male")$reference, "Garonzik")
})
