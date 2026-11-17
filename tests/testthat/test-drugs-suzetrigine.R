# Suzetrigine and M6-SUZ (R/drugs_suzetrigine.R, R/drugs_M6SUZ.R): the
# applicant's model 2822 in the FDA Integrated Review of NDA 219209, with the
# enzyme autoinduction advanced by the saturable engine
# (R/advanceMichaelisMenten.R: AUTOINDUCTION with a Hill coefficient, the
# METABOLITE it advances, the zero-order depot release and the dose-dependent
# ka).  Drafted by Claude Code, 2026-11-16.  The expectations are computed
# here from Table 149 and Figure 24, not from the drug file, and the curves are
# checked against an independent integration of the review's equations.

noEvents <- data.frame(Time = double(), Event = character())

# Table 149, in hours
P <- list(CL = 5.56, male = 1.21, bmiExp = -0.605, bmiRef = 27.8, wtRef = 76.2,
          V2P = 103, V3P = 204, Q3P = 19, KA = 2.02 * 2.24, F1 = 0.88,
          ALAG = 0.182, D1 = 1.1, FR = 0.8, CLMrel = 0.273, V2M = 1.7,
          V3M = 29.5, Q3M = 8.75, KENZ = 0.00939, AMAX = 0.811, AC50 = 432,
          AHILL = 10, fD50 = 162, fMax = 0.419, fHill = 5, kD50 = 36,
          kMax = 0.895)

clp <- function(wt, ht, sex) P$CL * (if (sex == "male") P$male else 1) *
  ((wt / (ht / 100)^2) / P$bmiRef)^P$bmiExp

# An independent integration of Figure 24, written from the review rather than
# from the engine: classical RK4 at a fixed step of h hours, each dose its own
# depot (its own ka), the zero-order release a box of rate, no step
# straddling a box edge.
referenceSuzetrigine <- function(wt, ht, sex, doses, times, h = 0.002,
                                 kenz = P$KENZ) {
  f <- wt / P$wtRef
  V2 <- P$V2P * f; V3 <- P$V3P * f; V4 <- P$V2M * f; V5 <- P$V3M * f
  CL <- clp(wt, ht, sex); CLM <- P$CLMrel * CL
  Fd <- function(d) P$F1 * (1 - P$fMax * d^P$fHill / (P$fD50^P$fHill + d^P$fHill))
  Kd <- function(d) P$KA * (1 - P$kMax * d / (P$kD50 + d))
  nd <- nrow(doses)
  ka <- vapply(doses$mg, Kd, 1)
  start <- doses$h + P$ALAG
  rate <- vapply(doses$mg, function(d) d * 1000 * Fd(d) / P$D1, 1)   # mcg/h
  deriv <- function(x, t) {
    dep <- x[seq_len(nd)]
    A2 <- x[nd + 1]; A3 <- x[nd + 2]; A4 <- x[nd + 3]; A5 <- x[nd + 4]
    E <- x[nd + 5]
    Cp <- A2 / V2; Cm <- A4 / V4
    inBox <- t >= start - 1e-9 & t < start + P$D1 - 1e-9
    absorbed <- ka * dep
    dA2 <- sum(absorbed) - CL * E * Cp - P$Q3P * (Cp - A3 / V3)
    c(rate * inBox - absorbed,
      dA2,
      P$Q3P * (Cp - A3 / V3),
      P$FR * CL * E * Cp - CLM * E * Cm - P$Q3M * (Cm - A5 / V5),
      P$Q3M * (Cm - A5 / V5),
      kenz * (1 + P$AMAX * Cp^P$AHILL / (P$AC50^P$AHILL + Cp^P$AHILL)) -
        kenz * E)
  }
  x <- c(rep(0, nd + 4), 1)
  t <- 0
  out <- matrix(NA_real_, length(times), 2)
  edges <- sort(unique(c(start, start + P$D1)))
  for (i in seq_along(times)) {
    while (t < times[i] - 1e-12) {
      # a step never straddles a box edge or passes the time asked for; the
      # box's rate is held over the step, evaluated at its middle
      nextEdge <- edges[edges > t + 1e-12][1]
      hs <- min(h, times[i] - t, if (is.na(nextEdge)) Inf else nextEdge - t)
      tm <- t + hs / 2
      k1 <- deriv(x, tm); k2 <- deriv(x + hs / 2 * k1, tm)
      k3 <- deriv(x + hs / 2 * k2, tm); k4 <- deriv(x + hs * k3, tm)
      x <- x + hs / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
      t <- t + hs
    }
    out[i, ] <- c(x[nd + 1] / V2, x[nd + 3] / V4)
  }
  out
}

simulate <- function(wt, ht, sex, doses, hours, ffm = FALSE) {
  dt <- data.frame(Drug = "suzetrigine", Time = doses$h * 60, Dose = doses$mg,
                   Units = "mg PO")
  simulateDrugsWithCovariates(dt, noEvents, wt, ht, 42, sex, hours * 60, FALSE,
                              adjustToFFM = ffm)
}

test_that("the parameters are Table 149's", {
  X <- suzetrigine(76.2, 165, 42, "female", adjustToFFM = FALSE)
  set <- X$PK$default
  bmi <- 76.2 / 1.65^2
  expect_equal(set$v1, 103)
  expect_equal(set$v2, 204)
  expect_equal(set$cl2 * 60, 19)
  expect_equal(set$cl1 * 60, 5.56 * (bmi / 27.8)^-0.605)
  expect_equal(set$ka_PO * 60, 2.02 * 2.24)
  expect_equal(set$tlag_PO, 0.182 * 60)
  expect_equal(set$bioavailability_PO, 0.88)
  mm <- X$michaelisMenten
  expect_equal(mm$vmax, 0)
  expect_equal(mm$autoinduction$cl, set$cl1)
  expect_equal(mm$autoinduction$kEnz * 60, 0.00939)
  expect_equal(c(mm$autoinduction$emax, mm$autoinduction$ec50, mm$autoinduction$hill),
               c(0.811, 432, 10))
  expect_equal(mm$zeroOrderDepot, 1.1 * 60)
  expect_equal(mm$metabolite$fraction, 0.8)
  expect_equal(mm$metabolite$cl, 0.273 * set$cl1)
  expect_equal(c(mm$metabolite$v1, mm$metabolite$v2, mm$metabolite$q * 60),
               c(1.7, 29.5, 8.75))
  expect_true(mm$metabolite$induced)
  # men clear 1.21 times faster
  male <- suzetrigine(76.2, 165, 42, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(male$cl1 / set$cl1, 1.21)
  # the metabolite's own model is the one the parent reads
  m6 <- M6SUZ(76.2, 165, 42, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(m6$cl1, mm$metabolite$cl)
  expect_equal(m6$v1, mm$metabolite$v1)
})

test_that("BMI is held within the range studied, 18.1-39.7", {
  cl <- function(wt) suzetrigine(wt, 170, 42, "female", FALSE)$PK$default$cl1
  expect_equal(cl(200), cl(39.7 * 1.7^2))
  expect_equal(cl(30), cl(18.1 * 1.7^2))
})

test_that("both curves agree with an independent integration of Figure 24", {
  # 100 mg, then 50 mg at 12 and 24 h (the label's regimen), to 96 h: the
  # enzyme rises by a third over these days, so the comparison covers the
  # induction, the dose-dependent ka (two depots) and the zero-order release
  doses <- data.frame(h = c(0, 12, 24, 36, 48), mg = c(100, 50, 50, 50, 50))
  wt <- 70; ht <- 165
  s <- simulate(wt, ht, "female", doses, 96)
  p <- s$suzetrigine$wide; m <- s$M6SUZ$wide
  # at points of the engine's own line (both moieties share it); the
  # reference's step divides every dose and box edge
  at <- c(1, 2, 3, 6, 12.5, 14, 24, 30, 48, 50, 60, 72, 96)
  i <- vapply(at, function(h) which.min(abs(p$Time - h * 60)), 1L)
  expect_equal(m$Time[i], p$Time[i])
  ref <- referenceSuzetrigine(wt, ht, "female", doses, p$Time[i] / 60)
  expect_equal(p$Plasma[i], ref[, 1], tolerance = 1e-4)
  expect_equal(m$Plasma[i], ref[, 2], tolerance = 1e-4)
})

test_that("Table 150: the applicant's simulation of 100 mg then 50 mg every 12 h", {
  # A woman at the reference BMI (the trials' patients were mostly women):
  # the means of Table 150 within 15%
  ht <- 163; wt <- 27.8 * 1.63^2
  dt <- data.frame(Drug = "suzetrigine", Time = c(0, 720), Dose = c(100, 50),
                   Units = c("mg PO", "mg PO bid"))
  s <- simulateDrugsWithCovariates(dt, noEvents, wt, ht, 42, "female", 16 * 1440,
                                   FALSE, adjustToFFM = FALSE)
  p <- s$suzetrigine$wide; m <- s$M6SUZ$wide
  auc <- function(w, a, b) {
    k <- w$Time >= a & w$Time <= b
    sum(diff(w$Time[k]) * (head(w$Plasma[k], -1) + tail(w$Plasma[k], -1)) / 2) / 60 / 1000
  }
  d14 <- 13 * 1440
  expect_equal(auc(p, 0, 2880), 15.8, tolerance = 0.15)            # ug.h/mL
  expect_equal(max(p$Plasma[p$Time <= 2880]) / 1000, 0.558, tolerance = 0.15)
  expect_equal(auc(p, d14, d14 + 1440), 11.5, tolerance = 0.15)
  expect_equal(max(p$Plasma[p$Time >= d14 & p$Time <= d14 + 1440]) / 1000, 0.623,
               tolerance = 0.15)
  expect_equal(auc(m, d14, d14 + 1440), 34.7, tolerance = 0.15)
  expect_equal(max(m$Plasma[m$Time >= d14 & m$Time <= d14 + 1440]) / 1000, 1.52,
               tolerance = 0.15)
  # the induction: the apparent clearance at day 14 is about twice the
  # single-dose one (Table 150: 7.01 and 13.9 L/h, means of individual ratios)
  expect_gt(auc(p, 0, 2880) / auc(p, d14, d14 + 1440), 1.3)
})

test_that("without the induction the curve would be the uninduced model's", {
  # Holding the enzyme at 1 (kEnz negligible) matches the same model with the
  # induction switched off in the reference integration; the induction lowers
  # the day-4 trough
  doses <- data.frame(h = c(0, 12, 24, 36, 48, 60, 72, 84), mg = c(100, rep(50, 7)))
  s <- simulate(70, 165, "female", doses, 96)
  p <- s$suzetrigine$wide
  ref0 <- referenceSuzetrigine(70, 165, "female", doses, 96, kenz = 1e-12)
  expect_lt(p$Plasma[which.min(abs(p$Time - 96 * 60))], ref0[1, 1])
})

test_that("the dose effects: F and ka fall with the dose", {
  X <- suzetrigine(70, 170, 42, "male")
  expect_equal(oralSaturationFraction(c(50, 100), X$oralSaturation),
               1 - 0.419 * c(50, 100)^5 / (162^5 + c(50, 100)^5))
  # 100 mg is absorbed more slowly than 50 mg: its peak comes later
  tmax <- function(mg) {
    w <- simulate(70, 170, "male", data.frame(h = 0, mg = mg), 24)$suzetrigine$wide
    w$Time[which.max(w$Plasma)]
  }
  expect_gt(tmax(100), tmax(50))
})

test_that("switch on: the volumes at the pharmacokinetic weight, BMI at total weight", {
  # 120 kg, 170 cm, 50 y man: pharmacokinetic weight 70 x 1.3049067
  set <- suzetrigine(120, 170, 50, "male")$PK$default
  expect_equal_rounded(set$v1, 103 * 70 * 1.3049067 / 76.2)
  # BMI 41.5, held at the studied 39.7
  expect_equal(set$cl1 * 60, clp(39.7 * 1.7^2, 170, "male"))
  off <- suzetrigine(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 103 * 120 / 76.2)
  expect_equal(off$cl1, set$cl1)
})

test_that("M6SUZ cannot be dosed; suzetrigine is oral only", {
  dd <- getDrugDefaultsGlobal()
  expect_true(all(is.na(unlist(dd$Units[dd$Drug == "M6SUZ"]))))
  expect_true(is.na(dd$Category[dd$Drug == "M6SUZ"]))
  expect_true(all(doseRoute(c("mg PO", "mg PO bid")) == ROUTE_PO))
})
