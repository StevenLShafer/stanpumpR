# Oxaliplatin (R/drugs_oxaliplatin.R): Bastian 2003, two compartments and a
# gallbladder store that fills until TLAG and then returns through the gut,
# as published (R/advanceBiliaryStore.R).  Drafted by Claude Code, 2026-11-06.

p <- OXALIPLATIN_BASTIAN
noEvents <- data.frame(Time = double(), Event = character())
SCR_REF <- 87 / SCR_UMOL_PER_MG_DL          # the cohort's median, in mg/dL
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
course <- function(hours, dose = 230, sex = "male", weight = 71, creatinine = SCR_REF,
                   maximum = 48 * 60, adjustToFFM = FALSE) {
  d <- data.frame(Drug = "oxaliplatinBastian", Time = c(0, hours * 60),
                  Dose = c(dose / hours, 0), Units = "mg/hr")
  simulateDrugsWithCovariates(d, noEvents, weight, 170, 59, sex, maximum, FALSE,
                              creatinine = creatinine,
                              adjustToFFM = adjustToFFM)$oxaliplatin
}
at <- function(w, hours) stats::approx(w$Time, w$Plasma, hours * 60)$y

# An independent fine-step fourth-order Runge-Kutta integration of Bastian's
# equations as published (store filling until each course's start + TLAG,
# then emptied into the gut), one system per course, summed.  Written from
# the paper, not from the engine.
rk4Bastian <- function(PK, courses, times, h = 0.2) {
  s <- PK$PK$default; b <- PK$biliaryStore
  out <- numeric(length(times))
  for (k in seq_len(nrow(courses))) {
    cs <- courses[k, ]
    sw <- cs$start + b$tlag
    rate <- function(t) if (t >= cs$start && t < cs$end) cs$rate else 0
    f <- function(t, x, filling) {
      kB <- if (filling) b$k1B else 0
      c(rate(t) - (s$k10 + s$k12 + kB) * x[1] + s$k21 * x[2] + b$ka * x[4],
        s$k12 * x[1] - s$k21 * x[2],
        kB * x[1],
        -b$ka * x[4])
    }
    x <- c(cs$bolus, 0, 0, 0)
    t <- cs$start
    tt <- sort(unique(round(c(seq(cs$start, max(times), by = h), sw, cs$end, times), 9)))
    tt <- tt[tt >= cs$start & tt <= max(times) & is.finite(tt)]
    vals <- numeric(length(tt)); vals[1] <- x[1]
    for (i in seq_along(tt)[-1]) {
      dt <- tt[i] - tt[i - 1]; t0 <- tt[i - 1]
      fill <- t0 < sw
      # rates switch only at grid points, so evaluate them at the step's start
      g <- function(t, x) f(t0, x, fill)
      k1 <- g(t0, x); k2 <- g(t0, x + dt / 2 * k1)
      k3 <- g(t0, x + dt / 2 * k2); k4 <- g(t0, x + dt * k3)
      x <- x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
      if (abs(tt[i] - sw) < 1e-9) { x[4] <- x[4] + x[3]; x[3] <- 0 }
      vals[i] <- x[1]
    }
    out <- out + ifelse(times < cs$start, 0, vals[match(times, tt)]) / s$v1
  }
  out
}

test_that("the model is Table 3 and the covariate equations", {
  PK <- getDrugPK("oxaliplatinBastian", 71, 170, 59, "male", adjustToFFM = FALSE,
                  creatinine = SCR_REF)
  s <- PK$PK$default
  expect_equal_rounded(s$v1, 24.9)
  expect_equal_rounded(s$v2, 136)
  expect_equal_rounded(s$cl1, 14.1 / 60)
  expect_equal_rounded(s$cl2, 34.8 / 60)
  expect_equal_rounded(PK$biliaryStore$k1B, 1.78 / 60)
  expect_equal_rounded(PK$biliaryStore$tlag, 120)
  expect_equal_rounded(PK$biliaryStore$ka, 0.23 / 60)
  # weight, creatinine and sex
  w <- 95; scr <- 150 / SCR_UMOL_PER_MG_DL
  s <- getDrugPK("oxaliplatinBastian", w, 170, 59, "female", adjustToFFM = FALSE,
                 creatinine = scr)$PK$default
  expect_equal_rounded(s$cl1, 14.1 * (w / 71)^1.10 * (150 / 87)^-0.57 * 0.60 / 60)
  expect_equal_rounded(s$v1, 24.9 * (w / 71)^1.29)
  expect_equal_rounded(s$cl2, 34.8 * (w / 71)^1.01 / 60)
  expect_equal_rounded(s$v2, 136)
})

test_that("the switch evaluates the weight terms at the pharmacokinetic weight", {
  on <- getDrugPK("oxaliplatinBastian", 110, 170, 59, "male", creatinine = SCR_REF)$PK$default
  pkw <- pkSizeFactors(110, 170, 59, "male", TRUE)$pkWeight
  expect_equal_rounded(on$v1, 24.9 * (pkw / 71)^1.29)
  expect_equal_rounded(on$cl1, 14.1 * (pkw / 71)^1.10 / 60)
  off <- getDrugPK("oxaliplatinBastian", 110, 170, 59, "male", adjustToFFM = FALSE,
                   creatinine = SCR_REF)$PK$default
  expect_equal_rounded(off$v1, 24.9 * (110 / 71)^1.29)
})

test_that("creatinine: blank is the assumed normal, and the floor is 53 umol/L", {
  blank <- getDrugPK("oxaliplatinBastian", 71, 170, 59, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(blank$cl1, 14.1 * (0.9 * SCR_UMOL_PER_MG_DL / 87)^-0.57 / 60)
  low <- getDrugPK("oxaliplatinBastian", 71, 170, 59, "male", adjustToFFM = FALSE,
                   creatinine = 0.2)$PK$default
  expect_equal_rounded(low$cl1, 14.1 * (53 / 87)^-0.57 / 60)
  expect_equal(OXALIPLATIN_MIN_SCR, 53)
})

test_that("the paper's derived numbers: half-lives, Vss and AUC = Dose/CL", {
  half <- function(sex) {
    s <- getDrugPK("oxaliplatinBastian", 71, 170, 59, sex, adjustToFFM = FALSE,
                   creatinine = SCR_REF)$PK$default
    k10 <- s$k10; k12 <- s$k12; k21 <- s$k21
    disc <- sqrt((k10 + k12 + k21)^2 - 4 * k10 * k21)
    log(2) / (((k10 + k12 + k21) - disc) / 2) / 60
  }
  # the terminal half-life is the two-compartment system's: 10.3 h and 15.5 h
  # (the paper prints 10.5 and 14.4, apparently posthoc means)
  expect_equal(half("male"), 10.3, tolerance = 0.005)
  expect_equal(half("female"), 15.5, tolerance = 0.005)
  expect_equal(24.9 + 136, 160.9)
  # the paper's example: 230 mg / median CL 11.3 L/h = 20.35 mg h/L
  expect_equal(230 / ((14.1 + 14.1 * 0.60) / 2), 20.35, tolerance = 0.002)
  # AUC(0-inf) = Dose / CL: every bit of stored drug returns
  w <- course(2, maximum = 14 * 24 * 60)$wide
  expect_equal(auc(w), 230 / 14.1, tolerance = 1e-4)
  w <- course(2, sex = "female", maximum = 21 * 24 * 60)$wide
  expect_equal(auc(w), 230 / (14.1 * 0.6), tolerance = 1e-4)
})

test_that("the typical man's curve after 230 mg over 2 h, and the 4-h artefact", {
  w <- course(2)$wide
  expect_equal(max(w$Plasma), 1.383, tolerance = 1e-3)
  expect_equal(w$Time[which.max(w$Plasma)], 120)
  expect_equal(at(w, c(2.5, 4, 6, 12, 24, 48)),
               c(1.026, 0.791, 0.697, 0.478, 0.216, 0.043), tolerance = 3e-3)
  expect_equal(auc(w), 15.67, tolerance = 1e-3)
  # the same dose over 4 h peaks higher: the store stops at 2 h
  w4 <- course(4)$wide
  expect_equal(max(w4$Plasma), 1.749, tolerance = 1e-3)
  expect_gt(max(w4$Plasma), max(w$Plasma))
})

test_that("Table 4: the covariates scale the area as BW^-1.10 SCr^0.57", {
  base <- auc(course(2, maximum = 14 * 24 * 60)$wide)
  heavy <- auc(course(2, weight = 98.2, maximum = 14 * 24 * 60)$wide)
  expect_equal(heavy / base, 0.70, tolerance = 2e-3)       # -30%
  renal <- auc(course(2, creatinine = 102.8 / SCR_UMOL_PER_MG_DL,
                      maximum = 14 * 24 * 60)$wide)
  expect_equal(renal / base, 1.10, tolerance = 2e-3)       # +10%
})

test_that("the engine matches an independent Runge-Kutta integration", {
  PK <- getDrugPK("oxaliplatinBastian", 80, 175, 59, "female", adjustToFFM = FALSE,
                  creatinine = 1.1)
  checks <- c(30, 90, 119, 120, 121, 150, 200, 240, 300, 360, 400, 600, 900,
              1440, 2000, 2880)
  # A 2-h infusion at 0, a bolus at 300 min, a second infusion from 400 min
  # whose rate changes at 450 (one course) and stops at 520.
  dose <- data.frame(
    Time = c(0, 120, 300, 400, 450, 520, checks),
    Dose = c(115 / 60, 0, 40, 100 / 60, 60 / 60, 0, rep(0, length(checks))),
    Units = c("mg/hr", "mg/hr", "mg", "mg/hr", "mg/hr", "mg/hr",
              rep("mg", length(checks))))
  eng <- advanceBiliaryStore(dose, PK, 2880)
  got <- eng$Cp[match(checks, eng$Time)]
  courses <- data.frame(start = c(0, 300, 400), end = c(120, 300, 520),
                        rate = c(115 / 60, 0, NA), bolus = c(0, 40, 0))
  # the second infusion's rate steps, so integrate it as two pieces of one
  # course: the same store and switch (at 400 + 120)
  ref <- rk4Bastian(PK, courses[1:2, ], checks)
  s <- PK$PK$default; b <- PK$biliaryStore
  third <- local({
    rate <- function(t) if (t >= 400 && t < 450) 100 / 60 else if (t >= 450 && t < 520) 1 else 0
    x <- c(0, 0, 0, 0); h <- 0.2
    tt <- sort(unique(round(c(seq(400, 2880, by = h), checks[checks >= 400]), 9)))
    v <- numeric(length(tt))
    for (i in seq_along(tt)[-1]) {
      t0 <- tt[i - 1]; dt <- tt[i] - t0; fill <- t0 < 520
      f <- function(x) { kB <- if (fill) b$k1B else 0
        c(rate(t0) - (s$k10 + s$k12 + kB) * x[1] + s$k21 * x[2] + b$ka * x[4],
          s$k12 * x[1] - s$k21 * x[2], kB * x[1], -b$ka * x[4]) }
      k1 <- f(x); k2 <- f(x + dt / 2 * k1); k3 <- f(x + dt / 2 * k2); k4 <- f(x + dt * k3)
      x <- x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
      if (abs(tt[i] - 520) < 1e-9) { x[4] <- x[4] + x[3]; x[3] <- 0 }
      v[i] <- x[1]
    }
    ifelse(checks < 400, 0, v[match(checks, tt)]) / s$v1
  })
  expect_equal(got, ref + third, tolerance = 1e-7)
})

test_that("courses superpose: the response to two courses is the sum of each", {
  # Two 1-h infusions, the second starting at 90 min, while the first course's
  # store is still filling (its switch is at 120).  A rate row replaces the
  # rate in force, as everywhere in the library, so the two infusions do not
  # overlap in time; their stores do.
  PK <- getDrugPK("oxaliplatinBastian", 71, 170, 59, "male", adjustToFFM = FALSE,
                  creatinine = SCR_REF)
  grid <- seq(0, 3000, by = 10)
  pad <- function(d) rbind(d, data.frame(Time = grid, Dose = 0, Units = "mg"))
  one <- function(start) data.frame(Time = c(start, start + 60),
                                    Dose = c(115 / 60, 0), Units = "mg/hr")
  both <- advanceBiliaryStore(pad(rbind(one(0), one(90))), PK, 3000)
  a <- advanceBiliaryStore(pad(one(0)), PK, 3000)
  b <- advanceBiliaryStore(pad(one(90)), PK, 3000)
  pick <- function(r) r$Cp[match(grid, r$Time)]
  expect_equal(pick(both), pick(a) + pick(b), tolerance = 1e-10)
  # and the second course is the first shifted by 90 minutes, its own store
  # emptying at 210
  expect_equal(pick(b)[grid >= 90], pick(a)[seq_len(sum(grid >= 90))],
               tolerance = 1e-9)
  expect_equal(biliaryStoreCourses(c(0, 60, 90, 150), c(1, 0, 1, 0),
                                   rep(FALSE, 4), rep(TRUE, 4))$start, c(0, 90))
})

test_that("a course is a bolus or an infusion episode", {
  cs <- biliaryStoreCourses(c(0, 30, 60, 100, 200, 300), c(2, 3, 0, 5, 1, 0),
                            c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                            c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(cs$start, c(0, 100, 200))
  expect_equal(cs$end, c(60, 100, 300))
  expect_equal(cs$bolus, c(0, 5, 0))
})

test_that("the store and gut are the depot; nothing is stored after TLAG", {
  o <- course(2)
  comp <- o$compartments
  expect_true(all(c("V2", "Depot") %in% names(comp)))
  # the depot only empties after the switch: maximal at 120 min
  expect_equal(comp$Time[which.max(comp$Depot)], 120)
})

test_that("the time until threshold is exact after the last switch, NA before", {
  PK <- getDrugPK("oxaliplatinBastian", 71, 170, 59, "male", adjustToFFM = FALSE,
                  creatinine = SCR_REF)
  dose <- data.frame(Time = c(0, 120, 360), Dose = c(115 / 60, 0, 0),
                     Units = c("mg/hr", "mg/hr", "mg"))
  r <- advanceBiliaryStore(dose, PK, 4000, plotRecovery = TRUE, emerge = 0.3)
  expect_true(all(is.na(r$Recovery[r$Time > 0 & r$Time < 120])))
  # from 6 h, the time to fall below 0.3 for good, read off the curve itself
  last <- max(which(r$Cp > 0.3))
  cross <- stats::approx(r$Cp[last:(last + 1)], r$Time[last:(last + 1)], 0.3)$y
  expect_equal(r$Recovery[r$Time == 360], cross - 360, tolerance = 1e-3)
  # no threshold: zeros
  r0 <- advanceBiliaryStore(dose, PK, 4000, plotRecovery = TRUE, emerge = 0)
  expect_true(all(r0$Recovery == 0))
})

test_that("getDrugPK and the engine refuse what the store cannot carry", {
  expect_error(validateBiliaryStore(list(k1B = 0, tlag = 1, ka = 1), "x"),
               "Invalid biliaryStore")
  PK <- getDrugPK("oxaliplatinBastian", 71, 170, 59, "male")
  expect_error(advanceBiliaryStore(
    data.frame(Time = 0, Dose = 10, Units = "mg PO"), PK, 600),
    "intravenous doses only")
})

test_that("oxaliplatin is one name for Bastian 2003 and Nikanjam 2015, chosen by stanpumpR", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "oxaliplatin", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  bastian <- dd[dd$Drug == "oxaliplatinBastian", ]
  expect_true(is.na(bastian$Category) || bastian$Category == "")
  # the larger study (Nikanjam, 186 patients, 0.6-86 years) covers adults
  expect_match(getDrugPK("oxaliplatin", 70, 170, 60, "male")$reference, "Nikanjam")
  # Bastian only beyond Nikanjam's 86 years
  expect_match(getDrugPK("oxaliplatin", 70, 170, 88, "male")$reference, "Bastian G")
  expect_equal(getDrugPK("oxaliplatinBastian", 70, 170, 60, "male")$tPeak, 0)
  expect_identical(therapeuticRange("oxaliplatin")$band_status, "not_applicable")
})
