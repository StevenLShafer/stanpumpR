# Idarucizumab (R/drugs_idarucizumab.R): derived from Glund 2017's group
# means.  Drafted by Claude Code, 2026-11-28.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Glund 2017 Tables 1 and 2: per group n, CL (mL/min), Vss (L), initial and
# terminal half-lives (h), mean Cockcroft-Gault CrCl (mL/min)
glund <- data.frame(
  n    = c(6, 6, 8, 8, 6, 6, 6),
  cl   = c(39.3, 47.1, 40.7, 39.6, 32.7, 32.8, 25.7),
  vss  = c(6.61, 8.86, 6.39, 8.03, 6.70, 6.34, 7.00),
  ta   = c(0.80, 0.78, 0.89, 0.91, 0.90, 0.93, 1.16),
  tb   = c(8.34, 10.3, 5.84, 10.8, 9.83, 9.52, 10.1),
  crcl = c(97.5, 110.4, 84.8, 83.3, 79.9, 72.8, 58.7)
)

test_that("the derived parameters follow from Glund 2017's Table 2", {
  each <- t(mapply(function(cl, vss, ta, tb)
    unlist(twoCompartmentFromHalfLives(cl / 1000, vss, ta * 60, tb * 60)),
    glund$cl, glund$vss, glund$ta, glund$tb))
  w <- glund$n / sum(glund$n)
  expect_equal(round(exp(sum(w * log(each[, "v1"]))), 3), IDARUCIZUMAB_V1)
  expect_equal(round(exp(sum(w * log(each[, "v2"]))), 3), IDARUCIZUMAB_V2)
  expect_equal(round(exp(sum(w * log(each[, "cl2"]))) * 1000, 3), IDARUCIZUMAB_Q)
  fit <- coef(lm(cl ~ crcl, glund))
  expect_equal(round(unname(fit), c(2, 3)), c(IDARUCIZUMAB_CL0, IDARUCIZUMAB_CL_CRCL))
})

test_that("the groups' initial half-lives are reproduced at their CrCl", {
  ta <- vapply(glund$crcl, function(crcl) {
    k10 <- (IDARUCIZUMAB_CL0 + IDARUCIZUMAB_CL_CRCL * crcl) / 1000 / IDARUCIZUMAB_V1
    k12 <- IDARUCIZUMAB_Q / 1000 / IDARUCIZUMAB_V1
    k21 <- IDARUCIZUMAB_Q / 1000 / IDARUCIZUMAB_V2
    s <- k10 + k12 + k21
    log(2) / ((s + sqrt(s^2 - 4 * k10 * k21)) / 2) / 60
  }, numeric(1))
  expect_true(all(abs(ta - glund$ta) < 0.1))
})

test_that("clearance is linear in the Cockcroft-Gault CrCl", {
  cl <- function(scr) idarucizumab(70, 170, 60, "male", adjustToFFM = FALSE,
                                   creatinine = scr)$PK$default$cl1 * 1000
  crcl <- function(scr) (140 - 60) * 70 / (72 * scr)
  expect_equal(cl(1.2), IDARUCIZUMAB_CL0 + IDARUCIZUMAB_CL_CRCL * crcl(1.2))
  expect_equal(cl(3) - cl(6), IDARUCIZUMAB_CL_CRCL * (crcl(3) - crcl(6)))
  # blank creatinine: the library's assumed 0.9 mg/dL
  expect_equal(idarucizumab(70, 170, 60, "male", adjustToFFM = FALSE)$PK$default$cl1,
               cl(0.9) / 1000)
})

test_that("with the switch off the volumes, Q and the intercept are fixed", {
  p <- idarucizumab(90, 160, 70, "female", adjustToFFM = FALSE, creatinine = 1)$PK$default
  expect_equal(p$v1, IDARUCIZUMAB_V1)
  expect_equal(p$v2, IDARUCIZUMAB_V2)
  expect_equal(p$cl2 * 1000, IDARUCIZUMAB_Q)
  crcl <- (140 - 70) * 90 / (72 * 1) * 0.85
  expect_equal(p$cl1 * 1000, IDARUCIZUMAB_CL0 + IDARUCIZUMAB_CL_CRCL * crcl)
})

test_that("Glund's size-free parameters are anchored at its 46 subjects", {
  cohort <- cohortFactorsIndependent(IDARUCIZUMAB_GLUND_COHORT)
  size <- function(w, h, a, s) pkSizeFactors(w, h, a, s, TRUE)
  p <- idarucizumab(70, 170, 35, "male", creatinine = 0.9)$PK$default
  ref <- size(70, 170, 35, "male")
  expect_equal(p$v1, IDARUCIZUMAB_V1 * ref$volume / cohort$volume, tolerance = 1e-6)
  expect_equal(p$cl2 * 1000, IDARUCIZUMAB_Q * ref$clearance / cohort$clearance,
               tolerance = 1e-6)
})

test_that("5 g over 5 minutes peaks near Glund's 25,000 nmol/L and is 2% of it by 6 h", {
  dt <- data.frame(Drug = "idarucizumab", Time = c(0, 5), Dose = c(60, 0), Units = "g/hr")
  # the 5 g middle-aged group: 76.0 kg, BMI 24.35 (176.7 cm), 50.0 years
  o <- simulateDrugsWithCovariates(dt, noEvents, 76, 176.7, 50, "male", 1440, FALSE)
  w <- o$idarucizumab$wide
  cmax <- max(w$Plasma)
  expect_equal(round(cmax * 1e3 / 47.766), 26381, tolerance = 0.002)
  expect_lt(abs(cmax * 1e3 / 47.766 / 25000 - 1), 0.1)
  at6 <- approx(w$Time, w$Plasma, 360)$y / cmax
  expect_true(at6 > 0.01 && at6 < 0.03)
})

test_that("pregnancy raises the creatinine clearance by 1.5 only with a blank creatinine", {
  cl <- function(...) idarucizumab(70, 165, 30, "female", adjustToFFM = FALSE, ...)$PK$default$cl1 * 1000
  expect_equal(cl(pregnant = TRUE) - IDARUCIZUMAB_CL0, 1.5 * (cl() - IDARUCIZUMAB_CL0))
  expect_equal(cl(creatinine = 0.6, pregnant = TRUE), cl(creatinine = 0.6))
})

test_that("idarucizumab is a reversal agent, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "idarucizumab", ]
  expect_identical(row$Category, "Hemostasis and reversal")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("idarucizumab", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Glund 2017")
  expect_match(PK$reference, "not printed in the paper")
  expect_identical(therapeuticRange("idarucizumab")$band_status, "not_applicable")
})

test_that("the line is within 16% of Glund 2019's patients' AUC 0-24 h", {
  # Glund 2019 Table 1, 5 g in RE-VERSE AD, by Cockcroft-Gault category;
  # evaluated at an assumed category CrCl of 99, 65, 40 and 21 mL/min
  auc24 <- function(crcl) {
    scr <- (140 - 78) * 70 / (72 * crcl)
    p <- idarucizumab(70, 170, 78, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
    k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
    A <- matrix(c(-(k10 + k12), k21, k12, -k21), 2, byrow = TRUE)
    e <- eigen(A)
    dose <- 5 / 47766 * 1e9                              # nmol
    x0 <- solve(e$vectors, c(dose, 0))
    # integral over 0-24 h of the central amount, analytically, per V1, in h
    amt <- e$vectors %*% (x0 * (exp(e$values * 1440) - 1) / e$values)
    amt[1] / p$v1 / 60
  }
  measured <- c(47100, 65000, 89700, 116000)
  model <- vapply(c(99, 65, 40, 21), auc24, numeric(1))
  expect_true(all(abs(model / measured - 1) < 0.16))
  expect_equal(round(model / measured, 2), c(0.85, 0.88, 0.93, 1.08))
})
