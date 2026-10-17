# lixisenatide: see R/drugs_lixisenatide.R (Frank 2013, Table 2).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
# creatinine giving the median Cockcroft-Gault clearance, 6.21 L/h, at 82.1 kg
scrMedian <- uniroot(function(s) creatinineClearanceCG(82.1, 55, "male", s) - 6.21 / 0.06,
                     c(0.3, 3), tol = 1e-12)$root

test_that("Table 2 at the medians: CL/F 38.6 L/h, V/F 41.2 L, MAT 3.25 h at 15 mcg", {
  pk <- lixisenatide(82.1, 175, 55, "male", FALSE, creatinine = scrMedian)$PK$default
  expect_equal(pk$cl1 * 60, 38.6, tolerance = 1e-8)
  expect_equal(pk$v1, 41.2)
  expect_equal(1 / (pk$ka_SC * 60), 3.25)
  # elimination time V/CL, 1.07 h in the paper
  expect_equal(round(pk$v1 / (pk$cl1 * 60), 2), 1.07)
})

test_that("covariates: Cockcroft-Gault on CL/F, weight on V/F, dose on MAT", {
  p <- lixisenatide(60, 160, 70, "female", FALSE, creatinine = 1.5)$PK$default
  clcr <- creatinineClearanceCG(60, 70, "female", 1.5) * 0.06
  expect_equal(p$cl1 * 60, 38.6 * (1 + 0.099 * (clcr - 6.21)))
  expect_equal(p$v1, 41.2 * (60 / 82.1)^1.56)
  # the paper: moderate impairment (1.8 to 3.0 L/h) raises exposure 1.5- to 1.8-fold
  fold <- 1 / (1 + 0.099 * (c(3.0, 1.8) - 6.21))
  expect_equal(round(fold, 1), c(1.5, 1.8))
  # dose, mg per injection: MAT (dose / 15 mcg)^0.236
  m20 <- lixisenatide(82.1, 175, 55, "male", FALSE, creatinine = scrMedian, dose = 0.020)$PK$default
  expect_equal(1 / (m20$ka_SC * 60), 3.25 * (20 / 15)^0.236)
  # switch on: the pharmacokinetic weight, in V/F and in Cockcroft-Gault
  size <- pkSizeFactors(120, 175, 50, "male", TRUE)
  on <- lixisenatide(120, 175, 50, "male", creatinine = 1)$PK$default
  expect_equal(on$v1, 41.2 * (size$pkWeight / 82.1)^1.56)
  expect_equal(on$cl1 * 60, 38.6 * (1 + 0.099 *
                 (creatinineClearanceCG(size$pkWeight, 50, "male", 1) * 0.06 - 6.21)))
  expect_equal_rounded(lixisenatide(70, 170, 40, "male")$PK$default$cl1 * 60, 39.6374516)
})

test_that("each injection size runs with its own absorption; exposure is dose / CL", {
  PK <- getDrugPK("lixisenatide", 82.1, 175, 55, "male", adjustToFFM = FALSE,
                  creatinine = scrMedian)
  expect_true(PK$doseDependent)
  run <- function(d) simCpCe(data.frame(Drug = "lixisenatide", Time = 0, Dose = d,
                                        Units = "mcg SC"), noEvents, PK, 1440, FALSE)$wide
  auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  w10 <- run(10); w20 <- run(20)
  expect_equal(auc(w20), 20000 / 38.6, tolerance = 0.01)      # pg.h/mL
  # slower absorption of the larger dose: the peak rises less than the dose
  expect_lt(max(w20$Plasma) / max(w10$Plasma), 2)
  # a 10 then a 20 mcg dose is the sum of two Bateman curves, each with its
  # own MAT, against the exact solution at every point
  both <- simCpCe(data.frame(Drug = "lixisenatide", Time = c(0, 720), Dose = c(10, 20),
                             Units = c("mcg SC", "mcg SC")), noEvents, PK, 1440, FALSE)$wide
  s <- PK$PK$default
  k <- s$cl1 / s$v1
  bateman <- function(t, mcg) {
    ka <- 1 / (3.25 * (mcg / 15)^0.236 * 60)
    ifelse(t > 0, mcg * 1000 * ka / (s$v1 * (ka - k)) * (exp(-k * t) - exp(-ka * t)), 0)
  }
  exact <- bateman(both$Time, 10) + bateman(both$Time - 720, 20)
  expect_equal(both$Plasma, exact, tolerance = 1e-6)
  expect_true(all(is.na(w20$`Effect Site`)))
})

test_that("units: subcutaneous micrograms, daily", {
  expect_equal(getDrugDefaults("lixisenatide")$Units[[1]], c("mcg SC", "mcg SC qd"))
  expect_true(all(c("creatinine", "dose") %in% names(formals(lixisenatide))))
})
