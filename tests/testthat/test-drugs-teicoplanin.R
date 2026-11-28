# Teicoplanin (R/drugs_teicoplaninKim.R, R/drugs_teicoplaninZhang.R, the
# group in R/modelSelection.R).  Drafted by Claude Code, 2026-11-27.  The
# expectations are the papers' equations, computed here, not read from the
# drug files.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))

ckdepi2021 <- function(age, female, scr) {
  k <- if (female) 0.7 else 0.9
  a <- if (female) -0.241 else -0.302
  142 * min(scr / k, 1)^a * max(scr / k, 1)^-1.200 * 0.9938^age * (if (female) 1.012 else 1)
}
duBois <- function(w, h) 0.007184 * w^0.425 * h^0.725

test_that("Kim 2024's equations, switch off", {
  set <- teicoplaninKim(90, 180, 40, "male", adjustToFFM = FALSE, creatinine = 1.1)$PK$default
  ce <- ckdepi2021(40, FALSE, 1.1) * duBois(90, 180) / 1.73
  expect_equal(set$cl1 * 60, 0.693 * (ce / 105.27)^0.785)
  expect_equal(set$v1, 3.96)
  expect_equal(set$v2, 8.24)
  expect_equal(set$v3, 69.8 * (90 / 67.85)^1.73)
  expect_equal(set$cl2 * 60, 4.45)
  expect_equal(set$cl3 * 60, 1.76)
  # at the typical CE and weight, Table 2's values
  set <- teicoplaninKim(67.85, 164, 32, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(set$v3, 69.8)
})

test_that("Kim with the switch on: the cohort anchor and the pharmacokinetic weight", {
  size <- pkSizeFactors(100, 170, 50, "female", TRUE)
  cohort <- cohortFactorsIndependent(list(weight = 67.9, age = 32, men = 6, women = 6,
                                          height = 164))
  on <- teicoplaninKim(100, 170, 50, "female", creatinine = 0.8)$PK$default
  ce <- ckdepi2021(50, TRUE, 0.8) * duBois(100, 170) / 1.73
  expect_equal(on$cl1 * 60, 0.693 * (ce / 105.27)^0.785)   # no size factor
  expect_equal(on$v1, 3.96 * size$volume / cohort$volume)
  expect_equal(on$cl2 * 60, 4.45 * size$clearance / cohort$clearance)
  expect_equal(on$v3, 69.8 * (size$pkWeight / 67.85)^1.73)
  # a patient like the cohort's receives the published values
  like <- teicoplaninKim(67.9, 164, 32, "male")$PK$default
  expect_lt(abs(like$v1 / 3.96 - 1), 0.15)  # the man, against a cohort half women
  expect_equal_rounded(0.76545, teicoplaninKim(70, 170, 35, "male")$PK$default$cl1 * 60)
})

test_that("Kim: late pregnancy raises the eGFR when the creatinine is blank", {
  base <- teicoplaninKim(70, 165, 30, "female")$PK$default
  preg <- teicoplaninKim(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal(preg$cl1 / base$cl1, 1.5^0.785)
  measured <- teicoplaninKim(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default
  expect_equal(measured,
               teicoplaninKim(70, 165, 30, "female", creatinine = 0.6)$PK$default)
})

test_that("Zhang 2020's equations, and its own check values", {
  scr <- 29.075 / 88.4
  set <- teicoplaninZhang(16.71, 100, 4, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(set$cl1 * 60, 0.0694 * (1 + 2.82) * 0.882)
  expect_equal(set$v1, 1.39 * 1.75)
  expect_equal(round(set$cl1 * 60 / 16.71, 3), 0.014)   # the paper's 0.014 L/h/kg
  expect_equal(round(set$v1 / 16.71, 2), 0.15)           # and 0.15 L/kg
  expect_equal(set$cl2, 0)
  set <- teicoplaninZhang(30, 130, 9, "female", adjustToFFM = FALSE, creatinine = 0.5)$PK$default
  expect_equal(set$cl1 * 60, 0.0694 * (1 + 2.82 * 30 / 16.71) * 0.882^(0.5 * 88.4 / 29.075))
  expect_equal(set$v1, 1.39 * 1.75^(30 / 16.71))
})

test_that("the program chooses Kim for adults and Zhang for children", {
  chosen <- function(age, weight) teicoplanin(weight, 170, age, "male")$modelChosen
  expect_equal(chosen(35, 70), "teicoplaninKim")
  expect_equal(chosen(80, 70), "teicoplaninKim")    # older than Kim's oldest: nearest
  expect_equal(chosen(5, 18), "teicoplaninZhang")
  expect_equal(chosen(0.05, 3.5), "teicoplaninZhang")
  expect_equal(chosen(16, 60), "teicoplaninZhang")  # nearer Zhang's 14 than Kim's 19
  expect_equal(teicoplanin(70, 170, 35, "male", creatinine = 2)$PK,
               teicoplaninKim(70, 170, 35, "male", creatinine = 2)$PK)
})

test_that("teicoplanin simulates, offers intravenous units and its trough band", {
  dt <- data.frame(Drug = "teicoplanin", Time = c(0, 30), Dose = c(800, 0),
                   Units = "mg/hr")
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 35, "male", 72 * 60, FALSE)$teicoplanin$wide
  expect_gt(max(w$Plasma), 0)
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "teicoplanin"])
  expect_true(all(c("mg", "mg qd", "mg/kg", "mg/hr") %in% units))
  row <- dd[dd$Drug == "teicoplanin", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper), c(15, 22.5, 30))
  expect_equal(row$endCe, 20)
  X <- teicoplanin(70, 170, 35, "male")
  expect_equal(c(X$lowerTypical, X$typical, X$upperTypical), c(15, 22.5, 30))
  expect_true(antibioticBandIsTrough("teicoplanin"))
  for (d in c("teicoplaninKim", "teicoplaninZhang"))
    expect_true(is.na(dd$Category[dd$Drug == d]) || !nzchar(dd$Category[dd$Drug == d]))
})
