# Acenocoumarol: Verstuyft 2012 apparent two-compartment S-enantiomer PK (half
# of the racemic dose) and the indirect-response INR model.  See the header of
# R/drugs_acenocoumarol.R and R/inr.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters at the 69 kg reference subject", {
  # Switch off reproduces the published values at the reference weight.
  actual <- acenocoumarol(69, 170, 40, "male", cyp2c9 = "*1/*1", vkorc1 = "AA",
                          adjustToFFM = FALSE)
  expect_equal_rounded(actual$PK$default, list(
    v1 = 24.7, v2 = 52.5, v3 = 1,
    cl1 = 15.3 / 60, cl2 = 4.85 / 60, cl3 = 0,
    ka_PO = 0.936 / 60, tlag_PO = 0.269 * 60, bioavailability_PO = 1
  ))
  expect_equal_rounded(actual$inr, list(
    type = "indirect", c50 = 0.0023, gamma = 1.2,
    kin = 0.0269 / 60, kout = 0.0268 / 60))
  expect_equal(actual$doseFraction, 0.5)   # S gets half the racemate
  expect_null(actual$parallelSystems)
  expect_equal(actual$MEAC, 0)
})

test_that("CYP2C9 *3 carriage lowers clearance; the rest act as wild type", {
  base <- acenocoumarol(69, 170, 40, "male", cyp2c9 = "*1/*1", vkorc1 = "AA",
                        adjustToFFM = FALSE)
  for (g in c("*1/*1", "*1/*2", "*2/*2"))
    expect_equal(acenocoumarol(69, 170, 40, "male", cyp2c9 = g,
                               adjustToFFM = FALSE)$PK$default$cl1,
                 base$PK$default$cl1, info = g)
  for (g in c("*1/*3", "*2/*3", "*3/*3"))
    expect_equal(acenocoumarol(69, 170, 40, "male", cyp2c9 = g,
                               adjustToFFM = FALSE)$PK$default$cl1,
                 15.3 * (1 - 0.3) / 60, tolerance = 1e-9, info = g)
})

test_that("VKORC1 raises the C50 for GG or GA; AA is the sensitive reference", {
  expect_equal(acenocoumarol(69, 170, 40, "male", vkorc1 = "AA")$inr$c50, 0.0023)
  expect_equal(acenocoumarol(69, 170, 40, "male", vkorc1 = "GG")$inr$c50,
               0.0023 * (1 + 1.29))
  expect_equal(acenocoumarol(69, 170, 40, "male", vkorc1 = "GA")$inr$c50,
               0.0023 * (1 + 1.29))
})

test_that("VKORC1 defaults to AA, the sensitive reference", {
  # Shafer, 2026-10-28: an unentered genotype should predict the LARGER INR
  # rise, so a dose read from the curve errs toward underdosing (see
  # VKORC1_DEFAULT in R/constants.R).  Here that is the source's own
  # sensitive reference, the base C50.
  expect_identical(VKORC1_DEFAULT, "AA")
  expect_equal(acenocoumarol(69, 170, 40, "male")$inr$c50, 0.0023)
  expect_lt(acenocoumarol(69, 170, 40, "male")$inr$c50,
            acenocoumarol(69, 170, 40, "male", vkorc1 = "GG")$inr$c50)
})

test_that("the switch off scales V1 by total body weight; CL, Q, V2 published", {
  m <- acenocoumarol(90, 180, 40, "male", adjustToFFM = FALSE)
  expect_equal(m$PK$default$v1, 24.7 * (90 / 69), tolerance = 1e-9)
  expect_equal(m$PK$default$cl1, 15.3 / 60, tolerance = 1e-9)   # size-free CL
  expect_equal(m$PK$default$cl2, 4.85 / 60, tolerance = 1e-9)   # size-free Q
  expect_equal(m$PK$default$v2, 52.5, tolerance = 1e-9)         # size-free V2
})

test_that("the switch on uses the fat-free-mass weight and library factors", {
  # 120 kg, 170 cm, 50 y man, switch on: fat-free-mass factors 1.3049067
  # (volumes) and 1.2209126 (clearances), pharmacokinetic weight 91.343469 kg
  # (the factors pinned in test-drugs-alfentanil.R).
  # CL, Q and V2 are anchored at Verstuyft's cohort (A2-F4, 2026-11-19), so
  # their factors are divided by the cohort's.
  pkW <- 91.343469; vol <- 1.3049067; clr <- 1.2209126
  kc <- cohortFactorsIndependent(VERSTUYFT_2012_COHORT)
  m <- acenocoumarol(120, 170, 50, "male")
  expect_equal(m$PK$default$v1, 24.7 * (pkW / 69), tolerance = 1e-6)  # own covariate
  expect_equal(m$PK$default$cl1, 15.3 * clr / kc$clearance / 60, tolerance = 1e-6)  # size-free CL
  expect_equal(m$PK$default$cl2, 4.85 * clr / kc$clearance / 60, tolerance = 1e-6)  # size-free Q
  expect_equal(m$PK$default$v2, 52.5 * vol / kc$volume, tolerance = 1e-6)           # size-free V2
})

test_that("invalid genotypes are rejected", {
  expect_error(acenocoumarol(69, 170, 40, "male", cyp2c9 = "*9/*9"), "cyp2c9")
  expect_error(acenocoumarol(69, 170, 40, "male", vkorc1 = "ZZ"), "vkorc1")
})

test_that("the simulated INR starts near 1, rises, and has no effect site", {
  dose <- data.frame(Drug = "acenocoumarol", Time = 0, Dose = 4,
                     Units = "mg PO qd", stringsAsFactors = FALSE)
  X <- simulateDrugsWithCovariates(dose, noEvents, 69, 170, 40, "male",
                                   maximum = 10 * 1440, plotRecovery = FALSE,
                                   cyp2c9 = "*1/*1", vkorc1 = "AA")
  r <- X$acenocoumarol$results
  p <- r[r$Site == "Plasma", ]
  expect_equal(p$Y[1], 0.0268 / 0.0269, tolerance = 1e-6)   # kout/kin baseline
  expect_gt(max(p$Y), 1.5)                                  # it climbs
  ce <- r[r$Site == "Effect Site", ]
  expect_true(all(is.na(ce$Y)))                             # no effect site
})

test_that("the steady-state INR follows Table IV's covariate directions", {
  endOf <- function(cyp, vk) {
    dose <- data.frame(Drug = "acenocoumarol", Time = 0, Dose = 4,
                       Units = "mg PO qd", stringsAsFactors = FALSE)
    X <- simulateDrugsWithCovariates(dose, noEvents, 69, 170, 40, "male",
                                     maximum = 10 * 1440, plotRecovery = FALSE,
                                     cyp2c9 = cyp, vkorc1 = vk)
    p <- X$acenocoumarol$results
    max(p$Y[p$Site == "Plasma"])
  }
  # *3 carrier > wild type; the sensitive AA > the less-sensitive GG.
  expect_gt(endOf("*1/*3", "AA"), endOf("*1/*1", "AA"))
  expect_gt(endOf("*1/*3", "AA"), endOf("*1/*3", "GG"))
})

test_that("anchored at Verstuyft's cohort: its typical subject receives the published CL, Q and V2 (A2-F4)", {
  # The cohort is fluindione's (VERSTUYFT_2012_COHORT, R/drugs_acenocoumarol.R):
  # the same 24 subjects took both drugs.  CL, Q and V2 have no sex term and
  # are anchored at the pooled subject: the 24 men and women, each at their
  # own CYP2C9 group's values, receive the published values on the geometric
  # mean (the wild-type CL; the *3 factor is a genotype term).
  subjects <- list(list(71.4, 23.6, 28.5, "male", 10), list(71.4, 23.6, 28.5, "female", 1),
                   list(67.8, 22.4, 26.8, "male", 9), list(67.8, 22.4, 26.8, "female", 4))
  l <- c(cl1 = 0, cl2 = 0, v2 = 0)
  for (s in subjects) {
    x <- acenocoumarol(s[[1]], 100 * sqrt(s[[1]] / s[[2]]), s[[3]], s[[4]],
                       cyp2c9 = "*1/*1")$PK$default
    for (p in names(l)) l[[p]] <- l[[p]] + s[[5]] * log(x[[p]])
  }
  expect_equal(exp(l[["cl1"]] / 24) * 60, 15.3, tolerance = 1e-9)
  expect_equal(exp(l[["cl2"]] / 24) * 60, 4.85, tolerance = 1e-9)
  expect_equal(exp(l[["v2"]] / 24), 52.5, tolerance = 1e-9)
  # V1 keeps its own weight term at the pharmacokinetic weight
  h <- 100 * sqrt(67.8 / 22.4)
  s <- pkSizeFactors(67.8, h, 26.8, "female")
  expect_equal(acenocoumarol(67.8, h, 26.8, "female")$PK$default$v1, 24.7 * s$pkWeight / 69,
               tolerance = 1e-9)
  # The *3 factor still multiplies the anchored CL
  kc <- cohortFactorsIndependent(VERSTUYFT_2012_COHORT)
  expect_equal(acenocoumarol(67.8, h, 26.8, "female", cyp2c9 = "*1/*3")$PK$default$cl1 * 60,
               15.3 * 0.7 * s$clearance / kc$clearance, tolerance = 1e-6)
  # The switch off is unchanged
  off <- acenocoumarol(67.8, h, 26.8, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$cl1 * 60, 15.3); expect_equal(off$cl2 * 60, 4.85); expect_equal(off$v2, 52.5)
})
