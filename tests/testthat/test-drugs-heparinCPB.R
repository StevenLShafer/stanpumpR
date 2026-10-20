# Heparin on cardiopulmonary bypass: Delavenne 2017 two-compartment anti-factor
# Xa model whose plotted output is the activated clotting time (ACT), by a
# direct Emax link.  See the headers of R/drugs_heparinCPB.R and R/act.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# The Emax link, in IU/mL, as the paper fitted it.
actOf <- function(antiXa) 116 + 720 * antiXa / (3.49 + antiXa)

test_that("returns the published parameters and ACT block with the switch off", {
  actual <- heparinCPB(70, 170, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 3.1, v2 = 2.23, v3 = 1,
      cl1 = 0.841 / 60, cl2 = 4.67 / 60, cl3 = 0
    )),
    act = list(act0 = 116, emax = 720, c50 = 3.49),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("body weight scales Vc linearly and CL to the 0.767 power", {
  a <- heparinCPB(70, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  b <- heparinCPB(140, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(b$v1 / a$v1, 2, tolerance = 1e-9)             # (140/70)^1
  expect_equal(b$cl1 / a$cl1, 2^0.767, tolerance = 1e-9)     # (140/70)^0.767
  # Vp and Q have no weight term with the switch off.
  expect_equal(b$v2, a$v2); expect_equal(b$cl2, a$cl2)
})

test_that("the plotted curve is the ACT in seconds from the anti-Xa activity", {
  # A 300 IU/kg bolus into Vc 3.1 L (reference man) is 300*70/3.1/1000 = 6.774
  # IU/mL at time 0, an ACT of 116 + 720*6.774/(3.49+6.774) = 591 s.
  dose <- data.frame(Drug = "heparinCPB", Time = 0, Dose = 300, Units = "IU/kg")
  r <- simulateDrugsWithCovariates(dose, noEvents, 70, 170, 50, "male",
                                   240, TRUE, adjustToFFM = FALSE)$heparinCPB
  pl <- r$results[r$results$Site == "Plasma", ]
  antiXa0 <- 300 * 70 / 3.1 / 1000
  expect_equal(pl$Y[1], actOf(antiXa0), tolerance = 1e-6)
  expect_equal(pl$Y[1], 591, tolerance = 1)
  # The ACT falls toward the 116 s baseline as heparin is cleared and never
  # drops below it (the Emax term is non-negative).
  expect_true(all(pl$Y >= 116 - 1e-9))
  expect_true(all(diff(pl$Y) <= 1e-9))                       # monotone decreasing
  expect_lt(pl$Y[nrow(pl)], pl$Y[1])
})

test_that("a larger bolus gives a higher ACT, saturating toward ACT0 + Emax", {
  big <- simulateDrugsWithCovariates(
    data.frame(Drug = "heparinCPB", Time = 0, Dose = 5000, Units = "IU/kg"),
    noEvents, 70, 170, 50, "male", 1, TRUE, adjustToFFM = FALSE)$heparinCPB
  pl <- big$results[big$results$Site == "Plasma", ]
  # 5000 IU/kg (about 113 IU/mL) is far above C50, so the ACT is near the
  # maximum 116 + 720 = 836 s (it reaches 814 s here).
  expect_gt(pl$Y[1], 800)
  expect_lt(pl$Y[1], 836)
})

test_that("it is dosed in international units, like heparin", {
  expect_error(
    simulateDrugsWithCovariates(
      data.frame(Drug = "heparinCPB", Time = 0, Dose = 50, Units = "mg"),
      noEvents, 70, 170, 50, "male", 120, TRUE),
    "international units")
})

test_that("the plotted output has no effect site", {
  r <- getDrugPK("heparinCPB", 70, 170, 50, "male")
  expect_equal(r$tPeak, 0)
  expect_equal(r$MEAC, 0)
  expect_false(is.null(r$act))
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: fat-free-mass factors 1.3049067 (volumes) and
  # 1.2209126 (clearances), pharmacokinetic weight 91.343469 kg (pinned in
  # test-drugs-alfentanil.R).  Vc and CL use the pharmacokinetic weight in their
  # own covariate; Vp and Q take the fat-free-mass factors.
  pkW <- 91.343469; vol <- 1.3049067; clr <- 1.2209126
  actual <- heparinCPB(120, 170, 50, "male")$PK$default
  expect_equal_rounded(actual[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 3.1 * (pkW / 70),
                            v2 = 2.23 * vol,
                            cl1 = 0.841 * (pkW / 70)^0.767 / 60,
                            cl2 = 4.67 / 60 * clr))
})
