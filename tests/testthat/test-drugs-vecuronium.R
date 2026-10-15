test_that("returns the published Vega-Villa parameters with the switch off", {
  # The switch off uses the absolute published values, unscaled.  Age 40 is
  # the top of Rupp's younger group, so no age factor applies.
  actual <- vecuronium(70, 171, 40, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 10.25,
        v2 = 9.7,
        v3 = 43.79,
        cl1 = 0.51,
        cl2 = 0.66,
        cl3 = 0.095
      )
    ),
    tPeak = 5.37,
    MEAC = 0,
    typical = 135,
    upperTypical = 155,
    lowerTypical = 110,
    reference = paste(
      "Vega-Villa KR et al., Br J Anaesth 2014;112(2):304-310 (non-burn",
      "reference values). https://pubmed.ncbi.nlm.nih.gov/24067332/ Effect",
      "site and potency from Rupp SM, Fisher DM et al., Anesthesiology",
      "1987;67(1):45-49. https://pubmed.ncbi.nlm.nih.gov/2886080/"
    )
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 40 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the published values by hand).  The Al-Sallami
  # maturation term is 1 for any adult, so these are the age-50 factors
  # too; age 40 carries no age factor of its own.
  actual <- vecuronium(120, 170, 40, "male")
  expected <- list(
    v1 = 13.3752937,
    v2 = 12.657595,
    v3 = 57.1418644,
    cl1 = 0.62266543,
    cl2 = 0.80580232,
    cl3 = 0.1159867
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the band and threshold match the CSV and Rupp's potency", {
  dd <- getDrugDefaults("vecuronium")
  ref <- vecuronium(70, 170, 40, "male")
  expect_equal(dd$Lower, ref$lowerTypical)
  expect_equal(dd$Upper, ref$upperTypical)
  expect_equal(dd$Typical, ref$typical)
  # Rupp 1987: Cpss50 92 ng/mL, gamma 5.80.  T1 25% recovery = 75% block.
  expect_equal(dd$endCe, round(92 * 3^(1 / 5.8), -1))
})

test_that("getDrugPK turns the tPeak back into a usable ke0", {
  # Rupp fitted ke0 0.169/min with his own kinetics; against the
  # Vega-Villa disposition the same 5.37 min peak needs 0.300/min.
  pk <- getDrugPK("vecuronium", 70, 170, 40, "male",
                  getDrugDefaults("vecuronium"), adjustToFFM = FALSE)
  expect_equal(pk$PK[[1]]$ke0, 0.300, tolerance = 0.01)
})

test_that("Rupp's age effect lowers the volumes and clearances, and the tPeak", {
  # Rupp Table 2: the elderly differed significantly in steady-state volume
  # (179 vs 244 mL/kg) and clearance (3.7 vs 5.2 mL/kg/min) only.  Those two
  # ratios reach every volume and every clearance.  tPeak 5.37 -> 4.80 min.
  old <- vecuronium(70, 170, 75, "male", adjustToFFM = FALSE)
  expect_equal_rounded(old$PK$default, list(
    v1 = 7.5194672, v2 = 7.1159836, v3 = 32.1246311,
    cl1 = 0.362884615, cl2 = 0.469615385, cl3 = 0.067596154
  ))
  expect_equal_rounded(old$tPeak, 4.80)

  # Steady-state volume and clearance fall by exactly the published ratios
  young <- vecuronium(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(with(old$PK$default, v1 + v2 + v3) / with(young, v1 + v2 + v3),
               179 / 244, tolerance = 1e-9)
  expect_equal(old$PK$default$cl1 / young$cl1, 3.7 / 5.2, tolerance = 1e-9)

  # Halfway across the 40-70 interpolation
  mid <- vecuronium(70, 170, 55, "male", adjustToFFM = FALSE)
  expect_equal_rounded(mid$PK$default$v1, 8.8847336)
  expect_equal_rounded(mid$PK$default$cl1, 0.436442308)
  expect_equal_rounded(mid$tPeak, 5.085)

  # Flat outside the studied range, and the potency never moves
  expect_identical(vecuronium(70, 170, 25, "male", adjustToFFM = FALSE)$PK,
                   vecuronium(70, 170, 40, "male", adjustToFFM = FALSE)$PK)
  expect_identical(vecuronium(70, 170, 90, "male", adjustToFFM = FALSE)$PK,
                   old$PK)
  expect_equal(vecuronium(70, 170, 80, "male")$lowerTypical,
               vecuronium(70, 170, 30, "male")$lowerTypical)
})
