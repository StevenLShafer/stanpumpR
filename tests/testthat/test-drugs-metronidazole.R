# metronidazole: see the header of R/drugs_metronidazole.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- metronidazole(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 37.866665,
        v2 = 1,
        v3 = 1,
        cl1 = 0.053666667,
        cl2 = 0,
        cl3 = 0,
        ka_PO = 0.022266667,
        bioavailability_PO = 0.841,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 8, upperTypical = 25, lowerTypical = 4,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- metronidazole(120, 170, 50, "male")
  expected <- list(
    v1 = 49.018278,
    v2 = 1,
    v3 = 1,
    cl1 = 0.065522311,
    cl2 = 0,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("volume follows Devine adjusted body weight with the switch off", {
  # Devine 171 cm male: 50 + 2.3 x (67.32 - 60) = 66.84 kg; adjusted 68.10 kg
  expect_equal(idealBodyWeightDevine(171, "male"), 50 + 2.3 * (171 / 2.54 - 60))
  expect_equal(idealBodyWeightDevine(160, "female"), 45.5 + 2.3 * (160 / 2.54 - 60))
  x <- metronidazole(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$v1, 0.556 * adjustedBodyWeight(70, 171, "male"))
  expect_equal(x$cl1 * 60, 3.22)
  # Half-time at the illustrative TBW = AJBW = 70 kg: 8.378 h
  expect_equal(log(2) / (3.22 / (0.556 * 70)), 8.37804, tolerance = 1e-5)
  # Oral: F 0.841, ka 1.336 /h
  expect_equal(x$bioavailability_PO, 0.841); expect_equal(x$ka_PO * 60, 1.336)
})

test_that("a child's volume is on total body weight with the switch off", {
  # 0.5 y, 7 kg, 65 cm boy: Devine gives 50 + 2.3 x (25.59 - 60) = -29.1 kg and
  # the published adjusted weight -14.7 kg, i.e. V = -8.16 L.  Under 18 the
  # adjusted weight is replaced by total body weight: V = 0.556 x 7 = 3.892 L.
  # Clearance keeps the published (TBW/70)^0.75: 3.22 x 0.1^0.75 / 60.
  expect_lt(adjustedBodyWeight(7, 65, "male"), 0)
  x <- metronidazole(7, 65, 0.5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(x$v1, 3.892)
  expect_equal_rounded(x$cl1, 0.009543433)

  # 5 y, 20 kg, 110 cm: Devine is positive (11.6 kg) but still an adult
  # formula; the child is scaled on total weight, V = 0.556 x 20 = 11.12 L.
  x <- metronidazole(20, 110, 5, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(x$v1, 11.12)

  # From 18 the published adjusted weight applies unchanged, here at the
  # source's lower bounds of age, weight and height.
  x <- metronidazole(47.6, 144, 18, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(x$v1, 0.556 * adjustedBodyWeight(47.6, 144, "female"))

  # An adult at a height where Devine has no positive ideal weight (below
  # about 102 cm in a woman) falls back to total body weight as well.
  expect_lte(idealBodyWeightDevine(100, "female"), 0)
  x <- metronidazole(30, 100, 40, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(x$v1, 0.556 * 30)

  # The default fat-free-mass path does not use Devine and is unchanged.
  x <- metronidazole(7, 65, 0.5, "male")$PK$default
  expect_equal(x$v1, 0.556 * adjustedBodyWeight(70, 170, "male") *
                 pkSizeFactors(7, 65, 0.5, "male")$volume)
})

test_that("the infant's coefficients are positive and finite with the switch off", {
  # The negative volume used to give k10 < 0 and p_coef_infusion_l1 = -Inf.
  pk <- getDrugPK("metronidazole", weight = 7, height = 65, age = 0.5,
                  sex = "male", adjustToFFM = FALSE)$PK[[PK_EVENT_DEFAULT]]
  coefs <- unlist(pk[c("lambda_1", "p_coef_bolus_l1", "p_coef_infusion_l1", "p_coef_PO_l1")])
  expect_true(all(is.finite(coefs)) && all(coefs > 0))
})
