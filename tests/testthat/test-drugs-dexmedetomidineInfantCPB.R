# Zuppa 2019, the infant bypass model: until 2026-11-17 the infant branch
# (age <= 1) of dexmedetomidine, now a drug of its own.

test_that("returns the published parameters with total-body-weight scaling at age 1", {
  weight <- 70
  height <- 171
  age <- 1
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- dexmedetomidineInfantCPB(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 132,
        v2 = 78.9,
        v3 = 1,
        cl1 = 1.24,
        cl2 = 2.3,
        cl3 = 0
      ),
      CPBStart = list(
        v1 = 115,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB36 = list(
        v1 = 120.1535,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB35 = list(
        v1 = 125.6932,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB34 = list(
        v1 = 131.6601,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB33 = list(
        v1 = 138.1015,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB32 = list(
        v1 = 145.071,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB31 = list(
        v1 = 152.6307,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPBEnd = list(
        v1 = 155,
        v2 = 105,
        v3 = 1,
        cl1 = 0.6199935,
        cl2 = 0.209,
        cl3 = 0
      )
    ),
    tPeak = 2,
    MEAC = 0,
    typical = 0.6,
    upperTypical = 0.8,
    lowerTypical = 0.4,
    reference = "Zuppa AF et al., Br J Anaesth 2019;123(6):839-852. https://pubmed.ncbi.nlm.nih.gov/31623840/"
  )

  expect_equal_rounded(actual, expected)
})


test_that("the infant (Zuppa) model scales to fat-free mass across its bypass events", {
  # 10 kg, 75 cm, 1 y male: FFM 7.754 kg (maturation 0.88), so volumes
  # x 0.14234683 and clearances x 0.23174525, against Zuppa's 10/70 = 0.142857
  # and 0.232368.  Pins worked out by hand from the published parameters.
  actual <- dexmedetomidineInfantCPB(10, 75, 1, "male")
  expect_equal_rounded(actual$PK$default$v1,  18.78978)   # 132 L x Fv
  expect_equal_rounded(actual$PK$default$cl1, 0.2873641)  # 1.240 L/min x Fcl
  expect_equal_rounded(actual$PK$CPB33$v1,    19.65831)   # 115 L x Fv x (33/37)^-1.6
  expect_equal_rounded(actual$PK$CPB33$cl2,   0.6906008)  # 2.980 L/min x Fcl
  expect_equal_rounded(actual$PK$CPBEnd$cl1,  0.1436805)  # 0.623 x Fcl x 365/(1.77+365)
  # the placeholders for the missing third compartment are untouched
  expect_equal(actual$PK$CPBStart$v3, 1)
  expect_equal(actual$PK$CPBStart$cl3, 0)
})

test_that("a newborn entered as age 0 keeps a clearance after bypass", {
  # Zuppa's clearance after bypass matures with postnatal age in days,
  # PNA / (1.77 + PNA), which is zero at birth; postnatal age is floored at
  # one day so that it is not (issue #310).
  newborn <- dexmedetomidineInfantCPB(3.5, 50, 0, "male")
  oneDay  <- dexmedetomidineInfantCPB(3.5, 50, 1 / 365, "male")
  expect_gt(newborn$PK$CPBEnd$cl1, 0)
  expect_equal(newborn$PK$CPBEnd$cl1, oneDay$PK$CPBEnd$cl1)
  expect_equal(newborn$PK$CPBEnd$cl1 / newborn$PK$default$cl1,
               0.623 * (1 / 2.77) / 1.240, tolerance = 1e-8)
  # and the drug is simulated through getDrugPK() at age 0
  pk <- getDrugPK("dexmedetomidineInfantCPB", 3.5, 50, 0, "male")$PK$CPBEnd
  expect_gt(pk$k10, 0)
  expect_true(all(is.finite(c(pk$lambda_1, pk$lambda_2, pk$lambda_3))))
})

test_that("the bypass model is offered at every age, unchanged in an adult", {
  # Offered at every age (an extrapolation past 180 days); the parameters
  # are Zuppa's at any age, with the switch off scaled by total weight.
  adult <- dexmedetomidineInfantCPB(70, 171, 35, "male", adjustToFFM = FALSE)
  expect_equal(adult$PK$default$v1, 132)
  expect_equal(adult$PK$CPBStart$cl1, 0.0741)
  expect_setequal(names(adult$PK),
                  c(PK_EVENT_DEFAULT, "CPBStart", "CPB36", "CPB35", "CPB34",
                    "CPB33", "CPB32", "CPB31", "CPBEnd"))
})
