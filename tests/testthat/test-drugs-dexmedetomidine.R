test_that("returns the published parameters with total-body-weight scaling for age <= 1", {
  weight <- 70
  height <- 171
  age <- 1
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- dexmedetomidine(weight, height, age, sex, adjustToFFM = FALSE)

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
    upperTypical = 0.4,
    lowerTypical = 0.8,
    reference = "Zuppa AF et al., Br J Anaesth 2019;123(6):839-852. https://pubmed.ncbi.nlm.nih.gov/31623840/"
  )

  expect_equal_rounded(actual, expected)
})

test_that("returns the published parameters with total-body-weight scaling for age > 1", {
  weight <- 70
  height <- 171
  age <- 2
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- dexmedetomidine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 8.0574,
        v2 = 12.75343,
        v3 = 177.6944,
        cl1 = 0.4447685,
        cl2 = 2.078809,
        cl3 = 1.990178
      )
    ),
    tPeak = 10,
    MEAC = 0,
    typical = 0.6,
    upperTypical = 0.4,
    lowerTypical = 0.8,
    reference = "Dyck JB et al., Anesthesiology 1993;78(5):821-828. https://pubmed.ncbi.nlm.nih.gov/8098191/"
  )

  expect_equal_rounded(actual, expected)
})

test_that("the adult (Dyck) model scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the Al-Sallami formula by hand).
  actual <- dexmedetomidine(120, 170, 50, "male")
  expected <- list(
    v1 = 10.51415,
    v2 = 16.64204,
    v3 = 231.8747,
    cl1 = 0.5430234,
    cl2 = 2.538044,
    cl3 = 2.429833
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the infant (Zuppa) model scales to fat-free mass across its bypass events", {
  # 10 kg, 75 cm, 1 y male: FFM 7.754 kg (maturation 0.88), so volumes
  # x 0.14234683 and clearances x 0.23174525, against Zuppa's 10/70 = 0.142857
  # and 0.232368.  Pins worked out by hand from the published parameters.
  actual <- dexmedetomidine(10, 75, 1, "male")
  expect_equal_rounded(actual$PK$default$v1,  18.78978)   # 132 L x Fv
  expect_equal_rounded(actual$PK$default$cl1, 0.2873641)  # 1.240 L/min x Fcl
  expect_equal_rounded(actual$PK$CPB33$v1,    19.65831)   # 115 L x Fv x (33/37)^-1.6
  expect_equal_rounded(actual$PK$CPB33$cl2,   0.6906008)  # 2.980 L/min x Fcl
  expect_equal_rounded(actual$PK$CPBEnd$cl1,  0.1436805)  # 0.623 x Fcl x 365/(1.77+365)
  # the placeholders for the missing third compartment are untouched
  expect_equal(actual$PK$CPBStart$v3, 1)
  expect_equal(actual$PK$CPBStart$cl3, 0)
})
