# Dexmedetomidine: Morse 2020 above one year (since 2026-11-17, replacing
# Dyck's fixed set); Zuppa 2019 with its bypass sets at one year or under.

morseMF <- function(pma) pma / (52.4 + pma)

test_that("the standard man of Table 1 receives the published values", {
  # 70 kg, 176 cm, 35 y man: Al-Sallami FFM 56.13 kg, the paper's 56.1
  x <- dexmedetomidine(70, 176, 35, "male")$PK$default
  expect_equal(x$v1, 25.2, tolerance = 0.001)
  expect_equal(x$v2, 34.4, tolerance = 0.001)
  expect_equal(x$v3, 65.4, tolerance = 0.001)
  expect_equal(x$cl2, 1.68, tolerance = 0.001)
  expect_equal(x$cl3, 0.62, tolerance = 0.001)
  # clearance carries the unnormalised maturation, 0.973 at 35 years
  expect_equal(x$cl1, 0.897 * morseMF(40 + 52 * 35), tolerance = 0.001)
  expect_equal_rounded(unlist(x),
    c(v1 = 25.208103, v2 = 34.411061, v3 = 65.421028,
      cl1 = 0.872741, cl2 = 1.680615, cl3 = 0.620227))
})

test_that("the library's reference man is pinned", {
  x <- dexmedetomidine(70, 170, 35, "male")
  expect_equal_rounded(unlist(x$PK$default),
    c(v1 = 24.718918, v2 = 33.743285, v3 = 64.151479,
      cl1 = 0.853402, cl2 = 1.643374, cl3 = 0.606483))
  expect_equal(x$tPeak, 14.9)
  expect_equal(x$MEAC, 0)
  expect_equal(c(x$lowerTypical, x$typical, x$upperTypical), c(0.4, 0.6, 0.8))
  expect_match(x$reference, "Morse JD")
})

test_that("volumes follow normal fat mass and clearances fat-free mass", {
  ffm <- ffmAlSallami(140, 176, 40, "male")
  nfm <- ffm + 0.293 * (140 - ffm)
  x <- dexmedetomidine(140, 176, 40, "male")$PK$default
  expect_equal(x$v1, 25.2 * nfm / (56.1 + 0.293 * 13.9), tolerance = 1e-8)
  expect_equal(x$cl2, 1.68 * (ffm / 56.1)^0.75, tolerance = 1e-8)
  expect_equal(x$cl1, 0.897 * (ffm / 56.1)^0.75 * morseMF(40 + 52 * 40),
               tolerance = 1e-8)
  expect_equal_rounded(unlist(x),
    c(v1 = 40.549187, v2 = 55.352858, v3 = 105.234795,
      cl1 = 1.130833, cl2 = 2.170297, cl3 = 0.800943))
})

test_that("clearance matures with postmenstrual age, as the paper says", {
  # Morse's own function, which the drug uses only above one year
  # "Clearance in term neonates is 42% of adult values, reaching 80% by 3 years"
  expect_equal(morseMF(40), 0.433, tolerance = 0.002)
  expect_equal(morseMF(40 + 52 * 3), 0.789, tolerance = 0.002)
  term <- dexmedetomidineMorse(3.5, 50, 0, "female")$PK$default
  pre  <- dexmedetomidineMorse(3.5, 50, 0, "female", gestationalAge = 28)$PK$default
  expect_equal(term$cl1 / pre$cl1, morseMF(40) / morseMF(28), tolerance = 1e-8)
  # maturation is on clearance alone
  expect_equal(term[c("v1", "v2", "v3", "cl2", "cl3")],
               pre[c("v1", "v2", "v3", "cl2", "cl3")])
  expect_equal_rounded(c(term$cl1, pre$cl1), c(0.0426725, 0.0343291))
})

test_that("the size switch does not change Morse's model", {
  for (a in list(c(70, 170, 35), c(140, 176, 40), c(12, 85, 2))) {
    expect_equal(dexmedetomidine(a[1], a[2], a[3], "female", adjustToFFM = FALSE),
                 dexmedetomidine(a[1], a[2], a[3], "female", adjustToFFM = TRUE))
  }
})

test_that("the model is finite at the extremes and runs through getDrugPK()", {
  for (s in c("male", "female")) for (a in c(0, 0.01, 18, 90))
    for (w in c(0.5, 3, 70, 250)) {
      x <- unlist(dexmedetomidine(w, if (w < 5) 45 else 170, a, s,
                                  gestationalAge = 22)$PK)
      v <- x[grepl("v[123]$", names(x))]
      k <- x[grepl("cl[123]$", names(x))]
      expect_true(all(is.finite(v) & v > 0), info = paste(s, a, w))
      expect_true(all(is.finite(k) & k >= 0), info = paste(s, a, w))
    }
  pk <- getDrugPK("dexmedetomidine", 70, 170, 35, "male")$PK$default
  expect_gt(pk$ke0, 0)
  expect_true(all(is.finite(c(pk$lambda_1, pk$lambda_2, pk$lambda_3))))
})

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

test_that("a newborn entered as age 0 keeps a clearance after bypass", {
  # Zuppa's clearance after bypass matures with postnatal age in days,
  # PNA / (1.77 + PNA), which is zero at birth; postnatal age is floored at
  # one day so that it is not (issue #310).
  newborn <- dexmedetomidine(3.5, 50, 0, "male")
  oneDay  <- dexmedetomidine(3.5, 50, 1 / 365, "male")
  expect_gt(newborn$PK$CPBEnd$cl1, 0)
  expect_equal(newborn$PK$CPBEnd$cl1, oneDay$PK$CPBEnd$cl1)
  expect_equal(newborn$PK$CPBEnd$cl1 / newborn$PK$default$cl1,
               0.623 * (1 / 2.77) / 1.240, tolerance = 1e-8)
  # and the drug is simulated through getDrugPK() at age 0
  pk <- getDrugPK("dexmedetomidine", 3.5, 50, 0, "male")$PK$CPBEnd
  expect_gt(pk$k10, 0)
  expect_true(all(is.finite(c(pk$lambda_1, pk$lambda_2, pk$lambda_3))))
})

# The boundary between Zuppa's infant model (age <= 1) and the Dyck adult
# model (age > 1) is a step, and a large one: the help page says so, with the
# numbers pinned here.  This test documents the step so that a change to
# either side is deliberate (an audit of 2026-10-12 asked for it); the model

# The boundary between Zuppa's infant model (age <= 1) and Morse (age > 1) is
# a step, which the help page describes with these numbers.  The choice to
# keep Zuppa at one year or under is Steven L. Shafer's decision of
# 2026-11-17.  Before then the step was to Dyck's set, 16 times in V1.
test_that("the step at one year is the one the help page describes", {
  infant <- dexmedetomidine(9, 75, 1,     "male")
  child  <- dexmedetomidine(9, 75, 1.001, "male")
  expect_match(infant$reference, "Zuppa")
  expect_match(child$reference, "Morse")
  expect_equal_rounded(infant$PK$default$v1, 17.55146)
  expect_equal_rounded(child$PK$default$v1,  3.249023)
  # "about 5 times higher" immediately after a bolus, with fat-free-mass scaling
  expect_equal(infant$PK$default$v1 / child$PK$default$v1, 5.40, tolerance = 0.01)
  # and the clearance about half
  expect_equal(child$PK$default$cl1 / infant$PK$default$cl1, 0.451, tolerance = 0.01)

  infantLegacy <- dexmedetomidine(9, 75, 1, "male", adjustToFFM = FALSE)
  expect_equal_rounded(infantLegacy$PK$default$v1, 16.97143)
  # Morse ignores the switch, so the published-scaling step is about 5.2
  expect_equal(infantLegacy$PK$default$v1 / child$PK$default$v1, 5.22, tolerance = 0.01)
})
