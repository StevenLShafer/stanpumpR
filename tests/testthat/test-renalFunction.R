# Renal function estimated from the four covariates the app collects, at an
# assumed normal serum creatinine for age and sex.  Expected values were worked out by hand
# from the published formulas, not from the code under test.

test_that("Cockcroft-Gault reproduces the textbook formula", {
  # (140 - 35) x 70 / (72 x 1.0)
  expect_equal(creatinineClearanceCG(70, 35, "male"), 102.0833333, tolerance = 1e-7)
  # Woman: assumed creatinine 0.8 and the 0.85 factor
  expect_equal(creatinineClearanceCG(60, 50, "female"), (90 * 60 / (72 * 0.8)) * 0.85)
  # An explicit creatinine overrides the assumption
  expect_equal(creatinineClearanceCG(70, 35, "male", scr = 2), 102.0833333 / 2, tolerance = 1e-7)
  # Falls with age
  expect_lt(creatinineClearanceCG(70, 80, "male"), creatinineClearanceCG(70, 35, "male"))
})

test_that("an adult's assumed creatinine depends on sex only", {
  expect_equal(assumedCreatinine(40, "male"), 1.0)
  expect_equal(assumedCreatinine(40, "female"), 0.8)
  expect_equal(assumedCreatinine(18, "male"), 1.0)
  expect_equal(assumedCreatinine(90, "female"), 0.8)
})

test_that("from 2 years a child's assumed creatinine is the EKFC normal for age", {
  # Pottel et al. 2021, Q in umol/L, 88.4 umol/L to the mg/dL
  qMale   <- function(a) exp(3.200 + 0.259 * a - 0.543 * log(a) - 0.00763 * a^2 + 0.0000790 * a^3) / 88.4
  qFemale <- function(a) exp(3.080 + 0.177 * a - 0.223 * log(a) - 0.00596 * a^2 + 0.0000686 * a^3) / 88.4
  for (a in c(2, 5, 8, 12, 15, 17.9)) {
    expect_equal(assumedCreatinine(a, "male"), qMale(a), info = a)
    expect_equal(assumedCreatinine(a, "female"), qFemale(a), info = a)
  }
  # About 0.35 mg/dL at five and 0.5 at ten; boys and girls part in
  # adolescence, to 0.79 and 0.66 at seventeen.
  expect_equal(assumedCreatinine(5, "male"), 0.353, tolerance = 1e-3)
  expect_equal(assumedCreatinine(10, "female"), 0.510, tolerance = 1e-3)
  expect_equal(assumedCreatinine(17, "male"), 0.791, tolerance = 1e-3)
  expect_equal(assumedCreatinine(17, "female"), 0.664, tolerance = 1e-3)
  # At 18 it steps to the adult convention, which sits above the children's
  # medians
  expect_lt(assumedCreatinine(17.99, "male"), 0.82)
  expect_lt(assumedCreatinine(17.99, "female"), 0.68)
})

test_that("under a year the assumed creatinine follows Boer 2010", {
  umol <- function(age, sex = "male") assumedCreatinine(age, sex) * 88.4
  day  <- function(d) d / 365.25
  # The plateau of 20 umol/L from day 65 to day 216, the same for both sexes
  expect_equal(umol(day(65)), 20)
  expect_equal(umol(day(150)), 20)
  expect_equal(umol(day(216), "female"), 20)
  # Before it, log10 creatinine falls 0.07 per doubling of age; after it, it
  # rises 0.045 per doubling
  expect_equal(umol(day(1)), 20 * 10^(0.07 * log2(65)))
  expect_equal(umol(1), 20 * 10^(0.045 * log2(365.25 / 216)))
  # Birth takes the day-1 value
  expect_equal(umol(0), umol(day(1)))
  # The paper's means: 55 umol/L on day 1, 22 in the second month
  expect_equal(umol(day(1)), 55, tolerance = 0.05)
  expect_equal(umol(day(45)), 22, tolerance = 0.02)
})

test_that("the assumed creatinine is continuous from birth to 18", {
  for (sex in SEX_VALUES) {
    for (a in c(65 / 365.25, 216 / 365.25, 1, 2)) {
      expect_equal(assumedCreatinine(a - 1e-9, sex), assumedCreatinine(a + 1e-9, sex),
                   tolerance = 1e-6, info = paste(sex, a))
    }
    ages <- seq(0, 17.9, by = 0.1)
    q <- vapply(ages, assumedCreatinine, numeric(1), sex = sex)
    expect_true(all(q > 0.2 & q < 0.85), info = sex)
  }
})

test_that("the adult renal equations see a child's creatinine on the adult scale", {
  # A blank creatinine is the adult value at any age
  for (sex in SEX_VALUES) {
    for (a in c(0, 0.5, 5, 12, 17, 18, 40)) {
      expect_equal(adultEquivalentCreatinine(NULL, a, sex), adultCreatinine(sex),
                   info = paste(sex, a))
    }
  }
  # A child at the normal creatinine for age reads as an adult at the
  # adult's; at twice it, as twice the adult's
  q5 <- assumedCreatinine(5, "male")
  expect_equal(adultEquivalentCreatinine(q5, 5, "male"), 1.0)
  expect_equal(adultEquivalentCreatinine(2 * q5, 5, "male"), 2.0)
  q10 <- assumedCreatinine(10, "female")
  expect_equal(adultEquivalentCreatinine(q10, 10, "female"), 0.8)
  # From 18 the entered value passes through
  expect_equal(adultEquivalentCreatinine(0.7, 18, "male"), 0.7)
  expect_equal(adultEquivalentCreatinine(2.5, 60, "female"), 2.5)
})

test_that("Cockcroft-Gault on a child's own creatinine overstates renal function", {
  # The example in R/renalFunction.R: a 5 y boy of 20 kg and 110 cm.  At his
  # normal creatinine Cockcroft-Gault is 106 mL/min, where the normal GFR for
  # his size is 107.3 mL/min/1.73 m^2 x 0.78 m^2 = 48 mL/min.  On the adult
  # scale he gets (140 - 5) x 20 / 72 = 37.5.
  q <- assumedCreatinine(5, "male")
  expect_equal(round(creatinineClearanceCG(20, 5, "male", q)), 106)
  expect_equal(round(bsaDuBois(20, 110), 2), 0.78)
  expect_equal(round(107.3 * bsaDuBois(20, 110) / 1.73), 48)
  expect_equal(creatinineClearanceCG(20, 5, "male", adultEquivalentCreatinine(NULL, 5, "male")),
               135 * 20 / 72)
})

test_that("Du Bois body surface area", {
  expect_equal(bsaDuBois(70, 170), 1.809708, tolerance = 1e-6)
})

test_that("CKD-EPI 2009 without the race term", {
  # Male, 35 y, creatinine 1.0: 141 x (1/0.9)^-1.209 x 0.993^35
  expect_equal(egfrCKDEPI2009(35, "male"), 97.07826, tolerance = 1e-6)
  # Female, 50 y, creatinine 0.8: ratio 0.8/0.7 > 1, so the -1.209 branch,
  # times 1.018
  expect_equal(egfrCKDEPI2009(50, "female"),
               141 * (0.8 / 0.7)^(-1.209) * 0.993^50 * 1.018, tolerance = 1e-9)
  # Below kappa the alpha branch applies
  expect_equal(egfrCKDEPI2009(35, "male", scr = 0.6),
               141 * (0.6 / 0.9)^(-0.411) * 0.993^35, tolerance = 1e-9)
  # De-indexing multiplies by BSA / 1.73
  expect_equal(egfrDeindexed(70, 170, 35, "male"), 97.07826 * 1.809708 / 1.73, tolerance = 1e-5)
})
