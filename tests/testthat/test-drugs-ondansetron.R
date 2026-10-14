# ondansetron: see the header of R/drugs_ondansetron.R for what the model is and
# what it leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters at the median age, switch off", {
  # 58 y is Chiang's median age, where the age factor on Vc is exactly 1
  weight <- 70
  height <- 171
  age <- 58
  sex <- "male"
  actual <- ondansetron(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 63.3,
        v2 = 107,
        v3 = 1,
        cl1 = 0.41,
        cl2 = 3.5166667,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 20, upperTypical = 40, lowerTypical = 5,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126;
  # Vc also x (50/58)^-4.91 = 2.0724722
  actual <- ondansetron(120, 170, 50, "male")
  expected <- list(
    v1 = 171.18744,
    v2 = 139.62502,
    v3 = 1,
    cl1 = 0.50057417,
    cl2 = 4.2935426,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("4 mg over 15 minutes reproduces the reference implementation", {
  # Values from the contributed Python reference (exact matrix exponentials,
  # antiemetic_pkpd.py): 43.6168, 19.3096 and 4.1580 ng/mL at 0.25, 1 and 12 h.
  DT <- data.frame(Drug = "ondansetron", Time = c(0, 15), Dose = c(16, 0),
                   Units = "mg/hr")
  at <- function(t) {
    # at the 58-year median, where the age factor is 1 (the Python reference
    # had no age term)
    o <- simulateDrugsWithCovariates(DT[DT$Time < t, ], noEvents, 70, 170, 58,
                                     "male", t, FALSE, adjustToFFM = FALSE)
    w <- o$ondansetron$wide
    w$Plasma[nrow(w)]
  }
  expect_equal(at(15), 43.616758, tolerance = 1e-5)
  expect_equal(at(60), 19.309551, tolerance = 1e-5)
  expect_equal(at(720), 4.1580069, tolerance = 1e-5)
})

test_that("Chiang's age term scales Vc only, within the fitted 45-70 years", {
  # Vc = 63.3 x (age/58)^-4.91, age held within 45-70: 131.18749 L at 50,
  # 220.07044 L at 45 and below, 25.142229 L at 70 and above
  vc <- function(age) ondansetron(70, 171, age, "male", adjustToFFM = FALSE)$PK$default$v1
  expect_equal(vc(50), 131.18749, tolerance = 1e-7)
  expect_equal(vc(45), 220.07044, tolerance = 1e-7)
  expect_equal(vc(70), 25.142229, tolerance = 1e-7)
  expect_equal(vc(30), vc(45))
  expect_equal(vc(18), vc(45))   # adults 18-45 are held at 45 (children use Mondick)
  expect_equal(vc(85), vc(70))
  # nothing else moves with age
  a <- ondansetron(70, 171, 45, "male", adjustToFFM = FALSE)$PK$default
  b <- ondansetron(70, 171, 70, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(a[c("v2", "cl1", "cl2")], b[c("v2", "cl1", "cl2")])
})

# --- Mondick 2010, below 18 years ----------------------------------------
# CL = 1.53 WT^0.75 (1 - 0.760 exp(-(AGE-1) ln2/3.82)) L/h, V1 = 0.930 WT,
# Q = 15.0 WT^0.75 L/h, V2 = 2.52 WT; AGE in months, held at >= 1 month.
# Pins worked out by hand (Python).

test_that("Mondick's model at the 10.4 kg reference, 12 months, switch off", {
  actual <- ondansetron(10.4, 76, 1, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 9.672, v2 = 26.208, v3 = 1,
        cl1 = 0.13242714, cl2 = 1.4478215, cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 20, upperTypical = 40, lowerTypical = 5,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  expect_match(actual$reference, "Mondick")
})

test_that("Mondick's maturation reproduces the paper's checks", {
  # clearance reduced 31%, 53% and 76% at 6, 3 and 1 months
  expect_equal(1 - ondansetronMaturation(c(6, 3, 1)[1]), 0.3067, tolerance = 1e-3)
  expect_equal(1 - ondansetronMaturation(3), 0.5287, tolerance = 1e-3)
  expect_equal(1 - ondansetronMaturation(1), 0.76)
  # held at 1 month below it (the youngest patient)
  expect_equal(ondansetronMaturation(0.2), ondansetronMaturation(1))
  # clearance per kg 0.629, 0.770, 0.794 L/h/kg at 6 mo/8 kg, 12 mo/10 kg,
  # 24 mo/13 kg (the paper rounds; these are the equation's values)
  clkg <- function(w, y) ondansetron(w, 70, y, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60 / w
  expect_equal(clkg(8, 0.5), 0.6307, tolerance = 1e-3)
  expect_equal(clkg(10, 1), 0.7716, tolerance = 1e-3)
  expect_equal(clkg(13, 2), 0.7958, tolerance = 1e-3)
  # and 0.527 L/h/kg extrapolated to a mature 70 kg patient
  expect_equal(clkg(70, 17), 0.5287, tolerance = 1e-3)
})

test_that("Mondick's model uses the pharmacokinetic weight with the switch on", {
  # 12 kg, 80 cm, 2 y boy: pharmacokinetic weight 11.72312 kg, maturation
  # factor at 24 months 0.98829613
  actual <- ondansetron(12, 80, 2, "male")$PK$default
  expected <- list(v1 = 10.902502, v2 = 29.542262, cl1 = 0.15966499,
                   cl2 = 1.5838805)
  expect_equal_rounded(actual[names(expected)], expected)
})

test_that("the models switch at 18 years, discontinuously, as documented", {
  young <- ondansetron(70, 176, 17.99, "male", adjustToFFM = FALSE)
  adult <- ondansetron(70, 176, 18, "male", adjustToFFM = FALSE)
  expect_match(young$reference, "Mondick")
  expect_match(adult$reference, "Chiang")
  # Mondick at 70 kg: CL 37.0 L/h, V1 65.1 L; Chiang held at 45 y: Vc 220.07 L
  expect_equal(young$PK$default$v1, 65.1)
  expect_equal(young$PK$default$cl1 * 60, 37.026696, tolerance = 1e-6)
  expect_equal(adult$PK$default$v1, 220.07044, tolerance = 1e-7)
  expect_equal(adult$PK$default$cl1 * 60, 24.6)
  expect_equal(ONDANSETRON_PEDIATRIC_AGE, 18)
})

test_that("the band is the CSV's", {
  d <- getDrugDefaults("ondansetron")
  x <- ondansetron(70, 170, 50, "male")
  expect_equal(c(d$Lower, d$Upper, d$Typical, d$MEAC),
               c(x$lowerTypical, x$upperTypical, x$typical, x$MEAC))
})
