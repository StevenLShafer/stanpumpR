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
  expect_equal(vc(5), vc(45))
  expect_equal(vc(85), vc(70))
  # nothing else moves with age
  a <- ondansetron(70, 171, 45, "male", adjustToFFM = FALSE)$PK$default
  b <- ondansetron(70, 171, 70, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(a[c("v2", "cl1", "cl2")], b[c("v2", "cl1", "cl2")])
})

test_that("the band is the CSV's", {
  d <- getDrugDefaults("ondansetron")
  x <- ondansetron(70, 170, 50, "male")
  expect_equal(c(d$Lower, d$Upper, d$Typical, d$MEAC),
               c(x$lowerTypical, x$upperTypical, x$typical, x$MEAC))
})
