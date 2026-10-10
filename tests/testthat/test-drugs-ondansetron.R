# ondansetron: see the header of R/drugs_ondansetron.R for what the model is and
# what it leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
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
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  actual <- ondansetron(120, 170, 50, "male")
  expected <- list(
    v1 = 82.600594,
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
    o <- simulateDrugsWithCovariates(DT[DT$Time < t, ], noEvents, 70, 170, 50,
                                     "male", t, FALSE, adjustToFFM = FALSE)
    w <- o$ondansetron$wide
    w$Plasma[nrow(w)]
  }
  expect_equal(at(15), 43.616758, tolerance = 1e-5)
  expect_equal(at(60), 19.309551, tolerance = 1e-5)
  expect_equal(at(720), 4.1580069, tolerance = 1e-5)
})

test_that("the band is the CSV's", {
  d <- getDrugDefaults("ondansetron")
  x <- ondansetron(70, 170, 50, "male")
  expect_equal(c(d$Lower, d$Upper, d$Typical, d$MEAC),
               c(x$lowerTypical, x$upperTypical, x$typical, x$MEAC))
})
