# Acetaminophen: Morse 2022 intravenous and fasted-tablet disposition with
# clearance on normal fat mass, and ke0 from Anderson 2001.  See the header of
# R/drugs_acetaminophen.R.  Pins were worked out from the published numbers by
# hand (Python), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # Clearance uses normal fat mass in either switch position (70 kg, 171 cm
  # male: NFM 67.17 kg against Morse's standard 67.45 kg), so it is not
  # exactly 24 L/h here.
  actual <- acetaminophen(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 43.7,
        v2 = 29.7,
        v3 = 1,
        cl1 = 0.398876879,
        cl2 = 0.725,
        cl3 = 0,
        ka_PO = 0.0602736679,
        bioavailability_PO = 0.859,
        tlag_PO = 5.3
      )
    ),
    tPeak = 0,
    ke0 = 0.0130782487,
    MEAC = 0,
    typical = 7,
    upperTypical = 15,
    lowerTypical = 3,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg.  Clearance follows its own
  # normal-fat-mass covariate, 24 x ((71.09 + 0.816 x 48.91) / 67.45)^0.75
  # L/h; the volumes see the pharmacokinetic weight 91.34 kg; Q takes the
  # library clearance factor 1.2209126.
  actual <- acetaminophen(120, 170, 50, "male")
  expected <- list(
    v1 = 57.024420436,
    v2 = 38.75572739,
    v3 = 1,
    cl1 = 0.581202027,
    cl2 = 0.885161648,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Morse's vector is recovered for her standard 70 kg, 176 cm man", {
  x <- acetaminophen(70, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 24.0)
  expect_equal(x$v1, 43.7); expect_equal(x$v2, 29.7); expect_equal(x$cl2 * 60, 43.5)
  # The library's reference man is a little fatter: 23.92 L/h
  y <- acetaminophen(70, 170, 35, "male")$PK$default
  expect_equal(y$cl1, 0.398647084, tolerance = 1e-8)
  expect_equal(y$v1, 43.7); expect_equal(y$cl2 * 60, 43.5)
  # With the switch off, volumes and Q follow total weight as published
  z <- acetaminophen(140, 176, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(z$v1, 87.4); expect_equal(z$v2, 59.4)
  expect_equal(z$cl2 * 60, 43.5 * 2^0.75)
})

test_that("the oral absorption is Morse's fasted tablet, lag kept", {
  x <- acetaminophen(70, 170, 35, "male")$PK$default
  expect_equal(x$tlag_PO, 5.3)
  expect_equal(log(2) / x$ka_PO, 11.5)              # absorption half-life, min
  expect_equal(x$bioavailability_PO, 0.859)
})

test_that("ke0 is Anderson's 53 min equilibration half-time", {
  PK <- getDrugPK("acetaminophen", 70, 170, 35, "male",
                  getDrugDefaults("acetaminophen"))$PK$default
  expect_equal(log(2) / PK$ke0, 53)
})

# Independent check: integrate the two-compartment model with an effect site
# and first-order oral input by fourth-order Runge-Kutta, from the published
# parameters, and compare with the closed-form engine at its own time points.
# The oral lag is applied by time shift (the system is time-invariant): an
# oral dose at 0 gives, at time t, what an unlagged dose gives at t - 5.3,
# and nothing before 5.3 min.
rk4Acetaminophen <- function(times, doseIV, dosePO, dt = 0.02)
{
  v1 <- 43.7; v2 <- 29.7; cl <- 0.398647084; q <- 43.5 / 60  # reference man
  ka <- log(2) / 11.5; f <- 0.859; ke0 <- log(2) / 53
  d <- function(s) c(-ka * s[1],
                     f * ka * s[1] - (cl + q) * s[2] / v1 + q * s[3] / v2,
                     q * s[2] / v1 - q * s[3] / v2,
                     ke0 * (s[2] / v1 - s[4]))
  s <- c(dosePO, doseIV, 0, 0); t <- 0
  out <- matrix(NA_real_, length(times), 2)
  for (i in seq_along(times)) {
    while (t < times[i] - 1e-12) {
      h <- min(dt, times[i] - t)
      k1 <- d(s); k2 <- d(s + h / 2 * k1); k3 <- d(s + h / 2 * k2); k4 <- d(s + h * k3)
      s <- s + h * (k1 + 2 * k2 + 2 * k3 + k4) / 6
      t <- t + h
    }
    out[i, ] <- c(s[2] / v1, s[4])
  }
  out
}

test_that("1 g intravenous and oral match an independent integration", {
  iv <- simulateDrugsWithCovariates(
    data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000, Units = "mg"),
    noEvents, 70, 170, 35, "male", 360, TRUE)$acetaminophen$wide
  ref <- rk4Acetaminophen(iv$Time, 1000, 0)
  # skip t = 0, where the engine reports the pre-bolus value
  keep <- iv$Time > 0
  expect_equal(iv$Plasma[keep], ref[keep, 1], tolerance = 1e-5)
  expect_equal(iv$"Effect Site"[keep], ref[keep, 2], tolerance = 1e-5)

  po <- simulateDrugsWithCovariates(
    data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 360, TRUE)$acetaminophen$wide
  lag <- 5.3
  after <- po$Time > lag
  ref <- rk4Acetaminophen(po$Time[after] - lag, 0, 1000)
  expect_equal(po$Plasma[after], ref[, 1], tolerance = 1e-5)
  expect_equal(po$"Effect Site"[after], ref[, 2], tolerance = 1e-5)
  # Nothing reaches plasma during the lag
  expect_true(all(po$Plasma[po$Time < lag] == 0))
  # Fasted 1 g tablet: the Python integration peaks at 11.05 mg/L at 34 min
  expect_gt(max(po$Plasma), 10.8)
  expect_lt(max(po$Plasma), 11.2)
  expect_gt(po$Time[which.max(po$Plasma)], 29)
  expect_lt(po$Time[which.max(po$Plasma)], 39)
})


test_that("the CSV band and threshold match the model, and 1 g IV crosses it", {
  dd <- getDrugDefaults("acetaminophen")
  x <- acetaminophen(70, 170, 35, "male")
  expect_equal(dd$Lower, x$lowerTypical)
  expect_equal(dd$Upper, x$upperTypical)
  expect_equal(dd$Typical, x$typical)
  expect_equal(dd$endCe, 5)
  expect_equal(dd$MEAC, 0)
  # The threshold is set so an ordinary adult dose produces a real time
  # until threshold: 1 g IV in the reference man peaks near 7.3 mcg/mL in
  # the effect site, above 5.
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000, Units = "mg"),
    noEvents, 70, 170, 35, "male", 480, TRUE)$acetaminophen$wide
  expect_gt(max(w$"Effect Site"), dd$endCe)
  expect_gt(max(w$Recovery, na.rm = TRUE), 60)
})
