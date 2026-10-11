# Methylphenidate in children (Shader 1999): total methylphenidate after
# racemic IR tablets.
#
# What is guarded: the published parameters and the volume they imply, the
# linear weight scaling, the one-compartment curve against the paper's own
# appendix equation, and the steady-state check against Shader's sampled
# concentrations.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

shaderCurve <- function(dose, weight = 40, maximum = 1440) {
  simulateDrugsWithCovariates(dose, noEvents, weight, 145, 11, "male",
                              maximum, FALSE, adjustToFFM = FALSE
  )$methylphenidatePediatric$wide
}


test_that("returns the correct calculations", {
  # 40 kg, switch off: CL/F = 90.7 mL/min/kg x 40 kg; V/F = CL/F / BETA
  actual <- methylphenidatePediatric(40, 145, 11, "male", adjustToFFM = FALSE)
  cl <- 90.7 / 1000 * 40
  expected <- list(
    PK = list(default = list(
      v1 = cl / (0.154 / 60), v2 = 1, v3 = 1,
      cl1 = cl, cl2 = 0, cl3 = 0,
      ka_PO = 1.192 / 60,
      bioavailability_PO = 1,
      tlag_PO = 0
    )),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  # The implied volume, 35.3 L/kg, and the 4.5 h half-life
  expect_equal(actual$PK$default$v1 / 40, 35.34, tolerance = 1e-3)
  expect_equal(log(2) / 0.154, 4.5, tolerance = 0.01)
})


test_that("clearance and volume are proportional to weight", {
  a <- methylphenidatePediatric(20, 115, 6, "female", adjustToFFM = FALSE)$PK$default
  b <- methylphenidatePediatric(60, 165, 16, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(b$cl1 / a$cl1, 3)
  expect_equal(b$v1 / a$v1, 3)

  # Switch on: the pharmacokinetic weight, 70 x 1.3049067 kg for a 120 kg man
  pkW <- 70 * 1.3049067
  y <- methylphenidatePediatric(120, 170, 50, "male")$PK$default
  expect_equal_rounded(y$cl1, 90.7 / 1000 * pkW)
})


test_that("the curve is the appendix's one-compartment equation", {
  # C = D x KA / (V (KA - BETA)) x (exp(-BETA t) - exp(-KA t)), V = CL/BETA,
  # here for a single 10 mg dose at 40 kg
  w <- shaderCurve(data.frame(Drug = "methylphenidatePediatric", Time = 0,
                              Dose = 10, Units = "mg PO"))
  t <- w$Time / 60
  V <- 90.7 * 60 / 1000 * 40 / 0.154
  ref <- 10000 / V * 1.192 / (1.192 - 0.154) * (exp(-0.154 * t) - exp(-1.192 * t))
  expect_equal(w$Plasma, ref, tolerance = 1e-6)
  # single-dose peak near 2 h
  expect_equal(t[which.max(w$Plasma)], log(1.192 / 0.154) / (1.192 - 0.154), tolerance = 0.05)
})


test_that("steady state on the tid regimen agrees with Shader's samples", {
  # Mean tid dose 0.360 mg/kg about 4 h apart; the repeat-sample subgroup
  # averaged 9.6 ng/mL.  The model's daily average at steady state is
  # 1.08 mg/kg/day / (5.442 L/h/kg x 24 h) = 8.3 ng/mL.
  dose <- data.frame(Drug = "methylphenidatePediatric",
                     Time = as.vector(outer(c(0, 240, 480), 1440 * 0:6, `+`)),
                     Dose = 0.36, Units = "mg/kg PO")
  w <- shaderCurve(dose, maximum = 7 * 1440)
  day <- w[w$Time >= 6 * 1440, ]
  avg <- trapz(day$Time, day$Plasma) / (max(day$Time) - min(day$Time))
  expect_equal(avg, 1.08 / (90.7 * 60 / 1000 * 24) * 1000, tolerance = 0.02)
  expect_gt(avg / 9.6, 0.75)
  expect_lt(avg / 9.6, 1.1)
})


test_that("offered orally only, IR only, with no claimed effect or band", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "methylphenidatePediatric", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_true(all(doseRoute(units) == "PO"))
  expect_false(any(grepl("XR", units)))
  expect_equal(row$Category, "Stimulants")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC, row$endCe), rep(0, 5))
})
