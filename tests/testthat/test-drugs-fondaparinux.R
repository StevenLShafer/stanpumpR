# Fondaparinux: Delavenne 2010 subcutaneous two-compartment model after
# orthopaedic surgery.  See the header of R/drugs_fondaparinux.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the reference parameters with the switch off", {
  # 68 kg, 74 y male (reference weight and age); clearance uses the
  # simplified-MDRD creatinine clearance, so build the expected CL from egfrMDRD().
  scr <- 0.9
  actual <- fondaparinux(68, 170, 74, "male", adjustToFFM = FALSE, creatinine = scr)
  crcl <- egfrMDRD(74, "male", adultEquivalentCreatinine(scr, 74, "male"))
  cl <- 0.278 * (68 / 68)^0.534 * (crcl / 87)^0.532 * (74 / 74)^-0.461
  expected <- list(
    PK = list(default = list(
      v1 = 8.14, v2 = 15.6, v3 = 1,
      cl1 = cl / 60, cl2 = 0.082 / 60, cl3 = 0,
      ka_SC = 0.716 / 60, tlag_SC = 0, bioavailability_SC = 1
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("clearance carries the weight, creatinine-clearance and age exponents", {
  a <- fondaparinux(68, 170, 74, "male", adjustToFFM = FALSE, creatinine = 0.9)
  bw <- fondaparinux(90, 170, 74, "male", adjustToFFM = FALSE, creatinine = 0.9)
  expect_equal(bw$PK$default$cl1 / a$PK$default$cl1, (90 / 68)^0.534, tolerance = 1e-8)
  expect_equal(bw$PK$default$v1  / a$PK$default$v1,  (90 / 68)^0.522, tolerance = 1e-8)
  # Clearance falls with age (negative exponent) at matched creatinine.
  older <- fondaparinux(68, 170, 84, "male", adjustToFFM = FALSE, creatinine = 0.9)
  expect_lt(older$PK$default$cl1,
            fondaparinux(68, 170, 64, "male", adjustToFFM = FALSE,
                         creatinine = 0.9)$PK$default$cl1)
})

test_that("the plotted concentration is plasma only, with no effect site", {
  r <- getDrugPK("fondaparinux", 68, 170, 74, "male")
  expect_equal(r$tPeak, 0)
  expect_equal(r$MEAC, 0)
})

test_that("a subcutaneous mg dose reaches a sensible concentration", {
  # 2.5 mg SC once daily; steady state is in the ~0.3-0.8 mg/L range.
  dose <- data.frame(Drug = "fondaparinux", Time = (0:9) * 1440, Dose = 2.5,
                     Units = "mg SC")
  r <- simulateDrugsWithCovariates(dose, noEvents, 68, 170, 74, "male",
                                   14400, TRUE, creatinine = 0.9,
                                   adjustToFFM = FALSE)$fondaparinux
  pl <- r$results[r$results$Site == "Plasma", ]
  ss <- max(pl$Y[pl$Time > 12960])
  expect_gt(ss, 0.3)
  expect_lt(ss, 1.0)
})
