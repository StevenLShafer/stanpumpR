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

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y man, switch on: fat-free-mass factors 1.3049067
  # (volumes) and 1.2209126 (clearances), pharmacokinetic weight 91.343469 kg
  # (the factors pinned in test-drugs-alfentanil.R).  Worked out by hand from
  # the published equations, not from the code under test.
  # Simplified MDRD at the assumed 0.9 mg/dL and 50 y: 89.321 mL/min/1.73 m^2.
  # Cl/F = 0.278 x (91.343469/68)^0.534 x (89.321/87)^0.532 x (50/74)^-0.461;
  # V2/F = 8.14 x (91.343469/68)^0.522; V3/F = 15.6 x 1.3049067; Q/F = 0.082 x
  # 1.2209126.
  actual <- fondaparinux(120, 170, 50, "male")$PK$default
  expect_equal_rounded(actual[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 9.4957311, v2 = 20.356545,
                            cl1 = 0.0065903437, cl2 = 0.0016685806))
})

test_that("pregnancy: creatinine clearance x 1.5 when the creatinine is blank", {
  # A woman of 30 years, 70 kg, 165 cm, creatinine blank: the MDRD creatinine
  # clearance in the renal term x PREGNANCY_GFR_FACTOR, so CL/F x 1.5^0.532
  # (R/drugs_fondaparinux.R, PREGNANCY); nothing else moves.
  on  <- getDrugPK("fondaparinux", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("fondaparinux", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("fondaparinux", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, PREGNANCY_GFR_FACTOR^0.532, tolerance = 1e-6)
  same <- c("v1", "v2", "v3", "cl2", "cl3", "ka_RA", "bioavailability_RA",
            "tlag_RA")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  expect_equal(PREGNANCY_GFR_FACTOR^0.532, 1.241, tolerance = 1e-3)
  # A measured creatinine already carries the rise: no pregnancy change
  cr <- function(p) getDrugPK("fondaparinux", 70, 165, 30, SEX_FEMALE, pregnant = p,
                              creatinine = 0.6)
  expect_identical(cr(TRUE)$PK, cr(FALSE)$PK)
  expect_no_match(cr(TRUE)$reference, "Wu Y", fixed = TRUE)
  expect_match(on$reference, "Wu Y", fixed = TRUE)
  expect_no_match(off$reference, "Wu Y", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- fondaparinux(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(7.384643853, actual$v1)
  expect_equal_rounded(12.57508283, actual$v2)
  expect_equal_rounded(1, actual$v3)
  expect_equal_rounded(0.007754777382, actual$cl1)
  expect_equal_rounded(0.001162658829, actual$cl2)
  expect_equal_rounded(0, actual$cl3)
})
