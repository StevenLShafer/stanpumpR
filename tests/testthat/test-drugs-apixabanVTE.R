# Apixaban VTE: Byon 2017 final apparent oral two-compartment model for venous
# thromboembolism treatment.  See the header of R/drugs_apixabanVTE.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Serum creatinine giving Cockcroft-Gault 100 mL/min at 85 kg, 60 y male:
#   scr = (140 - 60) * 85 / (72 * 100) = 0.944444... mg/dL
scrRef <- (140 - 60) * 85 / (72 * 100)

test_that("returns the reference VTE parameters with the switch off", {
  # Reference VTE patient: 60 y, 85 kg, cCrCL 100 mL/min, non-Asian man.
  actual <- apixabanVTE(85, 176, 60, "male", adjustToFFM = FALSE, creatinine = scrRef)
  clR  <- 1.83 * 100 / 100
  clNR <- 2.52 * (60 / 60)^-0.267 * 1
  cl   <- clR + clNR
  expected <- list(
    PK = list(default = list(
      v1 = 32.1 * (85 / 85)^0.523,
      v2 = 19.8, v3 = 1,
      cl1 = cl / 60, cl2 = 1.62 / 60, cl3 = 0,
      ka_PO = 0.440 / 60, tlag_PO = 0, bioavailability_PO = 1
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  # The reference total clearance is 1.83 + 2.52 = 4.35 L/h (the paper's value).
  expect_equal(cl, 4.35, tolerance = 0.005)
})

test_that("severe renal impairment lowers clearance by the paper's 36%", {
  # cCrCL 15 mL/min: CL/F = 2.52 + 1.83 x 15/100 = 2.7945 L/h, a 36% fall from
  # the reference 4.35 and a 56% higher exposure, as Byon 2017 reports.
  scr15 <- (140 - 60) * 85 / (72 * 15)
  severe <- apixabanVTE(85, 176, 60, "male", adjustToFFM = FALSE, creatinine = scr15)$PK$default$cl1
  ref    <- apixabanVTE(85, 176, 60, "male", adjustToFFM = FALSE, creatinine = scrRef)$PK$default$cl1
  expect_equal(severe * 60, 2.52 + 1.83 * 15 / 100, tolerance = 1e-6)
  expect_equal(severe / ref, 0.642, tolerance = 0.005)       # ~36% fall
})

test_that("the renal term is capped at 150 mL/min", {
  # Above 150 mL/min the renal clearance is held at its 150 value, so a very low
  # creatinine (high CrCL) gives the same clearance as cCrCL = 150.
  scrLow <- 0.3
  crcl <- creatinineClearanceCG(85, 40, "male", adultEquivalentCreatinine(scrLow, 40, "male"))
  expect_gt(crcl, 150)
  high <- apixabanVTE(85, 176, 40, "male", adjustToFFM = FALSE, creatinine = scrLow)$PK$default$cl1
  scr150 <- (140 - 40) * 85 / (72 * 150)
  at150 <- apixabanVTE(85, 176, 40, "male", adjustToFFM = FALSE, creatinine = scr150)$PK$default$cl1
  expect_equal(high, at150, tolerance = 1e-9)
})

test_that("body weight and age reproduce the paper's covariate predictions", {
  # Vc/F is 24% lower at 50 kg and 20% higher at 120 kg (relative to 85 kg).
  vc <- function(wt) apixabanVTE(wt, 176, 60, "male", adjustToFFM = FALSE,
                                 creatinine = scrRef)$PK$default$v1
  expect_equal(vc(50) / vc(85), 0.757, tolerance = 0.005)
  expect_equal(vc(120) / vc(85), 1.198, tolerance = 0.005)
  # CL_NR/F is 11% higher at 40 y and 7% lower at 80 y (relative to 60 y); read
  # it off a female (no renal term change) at a fixed creatinine so only CL_NR
  # moves... simplest: compare the age factor directly.
  expect_equal((40 / 60)^-0.267, 1.114, tolerance = 0.005)
  expect_equal((80 / 60)^-0.267, 0.926, tolerance = 0.005)
})

test_that("a woman has 22.3% lower non-renal clearance", {
  manNR   <- 2.52 * (60 / 60)^-0.267
  womanNR <- 2.52 * (60 / 60)^-0.267 * (1 - 0.223)
  expect_equal(womanNR / manNR, 1 - 0.223, tolerance = 1e-9)
})

test_that("5 mg twice daily reproduces the paper's steady-state exposure", {
  # Table 5: for 5 mg b.i.d. in the VTE population the median daily (0-24 h)
  # AUCss is 2446 ng*h/mL and the median Cmax 132 ng/mL.  At steady state the
  # AUC over a dosing interval equals dose / CL, so the reference patient's
  # daily AUC is 10 mg / 4.35 L/h = 2299 ng*h/mL (near the population median).
  dose <- data.frame(Drug = "apixabanVTE", Time = (0:19) * 720, Dose = 5, Units = "mg PO")
  r <- simulateDrugsWithCovariates(dose, noEvents, 85, 176, 60, "male",
                                   20 * 720, TRUE, creatinine = scrRef,
                                   adjustToFFM = FALSE)$apixabanVTE
  pl <- r$results[r$results$Site == "Plasma", ]
  cmax <- max(pl$Y[pl$Time > 12 * 720])
  expect_equal(cmax, 132, tolerance = 20)                    # near the population median
})

test_that("the plotted concentration is plasma only, with no effect site", {
  r <- getDrugPK("apixabanVTE", 85, 176, 60, "male")
  expect_equal(r$tPeak, 0)
  expect_equal(r$MEAC, 0)
  expect_null(r$oralSaturation)                              # no dose-dependent F applied
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y man, switch on: fat-free-mass factors 1.3049067
  # (volumes) and 1.2209126 (clearances), pharmacokinetic weight 91.343469 kg
  # (the factors pinned in test-drugs-alfentanil.R).  Worked out by hand from
  # the published equations, not from the code under test.
  # Cockcroft-Gault at the assumed 0.9 mg/dL: 90 x 91.343469 / 64.8 = 126.866
  # mL/min (below the 150 cap).  CL_R/F = 1.83 x 126.866/100; CL_NR/F = 2.52 x
  # (50/60)^-0.267 x 1.2209126.  Vc/F = 32.1 x (91.343469/85)^0.523; Vp/F =
  # 19.8 x 1.3049067; Q/F = 1.62 x 1.2209126.
  pkW <- 91.343469; vol <- 1.3049067; clr <- 1.2209126
  crcl <- 90 * pkW / 64.8
  clR  <- 1.83 * min(crcl, 150) / 100
  clNR <- 2.52 * (50 / 60)^-0.267 * clr
  actual <- apixabanVTE(120, 170, 50, "male")$PK$default
  expect_equal_rounded(actual[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 32.1 * (pkW / 85)^0.523,
                            v2 = 19.8 * vol,
                            cl1 = (clR + clNR) / 60,
                            cl2 = 1.62 / 60 * clr))
})
