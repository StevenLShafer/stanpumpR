# Apixaban: Cirincione 2018 stage 2 apparent oral two-compartment model in
# nonvalvular atrial fibrillation.  See the header of R/drugs_apixaban.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the reference NVAF parameters with the switch off", {
  # Reference NVAF patient: 65 y, 70 kg, CrCL 80 mL/min, non-Asian man.  The
  # creatinine that gives Cockcroft-Gault 80 at 70 kg, 65 y male is
  #   scr = (140 - 65) * 70 / (72 * 80) = 0.911458... mg/dL
  scr <- (140 - 65) * 70 / (72 * 80)
  actual <- apixaban(70, 176, 65, "male", adjustToFFM = FALSE, creatinine = scr)
  clR  <- 1.57 * (80 / 80)
  clNR <- 2.02 * (65 / 65)^-0.429 * 1
  cl   <- (clR + clNR) * (1 - 0.139)
  expected <- list(
    PK = list(default = list(
      v1 = 30.0 * (70 / 70)^0.790 * (1 - 0.0405),
      v2 = 27.0, v3 = 1,
      cl1 = cl / 60, cl2 = 1.91 / 60, cl3 = 0,
      ka_PO = 0.473 / 60, tlag_PO = 0, bioavailability_PO = 1
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    oralSaturation = actual$oralSaturation,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  # The reference NVAF total clearance is 3.59 x 0.861 = 3.09 L/h.
  expect_equal(cl, 3.09, tolerance = 0.005)
})

test_that("renal clearance is linear and unbounded in creatinine clearance", {
  # The published model (Cirincione 2018 Supplementary Eq. 1b) fixes the cCrCL
  # exponent at 1 with no cap, so CL_R scales as (cCrCL / 80) with no ceiling:
  # a very low creatinine (high CrCL, here ~230 mL/min) gives a higher renal
  # clearance than CrCL 80, by exactly the CrCL ratio.
  scr <- 0.3
  crcl <- creatinineClearanceCG(70, 40, "male", adultEquivalentCreatinine(scr, 40, "male"))
  expect_gt(crcl, 150)                                   # well above the old cap
  high <- apixaban(70, 176, 40, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default$cl1
  ref  <- apixaban(70, 176, 40, "male", adjustToFFM = FALSE,
                   creatinine = (140 - 40) * 70 / (72 * 80))$PK$default$cl1  # CrCL 80
  # CL_R rises by the full CrCL ratio; CL_NR is unchanged, so total CL rises.
  expect_gt(high, ref)
  clR_high <- 1.57 * (crcl / 80)
  clR_ref  <- 1.57
  expect_equal((high - ref) * 60, (clR_high - clR_ref) * (1 - 0.139), tolerance = 1e-6)
})

test_that("relative bioavailability falls with the dose, anchored at 2.5 mg", {
  sat <- apixaban(70, 176, 65, "male")$oralSaturation
  f <- oralSaturationFraction(c(2.5, 5, 10, 50), sat)
  expect_equal(f[1], 1)                          # anchored at 2.5 mg
  expect_equal(f, c(1, 0.966, 0.914, 0.580), tolerance = 1e-3)
  expect_true(all(diff(f) < 0))                  # monotone decreasing
})

test_that("5 mg twice daily reproduces the paper's steady-state Cmax", {
  # Table 4: median steady-state Cmax 171 ng/mL for 5 mg bid in NVAF.
  scr <- (140 - 65) * 70 / (72 * 80)
  dose <- data.frame(Drug = "apixaban", Time = (0:19) * 720, Dose = 5, Units = "mg PO")
  r <- simulateDrugsWithCovariates(dose, noEvents, 70, 176, 65, "male",
                                   20 * 720, TRUE, creatinine = scr,
                                   adjustToFFM = FALSE)$apixaban
  pl <- r$results[r$results$Site == "Plasma", ]
  cmax <- max(pl$Y[pl$Time > 12 * 720])
  expect_equal(cmax, 171, tolerance = 10)
})

test_that("the plotted concentration is plasma only, with no effect site", {
  r <- getDrugPK("apixaban", 70, 176, 65, "male")
  expect_equal(r$tPeak, 0)
  expect_equal(r$MEAC, 0)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y man, switch on: fat-free-mass factors 1.3049067
  # (volumes) and 1.2209126 (clearances), pharmacokinetic weight 91.343469 kg
  # (the factors pinned in test-drugs-alfentanil.R).  Worked out by hand from
  # the published equations, not from the code under test.
  # Cockcroft-Gault at the assumed 0.9 mg/dL: 90 x 91.343469 / 64.8 = 126.866
  # mL/min.  CL_R/F = 1.57 x 126.866/80; CL_NR/F = 2.02 x (50/65)^-0.429 x
  # 1.2209126; both x 0.861 (NVAF).  Vc/F = 30 x (91.343469/70)^0.790 x 0.9595;
  # Vp/F = 27 x 1.3049067; Q/F = 1.91 x 1.2209126.
  actual <- apixaban(120, 170, 50, "male")$PK$default
  expect_equal_rounded(actual[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 35.520086, v2 = 35.232481,
                            cl1 = 0.075334605, cl2 = 0.038865718))
})
