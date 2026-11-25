# Amikacin (R/drugs_amikacin.R): Tod 1998's two compartments, clearance
# linear in Cockcroft-Gault creatinine clearance.  Drafted by Claude Code,
# 2026-11-12.  Expectations from the source (PMC105554), not the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Tod 1998's parameters with the switch off", {
  set <- amikacin(65, 170, 50, "male", adjustToFFM = FALSE, creatinine = 1)$PK$default
  crcl <- (140 - 50) * 65 / (72 * 1)
  expect_equal(set$cl1, (0.367 * crcl * 60 / 1000 + 1.40) / 60)
  expect_equal(set$v1, 8.92)
  expect_equal(set$cl2, 4.43 / 60)
  expect_equal(set$v2, 11.4)
})

test_that("no renal function leaves the 1.40 L/h intercept", {
  expect_equal(amikacin(65, 170, 50, "male", adjustToFFM = FALSE,
                        creatinine = 1e6)$PK$default$cl1, 1.40 / 60, tolerance = 1e-4)
  # With the switch on the intercept, a fixed value, scales with fat-free
  # mass (0.75 power), anchored at Tod's cohort since 2026-11-20; the renal
  # term, whose CLcr carries the weight, does not
  r <- ffmRatioMaleIndependent(120, 170, 50)
  kc <- cohortFactorsIndependent(AMIKACIN_COHORT)
  on <- amikacin(120, 170, 50, "male", creatinine = 1)$PK$default
  pkW <- 70 * r
  crcl <- (140 - 50) * pkW / 72
  expect_equal(on$cl1, (0.367 * crcl * 60 / 1000 + 1.40 * r^0.75 / kc$clearance) / 60,
               tolerance = 1e-8)
})

test_that("anchored at Tod's cohort: its typical patients get Table 4, FFM scales the rest", {
  # (a) Table 1 (PDF p2): o.d. 16 men and 12 women, 65.8 kg, 50.2 y; b.i.d.
  # 19 men and 10 women, 68.1 kg, 51.3 y; heights from NCD-RisC (France,
  # born 1945 and 1943).  No parameter has a sex term, so the four typical
  # patients pooled geometrically by their numbers receive Table 4
  kc <- cohortFactorsIndependent(AMIKACIN_COHORT)
  expect_equal(round(c(kc$volume, kc$clearance), 4), c(0.8921, 0.9179))
  g <- list(c(65.8, 172.5, 50.2, 16, 1), c(65.8, 160.2, 50.2, 12, 0),
            c(68.1, 172.1, 51.3, 19, 1), c(68.1, 160.0, 51.3, 10, 0))
  pooled <- function(field, creatinine = 1) {
    s <- 0
    for (x in g) s <- s + x[4] * log(amikacin(x[1], x[2], x[3], if (x[5] == 1) "male" else "female",
                                              creatinine = creatinine)$PK$default[[field]])
    exp(s / 57)
  }
  expect_equal(pooled("v1"), 8.92)
  expect_equal(pooled("v2"), 11.4)
  expect_equal(pooled("cl2") * 60, 4.43)
  # with no renal function the clearance is the anchored intercept
  expect_equal(pooled("cl1", creatinine = 1e9) * 60, 1.40, tolerance = 1e-6)
  # (b) twice the reference fat-free mass: volumes x 2, CLD and the intercept
  # x 2^0.75, over the cohort's factors; the renal term at 140 kg, unanchored
  w <- uniroot(function(w) ffmAlSallami(w, 200, 50, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- amikacin(w, 200, 50, "male", creatinine = 1)$PK$default
  expect_equal(c(big$v1, big$v2), c(8.92, 11.4) * 2 / kc$volume)
  expect_equal(big$cl2 * 60, 4.43 * 2^0.75 / kc$clearance)
  crcl <- (140 - 50) * 140 / 72
  expect_equal(big$cl1 * 60, 0.367 * crcl * 60 / 1000 + 1.40 * 2^0.75 / kc$clearance)
  # (c) the switch off: Table 4 at any size, CLcr at total weight
  off <- amikacin(w, 200, 50, "male", adjustToFFM = FALSE, creatinine = 1)$PK$default
  expect_equal(c(off$v1, off$v2, off$cl2 * 60), c(8.92, 11.4, 4.43))
  expect_equal(off$cl1 * 60, 0.367 * (140 - 50) * w / 72 * 60 / 1000 + 1.40)
})

test_that("20 mg/kg gives a peak near Tod's measured first-dose peaks", {
  # Table 2: first-dose 1-h peaks after 20 mg/kg over 30 min, mean 75.4,
  # median 66.7 (43-170) mg/L
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "amikacin", Time = c(0, 30), Dose = c(40, 0), Units = "mg/kg/hr"),
    events, 65, 170, 50, "male", 240, FALSE, adjustToFFM = FALSE, creatinine = 0.9
  )$amikacin$wide
  at1h <- approx(wide$Time, wide$Plasma, 60)$y
  expect_true(at1h > 43 && at1h < 120)
})

test_that("the MIC line and the MBC band, on total drug", {
  mic <- antibioticMic("amikacin")
  expect_equal(c(mic$MIC, mic$FreeFraction, mic$Threshold), c(4, 0.82, 4.9))
  expect_equal(unname(antibioticMbcBand("amikacin")$plotted), signif(c(2, 4, 16) / 0.82, 2))
})
