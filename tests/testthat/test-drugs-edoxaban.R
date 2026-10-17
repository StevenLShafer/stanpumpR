# Edoxaban: Niebecker 2015 apparent oral two-compartment model with lag,
# Hokusai-VTE phase 3 patient.  See the header of R/drugs_edoxaban.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the reference parameters with the switch off (CLcr <= 90)", {
  # 70 kg (allometric centre), 60 y, non-Asian male; a creatinine giving
  # Cockcroft-Gault CLcr 80 mL/min (below the piece-wise breakpoint of 90).
  scr <- (140 - 60) * 70 / (72 * 80)
  actual <- edoxaban(70, 176, 60, "male", adjustToFFM = FALSE, creatinine = scr)
  crcl <- creatinineClearanceCG(70, 60, "male",
                                adultEquivalentCreatinine(scr, 60, "male"))
  expected <- list(
    PK = list(default = list(
      v1 = 209, v2 = 92.3, v3 = 1,
      cl1 = (15.2 + 0.202 * crcl) / 60, cl2 = 5.91 / 60, cl3 = 0,
      ka_PO = 3.36 / 60, tlag_PO = 0.250 * 60, bioavailability_PO = 1
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("renal clearance is piece-wise linear with the phase 3 slope above 90", {
  # Above CLcr 90 the second slope is the phase 3 value 0.0321 x 2.74 = 0.0880,
  # applied as (slope1 - slope2)*90 + slope2*CLcr.
  scr <- (140 - 50) * 80 / (72 * 120)
  crcl <- creatinineClearanceCG(80, 50, "male",
                                adultEquivalentCreatinine(scr, 50, "male"))
  expect_gt(crcl, 90)
  cl <- edoxaban(80, 176, 50, "male", adjustToFFM = FALSE,
                 creatinine = scr)$PK$default$cl1 * 60
  s2 <- 0.0321 * 2.74
  expected <- (15.2 + (0.202 - s2) * 90 + s2 * crcl) * (80 / 70)^0.75
  expect_equal(cl, expected, tolerance = 1e-8)
})

test_that("the renal slope is continuous at the breakpoint of 90 mL/min", {
  # Just below and just above CLcr 90 give the same renal clearance.
  below <- (15.2 + 0.202 * 89.999)
  s2 <- 0.0321 * 2.74
  above <- (15.2 + (0.202 - s2) * 90 + s2 * 90.001)
  expect_equal(below, above, tolerance = 1e-2)
})

test_that("allometric weight scales clearances by 0.75 and volumes by 1", {
  # Two weights at a fixed creatinine; compare the switch-off (published) scaling.
  scr <- 0.9
  a <- edoxaban(70, 176, 60, "male", adjustToFFM = FALSE, creatinine = scr)
  b <- edoxaban(140, 176, 60, "male", adjustToFFM = FALSE, creatinine = scr)
  expect_equal(b$PK$default$v1 / a$PK$default$v1, 140 / 70, tolerance = 1e-8)
  expect_equal(b$PK$default$cl2 / a$PK$default$cl2, (140 / 70)^0.75,
               tolerance = 1e-8)
})

test_that("the plotted concentration is plasma only, with no effect site", {
  r <- getDrugPK("edoxaban", 70, 176, 60, "male")
  expect_equal(r$tPeak, 0)
  expect_equal(r$MEAC, 0)
})
