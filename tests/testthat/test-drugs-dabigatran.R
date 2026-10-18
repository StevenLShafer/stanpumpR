# Dabigatran: Liesenfeld 2011 apparent oral two-compartment model with lag,
# RE-LY atrial-fibrillation patient.  See the header of R/drugs_dabigatran.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("reproduces the RE-LY typical clearance of 69.6 L/h", {
  # Medians: CRCL 68.6 mL/min, age 72, 80.3 kg, male.  A creatinine giving
  # Cockcroft-Gault 68.6 at 80.3 kg, 72 y male anchors the Hill function.
  scr <- (140 - 72) * 80.3 / (72 * 68.6)
  d <- dabigatran(80.3, 176, 72, "male", adjustToFFM = FALSE, creatinine = scr)
  expect_equal(d$PK$default$cl1 * 60, 69.6, tolerance = 0.1)
})

test_that("returns the reference structural parameters with the switch off", {
  scr <- (140 - 72) * 80.3 / (72 * 68.6)
  actual <- dabigatran(80.3, 176, 72, "male", adjustToFFM = FALSE,
                       creatinine = scr)
  crcl <- creatinineClearanceCG(80.3, 72, "male",
                                adultEquivalentCreatinine(scr, 72, "male"))
  cl <- 124 * crcl^1.29 / (56.7^1.29 + crcl^1.29)
  expected <- list(
    PK = list(default = list(
      v1 = 673, v2 = 345, v3 = 1,
      cl1 = cl / 60, cl2 = 35.5 / 60, cl3 = 0,
      ka_PO = 0.754 / 60, tlag_PO = 0.634 * 60, bioavailability_PO = 1
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("the female coefficient lowers clearance by 8.3% at matched CLcr", {
  # Compare at the same CLcr: give the female a creatinine that yields the same
  # Cockcroft-Gault value as the male, so only the 0.917 sex factor differs.
  crcl <- 80
  scrM <- (140 - 70) * 80 / (72 * crcl)
  scrF <- (140 - 70) * 80 * 0.85 / (72 * crcl)
  m <- dabigatran(80, 176, 70, "male",   adjustToFFM = FALSE, creatinine = scrM)
  f <- dabigatran(80, 165, 70, "female", adjustToFFM = FALSE, creatinine = scrF)
  expect_equal(f$PK$default$cl1 / m$PK$default$cl1, 0.917, tolerance = 1e-3)
})

test_that("the age coefficient lowers clearance by 0.41% per year above 72", {
  crcl <- 80
  scr72 <- (140 - 72) * 80 / (72 * crcl)
  scr82 <- (140 - 82) * 80 / (72 * crcl)
  y72 <- dabigatran(80, 176, 72, "male", adjustToFFM = FALSE, creatinine = scr72)
  y82 <- dabigatran(80, 176, 82, "male", adjustToFFM = FALSE, creatinine = scr82)
  expect_equal(y82$PK$default$cl1 / y72$PK$default$cl1,
               1 - 0.0041 * 10, tolerance = 1e-3)
})

test_that("weight enters V2/F at 0.77% per kg above 80.3 kg", {
  scr <- 0.9
  a <- dabigatran(80.3, 176, 72, "male", adjustToFFM = FALSE, creatinine = scr)
  b <- dabigatran(90.3, 176, 72, "male", adjustToFFM = FALSE, creatinine = scr)
  expect_equal(a$PK$default$v1, 673, tolerance = 1e-8)
  expect_equal(b$PK$default$v1, 673 * (1 + 0.0077 * 10), tolerance = 1e-8)
})

test_that("the plotted concentration is plasma only, with no effect site", {
  r <- getDrugPK("dabigatran", 80.3, 176, 72, "male")
  expect_equal(r$tPeak, 0)
  expect_equal(r$MEAC, 0)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y man, switch on: fat-free-mass factors 1.3049067
  # (volumes) and 1.2209126 (clearances), pharmacokinetic weight 91.343469 kg
  # (the factors pinned in test-drugs-alfentanil.R).  Worked out by hand from
  # the published equations, not from the code under test.
  # Cockcroft-Gault 126.866 mL/min: CL/F = 124 x 126.866^1.29 / (56.7^1.29 +
  # 126.866^1.29) x (1 - 0.0041 x (50 - 72)) x 1.2209126; V2/F = 673 x
  # (1 + 0.0077 x (91.343469 - 80.3)); V3/F = 345 x 1.3049067; Q/F = 35.5 x
  # 1.2209126.
  actual <- dabigatran(120, 170, 50, "male")$PK$default
  expect_equal_rounded(actual[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 730.22836, v2 = 450.19281,
                            cl1 = 2.0318601, cl2 = 0.72237329))
})
