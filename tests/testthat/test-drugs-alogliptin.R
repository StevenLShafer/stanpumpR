# alogliptin: see R/drugs_alogliptin.R (Naik 2016, Table 3, Eq. 6 and 7).
noEvents <- data.frame(Time = numeric(0), Event = character(0))

# the creatinine giving a Cockcroft-Gault clearance of `crcl` mL/min
scrFor <- function(crcl, weight = 84.9, age = 50, sex = "male")
  (140 - age) * weight / (72 * crcl) * (if (sex == "female") 0.85 else 1)

test_that("section 3.2: CL/F 20.2, 16.7, 13.2 and 4.1 L/h at CLcr 100, 70, 45 and 15", {
  cl <- function(crcl) alogliptin(84.9, 175, 50, "male", FALSE,
                                  creatinine = scrFor(crcl))$PK$default$cl1 * 60
  expect_equal(round(cl(100), 1), 20.2)
  expect_equal(round(cl(70), 1), 16.7)
  expect_equal(round(cl(45), 1), 13.2)
  expect_equal(round(cl(15), 1), 4.1)
  expect_equal(cl(72.95), 17.1, tolerance = 1e-8)
})

test_that("Table 3 and Eq. 6-7", {
  p <- alogliptin(100, 175, 50, "male", FALSE, creatinine = 1)$PK$default
  crcl <- (140 - 50) * 100 / 72
  expect_equal(p$cl1 * 60, 17.1 * (crcl / 72.95)^0.534 * (100 / 84.9)^0.345)
  expect_equal(p$v1, 194 * (100 / 84.9)^1.11)
  expect_equal(p$v2, 149)
  expect_equal(p$cl2 * 60, 10.2)
  expect_equal(p$ka_PO * 60, 2.5)
  expect_equal(p$bioavailability_PO, 1)
  expect_equal(p$tlag_PO, 0)
  # switch on: the pharmacokinetic weight in the weight terms and in Cockcroft-Gault
  size <- pkSizeFactors(120, 175, 50, "male", TRUE)
  on <- alogliptin(120, 175, 50, "male", creatinine = 1)$PK$default
  crclOn <- (140 - 50) * size$pkWeight / 72
  expect_equal(on$cl1 * 60, 17.1 * (crclOn / 72.95)^0.534 * (size$pkWeight / 84.9)^0.345)
  expect_equal(on$v1, 194 * (size$pkWeight / 84.9)^1.11)
  expect_equal_rounded(alogliptin(70, 170, 40, "male")$PK$default$cl1 * 60, 19.7300308)
})

test_that("severe renal impairment: the coefficient is 9.61 below a CLcr of 30, as published", {
  cl <- function(crcl) alogliptin(84.9, 175, 50, "male", FALSE,
                                  creatinine = scrFor(crcl))$PK$default$cl1 * 60
  expect_equal(cl(29.999), 9.61 * (29.999 / 72.95)^0.534, tolerance = 1e-8)
  expect_equal(cl(30.001), 17.1 * (30.001 / 72.95)^0.534, tolerance = 1e-8)
  expect_equal(cl(29.999) / cl(30.001), 9.61 / 17.1, tolerance = 1e-4)   # a 44% step
})

test_that("pregnancy: the rise in filtration when the creatinine is blank", {
  base <- alogliptin(70, 165, 30, "female")$PK$default
  preg <- alogliptin(70, 165, 30, "female", pregnant = TRUE)$PK$default
  crcl <- creatinineClearanceCG(pkSizeFactors(70, 165, 30, "female", TRUE)$pkWeight, 30,
                                "female", adultEquivalentCreatinine(NULL, 30, "female"))
  expect_equal(preg$cl1 / base$cl1, 1.5^0.534)
  expect_gt(crcl, 30)
  # an entered creatinine already carries it
  expect_equal(alogliptin(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default,
               alogliptin(70, 165, 30, "female", creatinine = 0.6)$PK$default)
})

test_that("a 25 mg dose peaks within 1-2 h (the label's tmax)", {
  PK <- getDrugPK("alogliptin", 84.9, 175, 50, "male", adjustToFFM = FALSE)
  w <- withOutputTimes(0:1440, simCpCe(
    data.frame(Drug = "alogliptin", Time = 0, Dose = 25, Units = "mg PO"),
    noEvents, PK, 1440, FALSE))$wide
  w <- w[!duplicated(w$Time), ]
  tmax <- w$Time[which.max(w$Plasma)] / 60
  expect_gt(tmax, 1); expect_lt(tmax, 2)
})

test_that("units: oral, daily", {
  expect_equal(getDrugDefaults("alogliptin")$Units[[1]], c("mg PO", "mg PO qd"))
})

test_that("Vp/F and Q/F take the library's factors with the switch on (no size term)", {
  size <- pkSizeFactors(120, 175, 50, "male", TRUE)
  on <- alogliptin(120, 175, 50, "male", creatinine = 1)$PK$default
  expect_equal(on$v2, 149 * size$volume)
  expect_equal(on$cl2 * 60, 10.2 * size$clearance)
  # pinned: FFM 73.456 kg against the reference man's 54.5, volume x 1.348,
  # clearance x 1.348^0.75
  expect_equal_rounded(on$v2, 200.9172051)
  expect_equal_rounded(on$cl2 * 60, 12.7636039)
  off <- alogliptin(120, 175, 50, "male", FALSE, creatinine = 1)$PK$default
  expect_equal(off$v2, 149)
  expect_equal(off$cl2 * 60, 10.2)
})
