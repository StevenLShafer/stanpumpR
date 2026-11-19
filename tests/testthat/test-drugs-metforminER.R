# metforminER: see R/drugs_metforminER.R (Duong 2013, Table 2, Eq. 1 and 2).
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 2 and Eq. 1-2 at a stated patient", {
  m <- metforminER(70, 170, 40, "male", adjustToFFM = FALSE, creatinine = 0.9)
  p <- m$PK$default
  lbw <- 9270 * 70 / (6680 + 216 * 70 / 1.7^2)          # Janmahasatian, men
  crcl <- (140 - 40) * lbw / (72 * 0.9)                  # Cockcroft-Gault
  expect_equal(lbwJanmahasatian(70, 170, "male"), lbw)
  expect_equal(p$cl1 * 60, 1212 * 60 / 1000 * crcl / 100)
  expect_equal(p$v1, 149)
  expect_equal(p$cl2 * 60, 203 * 60 / 1000)
  expect_equal(p$v2, 182)
  expect_equal(m$oralZeroOrder$duration, 9.55 * 60)
  expect_equal(m$oralZeroOrder$lag, 0)
  expect_equal(m$oralZeroOrder$relativeF$f, c(1.26, 1.16, 1.06, 1))
  expect_equal_rounded(p$cl1 * 60, 61.1333216)
  # women: the Janmahasatian and Cockcroft-Gault terms for women
  f <- metforminER(60, 160, 40, "female", adjustToFFM = FALSE, creatinine = 0.8)$PK$default
  lbwF <- 9270 * 60 / (8780 + 244 * 60 / 1.6^2)
  expect_equal(f$cl1 * 60, 1212 * 60 / 1000 * 0.85 * (100 * lbwF / (72 * 0.8)) / 100)
  # V1/F on total weight with the switch off, the pharmacokinetic weight on
  size <- pkSizeFactors(110, 175, 50, "male", TRUE)
  expect_equal(metforminER(110, 175, 50, "male")$PK$default$v1, 149 * size$pkWeight / 70)
  expect_equal(metforminER(110, 175, 50, "male", FALSE)$PK$default$v1, 149 * 110 / 70)
})

test_that("Duong's patient with end-stage kidney disease: Cav,ss 7.7 mg/L at 2000 mg", {
  # CLcr 15 mL/min: CL/F = 1212 x 0.15 mL/min
  cl <- 1212 * 0.15 * 60 / 1000                           # L/h
  expect_equal(2000 / (cl * 24), 7.7, tolerance = 0.02)
})

test_that("the release: a zero-order input over 9.55 h, scaled by the relative F", {
  expect_equal(oralZeroOrderFraction(c(250, 500, 750, 1000, 1500, 2000, 2500),
                                     METFORMIN_ER_RELATIVE_F),
               c(1.26, 1.26, 1.21, 1.16, 1.06, 1, 1))
  # steady state: the average over a day is dose x F / (CL/F x 24 h)
  PK <- getDrugPK("metforminER", 70, 170, 40, "male", adjustToFFM = FALSE, creatinine = 0.9)
  days <- 10
  for (dose in c(500, 2000)) {
    w <- withOutputTimes(seq(0, days * 1440, by = 5), simCpCe(
      data.frame(Drug = "metforminER", Time = (seq_len(days) - 1) * 1440, Dose = dose,
                 Units = "mg PO"), noEvents, PK, days * 1440, FALSE))$wide
    w <- w[!duplicated(w$Time), ]
    s <- w[w$Time >= (days - 1) * 1440, ]
    cav <- sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 1440
    f <- if (dose == 500) 1.26 else 1
    expect_equal(cav, dose * f / (PK$PK$default$cl1 * 1440), tolerance = 0.01, info = dose)
  }
  # nothing reaches the plasma before the dose, and the input lasts 9.55 h
  w <- withOutputTimes(0:1440, simCpCe(
    data.frame(Drug = "metforminER", Time = 0, Dose = 1000, Units = "mg PO"),
    noEvents, PK, 1440, FALSE))$wide
  w <- w[!duplicated(w$Time), ]
  expect_equal(w$Plasma[w$Time == 0], 0)
  expect_gt(w$Time[which.max(w$Plasma)], 9 * 60)
  expect_lt(w$Time[which.max(w$Plasma)], 10 * 60)
})

test_that("the relativeF block is validated", {
  expect_error(validateOralZeroOrder(list(duration = 60, relativeF = list(dose = c(2, 1), f = c(1, 1))), "x"),
               "relativeF")
  expect_error(validateOralZeroOrder(list(duration = 60, relativeF = list(dose = 1, f = -1)), "x"),
               "relativeF")
  expect_null(validateOralZeroOrder(list(duration = 60), "x")$relativeF)
})

test_that("pregnancy: metformin's factors", {
  base <- metforminER(70, 170, 30, "female", FALSE)$PK$default
  preg <- metforminER(70, 170, 30, "female", FALSE, pregnant = TRUE)$PK$default
  expect_equal(preg$cl1 / base$cl1, METFORMIN_PREGNANCY_CL_FACTOR)
  expect_equal(preg$v1 / base$v1, METFORMIN_PREGNANCY_V_FACTOR)
  expect_equal(preg$v2 / base$v2, METFORMIN_PREGNANCY_V_FACTOR)
  expect_equal(preg$cl2, base$cl2)
})

test_that("units: oral, daily", {
  expect_equal(getDrugDefaults("metforminER")$Units[[1]], c("mg PO", "mg PO qd"))
})
