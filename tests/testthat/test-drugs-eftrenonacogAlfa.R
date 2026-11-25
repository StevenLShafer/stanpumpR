# eftrenonacogAlfa: see R/drugs_eftrenonacogAlfa.R (Koopman 2024, Table 2 and
# the equations under it).
koopman <- function(w, age) {
  r <- w / 73
  list(cl = 1.41 * r^0.75 * (1 - 0.0047 * (age - 15.8)) / 10 / 60,
       v1 = 7.31 * r, q = 0.277 * r^0.75 / 60, v2 = 8.01 * r)
}
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 2's equations, switch off", {
  for (p in list(c(12, 2), c(73, 15.8), c(103, 71), c(70, 90))) {
    pk <- eftrenonacogAlfa(p[1], 170, p[2], "male", FALSE)$PK$default
    ref <- koopman(p[1], p[2])
    expect_equal(c(pk$cl1, pk$v1, pk$cl2, pk$v2), c(ref$cl, ref$v1, ref$q, ref$v2))
  }
})

test_that("the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(110, 170, 40, "male", TRUE)
  pk <- eftrenonacogAlfa(110, 170, 40, "male", TRUE)$PK$default
  ref <- koopman(size$pkWeight, 40)
  expect_equal(c(pk$cl1, pk$v1, pk$cl2, pk$v2), c(ref$cl, ref$v1, ref$q, ref$v2))
})

test_that("the checks in the header, at 73 kg and 15.8 years", {
  pk <- eftrenonacogAlfa(73, 175, 15.8, "male", FALSE)$PK$default
  k10 <- pk$cl1 / pk$v1; k12 <- pk$cl2 / pk$v1; k21 <- pk$cl2 / pk$v2
  b <- k10 + k12 + k21
  lambda <- (b - sqrt(b^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / lambda / 60, 87.08, tolerance = 0.002)        # h
  expect_equal(pk$cl1 * 60 * 1000 / 73, 1.93, tolerance = 0.002)       # mL/h/kg
  expect_equal((pk$v1 + pk$v2) * 1000 / 73, 209.9, tolerance = 0.002)  # mL/kg
  # the text's 1.38 and 1.05 dL/h at 20 and 70 years, from the equation
  cl <- function(a) eftrenonacogAlfa(73, 175, a, "male", FALSE)$PK$default$cl1 * 600
  expect_equal(c(cl(20), cl(70)), c(1.382, 1.051), tolerance = 0.001)
})

test_that("50 IU/kg at 73 kg: baseline plus 1.00 IU/dL per IU/kg", {
  PK <- getDrugPK("eftrenonacogAlfa", 73, 175, 15.8, "male", adjustToFFM = FALSE)
  expect_equal(PK$endogenousActivity, 0.5)
  w <- simCpCe(data.frame(Drug = "eftrenonacogAlfa", Time = 0, Dose = 50, Units = "IU/kg"),
               noEvents, PK, 1440, FALSE)$wide
  expect_equal(w$Plasma[w$Time == 0][1], 0.5 + 50 * 73 / 73.1, tolerance = 1e-6)
})

test_that("the Factor IX baseline field is read by this drug and not by factor VIII", {
  expect_identical(patientFieldReaders("factorIXBaseline"), "eftrenonacogAlfa")
  pk <- function(drug) getDrugPK(drug, 70, 175, 35, "male",
                                 factorBaselines = list(factorIXBaseline = 20,
                                                        factorVIIIBaseline = 7))
  expect_equal(pk("eftrenonacogAlfa")$endogenousActivity, 20)
  expect_equal(pk("factorVIII")$endogenousActivity, 7)
})
