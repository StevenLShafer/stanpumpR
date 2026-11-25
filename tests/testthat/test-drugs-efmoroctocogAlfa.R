# efmoroctocogAlfa: Allard 2020's factor VIII model with EHL = 1; see
# R/drugs_efmoroctocogAlfa.R and R/drugs_factorVIII.R.
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("the standard model's clearance times exp(-0.394), nothing else changed", {
  for (p in list(c(15.1, 3), c(64, 30), c(130, 77))) for (ffm in c(FALSE, TRUE)) {
    e <- efmoroctocogAlfa(p[1], 170, p[2], "male", ffm)$PK$default
    s <- factorVIII(p[1], 170, p[2], "male", ffm)$PK$default
    expect_equal(e$cl1, s$cl1 * exp(-0.394))
    expect_equal(c(e$v1, e$cl2, e$v2), c(s$v1, s$cl2, s$v2))
  }
})

test_that("pinned values: the reference man with the switch off, a heavy woman with it on", {
  # switch off, 70 kg, 35 years: CL = 204 x (35/30)^-0.214 x (70/64)^0.75 x exp(-0.394) mL/h,
  # V1 = 2640 x (70/64)^0.827 mL, Q 135 mL/h, V2 339 mL
  actual <- efmoroctocogAlfa(70, 170, 35, "male", FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 2.843081, v2 = 0.339, v3 = 1,
      cl1 = 0.002372624, cl2 = 0.00225, cl3 = 0
    )),
    tPeak = 0, MEAC = 0,
    typical = 100, upperTypical = 120, lowerTypical = 80,
    reference = paste0(
      "Allard 2020 (258 patients with haemophilia A, 3-77 years, eight concentrates): ",
      "two compartments, CL 204 mL/h x (age/30)^-0.214 x (weight/64)^0.75 x exp(-0.394) ",
      "(Elocta, extended half-life), V1 2640 mL x (weight/64)^0.827, Q 135 mL/h, V2 339 mL; ",
      "ELOCTATE (efmoroctocog alfa). Plotted as the patient's own factor VIII activity plus ",
      "the infused: baseline 0.5 IU/dL (blank: severe haemophilia A, the source population). ",
      "Band: the label's perioperative target for major surgery, 80-120 IU/dL. ",
      "https://doi.org/10.3390/pharmaceutics12040380"),
    endogenousActivity = 0.5
  )
  expect_equal_rounded(actual, expected)
  pk <- efmoroctocogAlfa(110, 165, 50, "female", TRUE)$PK$default
  expect_equal_rounded(c(pk$v1, pk$v2, pk$cl1, pk$cl2),
                       c(2.880294, 0.3443727, 0.002224342, 0.002276692))
})

test_that("the checks in the header, at the source's medians", {
  pk <- efmoroctocogAlfa(64, 175, 30, "male", FALSE)$PK$default
  k10 <- pk$cl1 / pk$v1; k12 <- pk$cl2 / pk$v1; k21 <- pk$cl2 / pk$v2
  b <- k10 + k12 + k21
  lambda <- (b - sqrt(b^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / lambda / 60, 15.23, tolerance = 0.002)   # h
  expect_equal(pk$cl1 * 60 * 1000 / 64, 2.15, tolerance = 0.002)  # mL/kg/h
  # 1.47 times the standard product's half-life (the source's switch: 1.48)
  s <- factorVIII(64, 175, 30, "male", FALSE)$PK$default
  k10 <- s$cl1 / s$v1; b <- k10 + k12 + k21
  expect_equal(lambda / ((b - sqrt(b^2 - 4 * k10 * k21)) / 2), 1 / 1.471, tolerance = 0.002)
})

test_that("it reads the Factor VIII baseline and plots above it", {
  expect_true(all(c("factorVIII", "efmoroctocogAlfa") %in% patientFieldReaders("factorVIIIBaseline")))
  PK <- getDrugPK("efmoroctocogAlfa", 70, 175, 35, "male",
                  factorBaselines = list(factorVIIIBaseline = 15))
  expect_equal(PK$endogenousActivity, 15)
  w <- simCpCe(data.frame(Drug = "efmoroctocogAlfa", Time = 60, Dose = 50, Units = "IU/kg"),
               noEvents, PK, 120, FALSE)$wide
  expect_equal(w$Plasma[w$Time < 60], rep(15, sum(w$Time < 60)))
})
