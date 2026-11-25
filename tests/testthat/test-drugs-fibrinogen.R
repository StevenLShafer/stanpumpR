# fibrinogen: see R/drugs_fibrinogen.R (Bellon 2020, Table 2, equations 4-5).
bellon <- function(w) list(cl = 0.00288 * w^0.556 / 60, v = 0.0960 * w^0.808)
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("equations 4 and 5, switch off", {
  for (w in c(10.8, 20, 64, 93.5)) {
    pk <- fibrinogen(w, 150, 20, "male", FALSE)$PK$default
    expect_equal(c(pk$cl1, pk$v1), c(bellon(w)$cl, bellon(w)$v))
    expect_equal(pk$cl2, 0)
  }
})

test_that("the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(110, 165, 40, "female", TRUE)
  pk <- fibrinogen(110, 165, 40, "female", TRUE)$PK$default
  expect_equal(c(pk$cl1, pk$v1), c(bellon(size$pkWeight)$cl, bellon(size$pkWeight)$v))
})

test_that("the checks in the header", {
  chk <- function(w) {
    pk <- fibrinogen(w, 170, 30, "male", FALSE)$PK$default
    c(cl = pk$cl1 * 60 * 1000 / w, thalf = log(2) * pk$v1 / pk$cl1 / 60,
      ir = w / (pk$v1 * 10))
  }
  expect_equal(unname(chk(70)), c(0.437, 67.4, 2.355), tolerance = 0.002)
  expect_equal(unname(chk(20)), c(0.762, 49.15, 1.852), tolerance = 0.002)
})

test_that("70 mg/kg: the patient's fibrinogen plus the infused, in mg/dL", {
  PK <- getDrugPK("fibrinogen", 70, 175, 35, "male", adjustToFFM = FALSE)
  expect_equal(PK$endogenousActivity, 0)
  w <- simCpCe(data.frame(Drug = "fibrinogen", Time = 0, Dose = 70, Units = "mg/kg"),
               noEvents, PK, 600, FALSE)$wide
  v <- bellon(70)$v
  expect_equal(w$Plasma[w$Time == 0][1], 70 * 70 / (v * 10), tolerance = 1e-6)
  # 4.9 g is the same dose
  g <- simCpCe(data.frame(Drug = "fibrinogen", Time = 0, Dose = 4.9, Units = "g"),
               noEvents, PK, 600, FALSE)$wide
  expect_equal(g$Plasma, w$Plasma)
  PK2 <- getDrugPK("fibrinogen", 70, 175, 35, "male", adjustToFFM = FALSE,
                   factorBaselines = list(fibrinogenBaseline = 150))
  w2 <- simCpCe(data.frame(Drug = "fibrinogen", Time = 0, Dose = 70, Units = "mg/kg"),
                noEvents, PK2, 600, FALSE)$wide
  expect_equal(w2$Plasma - w$Plasma, rep(150, nrow(w)))
  expect_identical(patientFieldReaders("fibrinogenBaseline"), "fibrinogen")
  expect_identical(plottedUnitText("mgdL", "fibrinogen"), "mg/dL")
})
