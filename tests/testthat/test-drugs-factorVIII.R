# factorVIII: see R/drugs_factorVIII.R (Allard 2020, Table 2 and the
# equations under it) and R/factorBaselines.R.
allard <- function(w, age) list(
  cl = 204 * (max(age, 3) / 30)^-0.214 * (w / 64)^0.75 / 1000 / 60,
  v1 = 2640 * (w / 64)^0.827 / 1000)
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 2's equations, switch off", {
  for (p in list(c(15.1, 3), c(64, 30), c(130, 77), c(20, 1))) {
    pk <- factorVIII(p[1], 170, p[2], "male", FALSE)$PK$default
    ref <- allard(p[1], p[2])
    expect_equal(c(pk$cl1, pk$v1), c(ref$cl, ref$v1))
    expect_equal(c(pk$cl2, pk$v2), c(0.135 / 60, 0.339))
  }
  # the age term is held at 3 years, the youngest the source studied
  expect_equal(factorVIII(15, 100, 0.5, "male", FALSE)$PK$default$cl1,
               factorVIII(15, 100, 3, "male", FALSE)$PK$default$cl1)
})

test_that("the pharmacokinetic weight with the switch on; Q and V2 by fat-free mass", {
  size <- pkSizeFactors(110, 170, 40, "male", TRUE)
  pk <- factorVIII(110, 170, 40, "male", TRUE)$PK$default
  ref <- allard(size$pkWeight, 40)
  expect_equal(c(pk$cl1, pk$v1), c(ref$cl, ref$v1))
  expect_equal(pk$cl2, 0.135 / 60 * size$clearance)
  expect_equal(pk$v2, 0.339 * size$volume)
})

test_that("the checks in the header, at the source's medians", {
  pk <- factorVIII(64, 175, 30, "male", FALSE)$PK$default
  k10 <- pk$cl1 / pk$v1; k12 <- pk$cl2 / pk$v1; k21 <- pk$cl2 / pk$v2
  b <- k10 + k12 + k21
  lambda <- (b - sqrt(b^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / lambda / 60, 10.35, tolerance = 0.002)     # h
  expect_equal(pk$cl1 * 60 * 1000 / 64, 3.19, tolerance = 0.002)    # mL/kg/h
  expect_equal((pk$v1 + pk$v2) * 1000 / 64, 46.5, tolerance = 0.002) # mL/kg
})

test_that("50 IU/kg at 64 kg: baseline plus 2.42 IU/dL per IU/kg at the end of the bolus", {
  PK <- getDrugPK("factorVIII", 64, 175, 30, "male", adjustToFFM = FALSE)
  expect_equal(PK$endogenousActivity, 0.5)
  out <- simCpCe(data.frame(Drug = "factorVIII", Time = 0, Dose = 50, Units = "IU/kg"),
                 noEvents, PK, 2880, FALSE)
  w <- out$wide
  expect_equal(w$Plasma[w$Time == 0][1], 0.5 + 50 * 64 / 26.4, tolerance = 1e-6)
  # before any dose, only the patient's own activity
  pre <- simCpCe(data.frame(Drug = "factorVIII", Time = 60, Dose = 50, Units = "IU/kg"),
                 noEvents, PK, 120, FALSE)$wide
  expect_equal(pre$Plasma[pre$Time < 60], rep(0.5, sum(pre$Time < 60)))
  # 50 IU/kg reaches the label's major-surgery target (80-120 IU/dL)
  expect_true(max(w$Plasma) > 80 && max(w$Plasma) < 140)
})

test_that("the Patient Profile's baseline is added, and only to factor VIII", {
  base <- function(b) getDrugPK("factorVIII", 70, 175, 35, "male",
                                factorBaselines = list(factorVIIIBaseline = b))
  expect_equal(base(12)$endogenousActivity, 12)
  expect_equal(base(NULL)$endogenousActivity, FACTOR_VIII_BASELINE)
  dt <- data.frame(Drug = "factorVIII", Time = 0, Dose = 1000, Units = "IU")
  a <- simCpCe(dt, noEvents, base(0), 600, FALSE)$wide$Plasma
  b <- simCpCe(dt, noEvents, base(40), 600, FALSE)$wide$Plasma
  expect_equal(b - a, rep(40, length(a)))
  expect_error(base(400), "Invalid factorVIIIBaseline")
  expect_error(getDrugPK("factorVIII", 70, 175, 35, "male",
                         factorBaselines = list(factorXBaseline = 1)),
               "Unknown factor baseline")
  # a drug that does not name the field ignores it
  expect_null(getDrugPK("propofol", 70, 175, 35, "male",
                        factorBaselines = list(factorVIIIBaseline = 12))$endogenousActivity)
  expect_identical(patientFieldReaders("factorVIIIBaseline"), "factorVIII")
})

test_that("activity is dosed in IU only, and plotted in IU/dL", {
  PK <- getDrugPK("factorVIII", 70, 175, 35, "male")
  expect_error(simCpCe(data.frame(Drug = "factorVIII", Time = 0, Dose = 1, Units = "mg"),
                       noEvents, PK, 60, FALSE), "international units")
  expect_identical(plottedUnitText("IUdL", "factorVIII"), "IU/dL")
  expect_identical(getDrugDefaults("factorVIII")$Concentration.Units, "IUdL")
})

test_that("simulateDrugsWithCovariates carries the baseline", {
  dt <- data.frame(Drug = "factorVIII", Time = 0, Dose = 2000, Units = "IU")
  a <- simulateDrugsWithCovariates(dt, noEvents, 70, 175, 35, "male", 600, FALSE)
  b <- simulateDrugsWithCovariates(dt, noEvents, 70, 175, 35, "male", 600, FALSE,
                                   factorBaselines = list(factorVIIIBaseline = 30))
  expect_equal(b$factorVIII$wide$Plasma - a$factorVIII$wide$Plasma,
               rep(30 - FACTOR_VIII_BASELINE, nrow(a$factorVIII$wide)))
})

test_that("the Simulation API takes the baseline", {
  expect_true(all(names(FACTOR_BASELINE_FIELDS) %in% API_PATIENT_FIELDS))
  req <- list(patient = list(weight = 70, height = 175, age = 35, sex = "male",
                             factorVIIIBaseline = 25),
              doses = list(list(Drug = "factorVIII", Time = 0, Dose = 0, Units = "IU")),
              duration = 60)
  res <- apiSimulate(req)
  expect_equal(res$patient$factorVIIIBaseline, 25)
  ts <- res$timeseries
  expect_true(all(abs(ts$Plasma[ts$Drug == "factorVIII"] - 25) < 1e-9))
})
