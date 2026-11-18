# Tadalafil (R/drugs_tadalafil.R): Ferguson-Sells 2022's combined adult and
# paediatric model in pulmonary arterial hypertension, with the Patient
# Profile's Pulmonary hypertension set, and without it the population model
# of study LVBF in men with erectile dysfunction (FDA review of NDA 21-368;
# 2026-11-17).  Drafted by Claude Code, 2026-11-15.  Expectations are
# computed here from Ferguson-Sells's Table 2 and the review, not from the
# drug file.

KA <- 0.860; CLBOS <- 3.23; NOBOS <- -0.418; V <- 88.1

events <- data.frame(Time = double(), Event = character())
run <- function(dose, age = 54, pah = TRUE) simulateDrugsWithCovariates(
  data.frame(Drug = "tadalafil", Time = 0, Dose = dose, Units = "mg PO"),
  events, 70, 170, age, "male", 14400, FALSE, adjustToFFM = FALSE,
  pulmonaryHypertension = pah)$tadalafil$wide
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)

test_that("Table 2 (pulmonary hypertension), switch off", {
  X <- tadalafilFergusonSells(55, 160, 52.4, "female", adjustToFFM = FALSE, pulmonaryHypertension = TRUE)
  set <- X$PK$default
  expect_equal(set$cl1 * 60, 1.88, tolerance = 0.001)   # "(calculated) 1.88"
  expect_equal(set$cl1, CLBOS * (1 + NOBOS) / 60)
  expect_equal(set$v1, V * 55 / 70)
  expect_equal(set$ka_PO, KA / 60)
  expect_equal(set$bioavailability_PO, 1)                # age 52.4
  expect_equal(X$oralSaturation, list(form = "power", Dref = 16.27, exponent = -0.227))
  # the age term
  expect_equal(tadalafilFergusonSells(70, 170, 80, "male", FALSE, pulmonaryHypertension = TRUE)$PK$default$bioavailability_PO,
               (80 / 52.4)^0.100)
})

test_that("the age term is held at 2.5 years, the youngest studied", {
  f <- function(age) tadalafilFergusonSells(15, 100, age, "male", FALSE, pulmonaryHypertension = TRUE)$PK$default$bioavailability_PO
  expect_equal(f(0), (2.5 / 52.4)^0.100)
  expect_equal(f(1), f(2.5))
  expect_gt(f(0), 0)
})

test_that("Table 3: a 70-kg adult of 54 on 40 mg (with bosentan) has CL/F 3.95", {
  F40 <- (40 / 16.27)^-0.227 * (54 / 52.4)^0.100
  expect_equal(round(CLBOS / F40, 2), 3.95)     # the source's simulated median 4.05
  expect_equal(round(V / F40), 108)             # 107
})

test_that("the simulated AUC is dose x F / CL", {
  set <- tadalafilFergusonSells(70, 170, 54, "male", FALSE, pulmonaryHypertension = TRUE)$PK$default
  for (dose in c(20, 40)) {
    Fd <- (dose / 16.27)^-0.227 * (54 / 52.4)^0.100
    expect_equal(auc(run(dose)) / 60, dose * 1000 * Fd / (set$cl1 * 60), tolerance = 0.01)
  }
  # half-life 32.5 h (Adcirca label: 35 h in pulmonary hypertension)
  expect_equal(round(log(2) * set$v1 / set$cl1 / 60, 1), 32.5)
})

test_that("switch on: the weight term at the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y man: pharmacokinetic weight 70 x 1.3049067
  set <- tadalafilFergusonSells(120, 170, 50, "male", pulmonaryHypertension = TRUE)$PK$default
  expect_equal_rounded(set$v1, V * 1.3049067)
  expect_equal(set$cl1, CLBOS * (1 + NOBOS) / 60)
})

test_that("without pulmonary hypertension: study LVBF, men with erectile dysfunction", {
  # FDA Biopharmaceutics Review of NDA 21-368, part 4, p.3: one compartment,
  # CL/F 2.24 L/h (no active cardiovascular disease), V/F 60.0 L, ka 1.75 /h
  # fixed.  Checked against the Cialis label (healthy subjects): half-life
  # 17.5 h, median peak at 2 h, accumulation about 1.6-fold
  X <- tadalafilFergusonSells(70, 170, 35, "male", adjustToFFM = FALSE)
  set <- X$PK$default
  expect_equal(set$cl1 * 60, 2.24)
  expect_equal(set$v1, 60)
  expect_equal(set$ka_PO * 60, 1.75)
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$tlag_PO, 0)
  expect_null(X$oralSaturation)
  k <- 2.24 / 60
  expect_equal(round(log(2) / k, 1), 18.6)
  expect_equal(round(log(1.75 / k) / (1.75 - k), 2), 2.25)
  expect_equal(round(1 / (1 - exp(-24 * k)), 2), 1.69)
  # the simulated AUC is dose / CL, proportional to dose
  for (dose in c(5, 20)) expect_equal(auc(run(dose, pah = FALSE)) / 60, dose * 1000 / 2.24,
                                      tolerance = 0.01)
  # the peak of 20 mg: 306 ng/mL (Forgue 2006, healthy subjects: mean 378)
  expect_equal(round(max(run(20, pah = FALSE)$Plasma)), 306)
  # the switch: the size factors
  on <- tadalafilFergusonSells(120, 170, 50, "male")$PK$default
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  expect_equal(on$v1, 60 * size$volume)
  expect_equal(on$cl1, 2.24 / 60 * size$clearance)
  expect_match(X$reference, "LVBF")
})

test_that("the Patient Profile's Pulmonary hypertension switches between the populations", {
  healthy <- getDrugPK("tadalafil", 70, 170, 54, "male", adjustToFFM = FALSE)
  pah <- getDrugPK("tadalafil", 70, 170, 54, "male", adjustToFFM = FALSE,
                   pulmonaryHypertension = TRUE)
  expect_equal(healthy$PK$default$cl1 * 60, 2.24)
  expect_equal(pah$PK$default$cl1 * 60, CLBOS * (1 + NOBOS))
  expect_match(healthy$reference, "erectile dysfunction")
  expect_match(pah$reference, "Ferguson-Sells")
  # the PAH half-life is about 1.7 times the other (labels: 35 and 17.5 h)
  half <- function(pk) log(2) * pk$PK$default$v1 / pk$PK$default$cl1 / 60
  expect_equal(round(half(healthy), 1), 18.6)
  expect_equal(round(half(pah), 1), 32.5)
  expect_identical(patientFieldReaders("pulmonaryHypertension"), c("sildenafil", "tadalafil"))
})
