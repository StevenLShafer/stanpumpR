# antithrombinIlias: see R/drugs_antithrombinIlias.R (Ilias 2000, Table 3);
# and the antithrombin group (R/modelSelection.R).
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 3's medians at 70 kg, in proportion to weight, switch off", {
  for (w in c(50, 70, 120)) {
    pk <- antithrombinIlias(w, 170, 40, "male", FALSE)$PK$default
    expect_equal(c(pk$v1, pk$cl1), c(4.5, 2.7 / 1000) * w / 70)
    expect_equal(c(pk$cl2, pk$cl3), c(0, 0))
  }
})

test_that("the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(110, 165, 60, "female", TRUE)
  pk <- antithrombinIlias(110, 165, 60, "female", TRUE)$PK$default
  expect_equal(c(pk$v1, pk$cl1), c(4.5, 2.7 / 1000) * size$pkWeight / 70)
  expect_equal(antithrombinIlias(70, 170, 35, "male", TRUE)$PK$default$v1, 4.5)
})

test_that("the checks in the header", {
  pk <- antithrombinIlias(70, 170, 40, "male", FALSE)$PK$default
  expect_equal(log(2) * pk$v1 / pk$cl1 / 60, 19.25, tolerance = 1e-3)   # hours
  expect_equal(70 / (pk$v1 * 10), 1.556, tolerance = 1e-3)              # IU/dL per IU/kg
})

test_that("the baseline is plotted, and only plotted", {
  PK <- getDrugPK("antithrombinIlias", 70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(PK$endogenousActivity, 63)
  dose <- data.frame(Drug = "antithrombinIlias", Time = 60, Dose = 50, Units = "IU/kg")
  w <- simCpCe(dose, noEvents, PK, 180, FALSE)$wide
  expect_equal(w$Plasma[w$Time < 60], rep(63, sum(w$Time < 60)))
  expect_equal(max(w$Plasma[w$Time == 60]), 63 + 3500 / 45, tolerance = 1e-6)
  PK2 <- getDrugPK("antithrombinIlias", 70, 170, 40, "male", adjustToFFM = FALSE,
                   factorBaselines = list(antithrombinBaseline = 30))
  w2 <- simCpCe(dose, noEvents, PK2, 180, FALSE)$wide
  expect_equal(w2$Plasma - w$Plasma, rep(30 - 63, nrow(w)))
})

test_that("the group: Moffett under 19 years, Ilias from 19", {
  chosen <- function(age, weight = 70)
    antithrombin(weight, 170, age, "male", FALSE)$modelChosen
  expect_equal(chosen(0.3, 5), "antithrombinMoffett")
  expect_equal(chosen(18.5), "antithrombinMoffett")
  expect_equal(chosen(19.5), "antithrombinIlias")
  expect_equal(chosen(90), "antithrombinIlias")
  X <- antithrombin(70, 170, 40, "male", FALSE)
  expect_equal(X$PK, antithrombinIlias(70, 170, 40, "male", FALSE)$PK)
  expect_match(X$reference, "chose Ilias 2000")
  expect_equal(antithrombin(5, 60, 0.3, "male", FALSE)$PK,
               antithrombinMoffett(5, 60, 0.3, "male", FALSE)$PK)
  # the group reads the baseline field, and so passes it to both candidates
  expect_true("antithrombinBaseline" %in% names(formals(antithrombin)))
  PK <- getDrugPK("antithrombin", 70, 170, 40, "male", adjustToFFM = FALSE,
                  factorBaselines = list(antithrombinBaseline = 30))
  expect_equal(PK$endogenousActivity, 30)
})
