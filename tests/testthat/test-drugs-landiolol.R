# Landiolol: the three candidates of the group `landiolol` (R/landiolol.R,
# R/drugs_landiolol*.R) and the group's choice among them.
# Drafted by Claude Code, 2026-11-25.

# Each source's Table, per kg (mL/min/kg and mL/kg), read on its PDF
SRC <- list(
  landiololHonda        = c(CL = 36.6, V1 = 101,  Q = 16.1, V2 = 55.6),  # Honda 2008, Table 3
  landiololKunisawa2014 = c(CL = 34.0, V1 = 74.9, Q = 70.9, V2 = 38.9),  # Kunisawa (J Anesth), Table 2
  landiololKunisawa2015 = c(CL = 30.7, V1 = 65.0, Q = 48.3, V2 = 54.4)   # Kunisawa (TCRM), Table 2
)

events <- data.frame(Time = double(), Event = character())

test_that("each candidate's PK set is its table per kilogram, at total weight with the switch off", {
  for (m in names(SRC)) {
    p <- SRC[[m]]
    set <- get(m)(58.9, 159, 73, "male", adjustToFFM = FALSE)$PK$default
    expect_equal_rounded(p[["CL"]] / 1000 * 58.9, set$cl1)
    expect_equal_rounded(p[["V1"]] / 1000 * 58.9, set$v1)
    expect_equal_rounded(p[["Q"]]  / 1000 * 58.9, set$cl2)
    expect_equal_rounded(p[["V2"]] / 1000 * 58.9, set$v2)
    PK <- getDrugPK(m, 58.9, 159, 73, "male", adjustToFFM = FALSE)
    s <- PK$PK$default
    expect_equal(s$cl3, 0, info = m)
    # the two exponents from the micro constants
    k10 <- p[["CL"]] / p[["V1"]]; k12 <- p[["Q"]] / p[["V1"]]; k21 <- p[["Q"]] / p[["V2"]]
    b <- k10 + k12 + k21
    lam <- c(b + sqrt(b^2 - 4 * k10 * k21), b - sqrt(b^2 - 4 * k10 * k21)) / 2
    expect_equal_rounded(lam[1], s$lambda_1)
    expect_equal_rounded(lam[2], s$lambda_2)
    # every terminal half-life is minutes, not hours
    expect_lt(log(2) / lam[2], 6)
  }
})

test_that("per-kilogram parameters follow the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(110, 175, 57, "male", TRUE)
  for (m in names(SRC)) {
    on <- get(m)(110, 175, 57, "male", adjustToFFM = TRUE)$PK$default
    expect_equal_rounded(SRC[[m]][["CL"]] / 1000 * size$pkWeight, on$cl1)
    expect_equal_rounded(SRC[[m]][["V1"]] / 1000 * size$pkWeight, on$v1)
  }
})

test_that("an infusion reaches the steady state its clearance sets", {
  dose <- data.frame(Drug = "landiolol", Time = c(0, 60),
                     Dose = c(10, 0), Units = "mcg/kg/min")
  for (age in c(25, 50, 80)) {
    m <- selectModel("landiolol", age, 60)$model
    wide <- simulateDrugsWithCovariates(dose, events, 60, 160, age, "male", 90,
                                        FALSE, adjustToFFM = FALSE)$landiolol$wide
    late <- wide$Plasma[wide$Time == 60][1]
    expect_lt(abs(late / (10 / SRC[[m]][["CL"]]) - 1), 0.01)
    # and it is gone within a quarter of an hour of the stop
    expect_lt(wide$Plasma[wide$Time >= 75][1] / late, 0.1)
  }
})

test_that("the group chooses by the source populations", {
  pick <- function(age, wt) selectModel("landiolol", age, wt)$model
  expect_identical(pick(25, 70), "landiololHonda")          # inside Honda
  expect_identical(pick(10, 30), "landiololHonda")          # nearest
  expect_identical(pick(35, 70), "landiololHonda")          # 2 y either side: Honda's n
  expect_identical(pick(36, 70), "landiololKunisawa2014")   # nearest
  expect_identical(pick(50, 60), "landiololKunisawa2014")   # inside
  expect_identical(pick(68, 60), "landiololKunisawa2014")   # both: 9 patients over 8
  expect_identical(pick(75, 60), "landiololKunisawa2015")   # inside only the PAD study
  expect_identical(pick(90, 60), "landiololKunisawa2015")   # nearest
  X <- landiolol(70, 170, 25, "male")
  expect_identical(X$modelChosen, "landiololHonda")
  expect_match(X$reference, "Honda 2008")
})

test_that("the effect site peaks 2 min after a bolus (heart rate, Krumpl 2017), whatever the model", {
  for (age in c(25, 50, 80)) {
    PK <- getDrugPK("landiolol", 70, 170, age, "male")
    expect_equal(PK$tPeak, 2)
    pk <- PK$PK$default
    coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)
    lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
    expect_equal(effectSitePeakTime(coef, lambda, pk$ke0), 2, tolerance = 1e-4)
  }
})

test_that("landiolol is a vasoactive drug with no band; its candidates are not offered", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "landiolol", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
  expect_equal(row$Upper, 0)
  for (m in names(SRC)) {
    expect_true(is.na(dd$Category[dd$Drug == m]) || !nzchar(dd$Category[dd$Drug == m]), info = m)
    expect_equal(LANDIOLOL_SOURCES[[c(landiololHonda = "honda",
                                      landiololKunisawa2014 = "kunisawa2014",
                                      landiololKunisawa2015 = "kunisawa2015")[[m]]]][["cl"]] * 1000,
                 SRC[[m]][["CL"]], info = m)
  }
})

test_that("weight changes the choice at the edges the help pages name", {
  pick <- function(age, wt) selectModel("landiolol", age, wt)$model
  expect_identical(pick(35, 48), "landiololKunisawa2014")   # below Honda's 50.7 kg
  expect_identical(pick(68, 45), "landiololKunisawa2015")   # below Kunisawa 2014's 47.1 kg
  expect_identical(pick(68, 47.1), "landiololKunisawa2014")
})
