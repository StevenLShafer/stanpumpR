# Micafungin (R/drugs_micafunginJullien.R, R/drugs_micafunginWasmann.R, the
# group in R/modelSelection.R).  Drafted by Claude Code, 2026-11-27.  The
# expectations are the papers' equations, computed here, not read from the
# drug files.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))

test_that("Jullien 2017's equations, switch off", {
  set <- micafunginJullien(100, 175, 60, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1 * 60, 1.34 * (100 / 84)^0.59)
  expect_equal(set$v1, 11.8 * (100 / 84)^0.61)
  expect_equal(set$v2, 7.68 * (100 / 84)^0.67)
  expect_equal(set$cl2 * 60, 4.67)
  # the typical patient: 84 kg, albumin above 25 g/L, SOFA below 10; Table 3's
  # AUC after 100 mg is 74.6 mg h/L, 100 / 1.34
  set <- micafunginJullien(84, 175, 60, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1 * 60, 1.34)
  expect_equal(round(100 / (set$cl1 * 60), 1), 74.6)
})

test_that("Jullien's albumin step: 1.14 at 2.5 g/dL or less, none when blank", {
  base <- micafunginJullien(84, 175, 60, "male", adjustToFFM = FALSE)$PK$default
  low <- micafunginJullien(84, 175, 60, "male", adjustToFFM = FALSE, albumin = 2.1)$PK$default
  high <- micafunginJullien(84, 175, 60, "male", adjustToFFM = FALSE, albumin = 3.5)$PK$default
  for (p in c("cl1", "v1", "v2")) expect_equal(low[[p]] / base[[p]], 1.14)
  expect_equal(low$cl2, base$cl2)
  expect_equal(high, base)
  # Table 3: SOFA < 10, albumin <= 25 g/L, 65.5 mg h/L after 100 mg
  expect_equal(round(100 / (low$cl1 * 60), 1), 65.5)
})

test_that("Wasmann 2019's equations, switch off", {
  set <- micafunginWasmann(150, 170, 45, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1 * 60, 0.690 * (150 / 70)^0.74)
  expect_equal(set$v1, 5.84 * (150 / 70)^1.17)
  expect_equal(set$v2, 6.96 * (150 / 70)^0.71)
  expect_equal(set$cl2 * 60, 7.15)
})

test_that("the switch on reads the pharmacokinetic weight; Q takes the size factor", {
  size <- pkSizeFactors(120, 170, 50, "female", TRUE)
  on <- micafunginJullien(120, 170, 50, "female")$PK$default
  expect_equal(on$v1, 11.8 * (size$pkWeight / 84)^0.61)
  expect_equal(on$cl2 * 60, 4.67 * size$clearance)
  cohort <- cohortFactorsIndependent(list(groups = list(
    list(weight = 70.8, age = 31, men = 4, women = 4, bmi = 22.5),
    list(weight = 156, age = 51, men = 3, women = 5, bmi = 44.4),
    list(weight = 141, age = 46, men = 5, women = 3, bmi = 43.5))))
  on <- micafunginWasmann(120, 170, 50, "female")$PK$default
  expect_equal(on$cl1 * 60, 0.690 * (size$pkWeight / 70)^0.74)
  expect_equal(on$cl2 * 60, 7.15 * size$clearance / cohort$clearance)
  expect_equal_rounded(6.0618, micafunginWasmann(70, 170, 35, "male")$PK$default$cl2 * 60)
  expect_equal_rounded(1.203339, micafunginJullien(70, 170, 35, "male")$PK$default$cl1 * 60)
})

test_that("the program chooses by the source populations", {
  chosen <- function(age, weight) micafungin(weight, 170, age, "male")$modelChosen
  expect_equal(chosen(40, 80), "micafunginJullien")   # both contain: the larger study
  expect_equal(chosen(25, 80), "micafunginWasmann")   # younger than Jullien's youngest
  expect_equal(chosen(50, 160), "micafunginWasmann")  # heavier than Jullien's heaviest
  expect_equal(chosen(70, 160), "micafunginJullien")  # neither: nearer by age
  expect_equal(chosen(8, 25), "micafunginWasmann")    # a child: nearer by age
  expect_equal(micafungin(84, 175, 60, "male", albumin = 2)$PK,
               micafunginJullien(84, 175, 60, "male", albumin = 2)$PK)
})

test_that("micafungin simulates, offers infusion rates only and no band", {
  dt <- data.frame(Drug = "micafungin", Time = 0, Dose = 100, Units = "mg/hr")
  dt <- rbind(dt, data.frame(Drug = "micafungin", Time = 60, Dose = 0, Units = "mg/hr"))
  w <- simulateDrugsWithCovariates(dt, events, 84, 175, 60, "male", 48 * 60, FALSE)$micafungin$wide
  expect_gt(max(w$Plasma), 0)
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "micafungin"])
  # The label: "intravenous infusion only. Infuse over one hour." So rates
  # only, no amount (bolus) unit (2026-11-28, after Devin's review of PR #1018
  # found the same for caspofungin and isavuconazole).
  expect_setequal(units, c("mg/hr", "mg/kg/hr"))
  expect_equal(dd$Upper[dd$Drug == "micafungin"], 0)
  expect_true(is.na(dd$Category[dd$Drug == "micafunginJullien"]) ||
                !nzchar(dd$Category[dd$Drug == "micafunginJullien"]))
})
