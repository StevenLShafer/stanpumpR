# Caspofungin (R/drugs_caspofunginXu.R, the group in R/modelSelection.R).
# Drafted by Claude Code, 2026-11-28.  The expectations are Xu 2026's
# equations, computed here, not read from the drug file.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
mosteller <- function(w, h) sqrt(w * h / 3600)

test_that("Xu 2026's Table 2 at the reference child, 0.79 m2", {
  # a child whose Mosteller BSA is exactly 0.79 m2
  h <- 110
  w <- 0.79^2 * 3600 / h
  set <- caspofunginXu(w, h, 5, "female")$PK$default
  expect_equal(set$cl1 * 60, 0.196)
  expect_equal(set$v1, 2.22)
  expect_equal(set$cl2 * 60, 1.01)
  expect_equal(set$v2, 1.63)
  expect_equal(set$cl3, 0)
  # the elimination rate constant the authors quote, 0.088 /h
  expect_equal(round(set$cl1 / set$v1 * 60, 3), 0.088)
})

test_that("Xu's allometry: clearances to the 0.66, volumes to the 1", {
  set <- caspofunginXu(30, 135, 10, "male")$PK$default
  b <- mosteller(30, 135) / 0.79
  expect_equal(set$cl1 * 60, 0.196 * b^0.66)
  expect_equal(set$cl2 * 60, 1.01 * b^0.66)
  expect_equal(set$v1, 2.22 * b)
  expect_equal(set$v2, 1.63 * b)
})

test_that("the BSA is the actual one, whichever way the switch is set", {
  expect_equal(caspofunginXu(90, 160, 15, "female", adjustToFFM = TRUE)$PK,
               caspofunginXu(90, 160, 15, "female", adjustToFFM = FALSE)$PK)
  expect_equal_rounded(0.33983, caspofunginXu(70, 170, 35, "male")$PK$default$cl1 * 60)
})

test_that("the group gives every patient Xu's model for now", {
  chosen <- function(age, weight) caspofungin(weight, 170, age, "male")$modelChosen
  expect_equal(chosen(5, 18), "caspofunginXu")
  expect_equal(chosen(35, 70), "caspofunginXu")
  expect_equal(caspofungin(18, 110, 5, "male")$PK, caspofunginXu(18, 110, 5, "male")$PK)
})

test_that("caspofungin simulates, offers the label's units and no band", {
  dt <- data.frame(Drug = "caspofungin", Time = c(0, 60), Dose = c(39.5, 0),
                   Units = "mg/hr")
  w <- simulateDrugsWithCovariates(dt, events, 16, 104, 5, "female", 48 * 60, FALSE)$caspofungin$wide
  expect_gt(max(w$Plasma), 0)
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "caspofungin"])
  expect_true(all(c("mg", "mg qd", "mg/m2", "mg/m2 qd", "mg/hr", "mg/m2/hr") %in% units))
  expect_equal(dd$Upper[dd$Drug == "caspofungin"], 0)
  expect_equal(dd$endCe[dd$Drug == "caspofungin"], 0)
  expect_true(is.na(dd$Category[dd$Drug == "caspofunginXu"]) ||
                !nzchar(dd$Category[dd$Drug == "caspofunginXu"]))
})
