# Miura 2026's children's models of sildenafil, tadalafil and ambrisentan
# (R/drugs_sildenafilMiura.R, R/drugs_tadalafilMiura.R,
# R/drugs_ambrisentanMiura.R) and the groups that choose them
# (R/modelSelection.R; Shafer, 2026-11-18: "Use Miura's model for children").
# Drafted by Claude Code, 2026-11-18.  Expectations are computed here from
# Tables 2 and 3 of the paper, not from the drug files.

clMiura <- function(cl, w, ref, e) cl * (w / ref)^e   # L/h

test_that("sildenafilMiura: Table 2, oral; intravenous on the FDA review's model", {
  X <- sildenafilMiura(16.5, 105, 5, "male", adjustToFFM = FALSE)
  set <- X$PK$default
  expect_equal(set$cl1, clMiura(60.6, 16.5, 32.3, 0.537) / 60)
  expect_equal(set$v1, 322)
  expect_equal(set$ka_PO, 4.51 / 60)
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$cl2, 0)
  expect_identical(X$routes, ROUTE_PO)
  iv <- X$parallelSystems[[1]]
  expect_identical(iv$routes, ROUTE_IV)
  fda <- sildenafilFDA(16.5, 105, 5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(iv$PK$default[c("v1", "v2", "cl1", "cl2")], fda[c("v1", "v2", "cl1", "cl2")])
  expect_null(iv$PK$default$tlag_PO)
  expect_equal(iv$PK$default$ka_PO, set$ka_PO)   # shared only for the time line
  # the Pulmonary hypertension field reaches the intravenous system only
  pah <- sildenafilMiura(16.5, 105, 5, "male", FALSE, pulmonaryHypertension = TRUE)
  expect_equal(pah$PK, X$PK)
  expect_equal(pah$parallelSystems[[1]]$PK$default$cl1,
               X$parallelSystems[[1]]$PK$default$cl1 * SILDENAFIL_PAH_CL)
})

test_that("tadalafilMiura and ambrisentanMiura: Tables 2 and 3", {
  set <- tadalafilMiura(10.7, 80, 2, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1, clMiura(1.76, 10.7, 14.2, 0.602) / 60)
  expect_equal(set$v1, 80.6)
  expect_equal(set$ka_PO, 0.84 / 60)
  set <- ambrisentanMiura(10.1, 80, 3, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1, clMiura(0.594, 10.1, 13.4, 0.561) / 60)
  expect_equal(set$v1, 12.4)
  expect_equal(set$ka_PO, 1.03 / 60)
  expect_null(set$tlag_PO)
})

test_that("switch on: the weight term at the pharmacokinetic weight", {
  pkW <- 70 * 1.3049067   # 120 kg, 170 cm, 50 y man, as the other tests pin it
  expect_equal_rounded(sildenafilMiura(120, 170, 50, "male")$PK$default$cl1,
                       clMiura(60.6, pkW, 32.3, 0.537) / 60)
  expect_equal_rounded(tadalafilMiura(120, 170, 50, "male")$PK$default$cl1,
                       clMiura(1.76, pkW, 14.2, 0.602) / 60)
  expect_equal_rounded(ambrisentanMiura(120, 170, 50, "male")$PK$default$cl1,
                       clMiura(0.594, pkW, 13.4, 0.561) / 60)
})

test_that("the source's median steady-state concentrations (Figure 5)", {
  css <- function(f, w, mgPerDay, age)
    mgPerDay * 1000 / 24 / (f(w, 110, age, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60)
  expect_equal(round(css(sildenafilMiura, 16.5, 30, 5), 1), 29.6)     # 25.4 reported
  expect_equal(round(css(tadalafilMiura, 10.7, 10.7, 2)), 300)       # 461, spironolactone mixed in
  expect_equal(round(css(ambrisentanMiura, 10.1, 2.02, 2)), 166)     # 164 reported
})

test_that("the groups choose Miura for children", {
  expect_identical(selectModel("sildenafil", 5, 18)$model, "sildenafilMiura")
  expect_identical(selectModel("sildenafil", 14, 50)$model, "sildenafilMiura")
  expect_identical(selectModel("sildenafil", 16, 60)$model, "sildenafilMiura")
  expect_identical(selectModel("sildenafil", 17, 60)$model, "sildenafilFDA")
  expect_identical(selectModel("sildenafil", 40, 70)$model, "sildenafilFDA")
  # tadalafil: Ferguson-Sells studied children from 2.5 years in a larger study
  expect_identical(selectModel("tadalafil", 1, 9)$model, "tadalafilMiura")
  expect_identical(selectModel("tadalafil", 5, 18)$model, "tadalafilFergusonSells")
  expect_identical(selectModel("tadalafil", 40, 70)$model, "tadalafilFergusonSells")
  # ambrisentan: Okour from 8 years and, as the nearest, in adults
  expect_identical(selectModel("ambrisentan", 4, 15)$model, "ambrisentanMiura")
  expect_identical(selectModel("ambrisentan", 8.5, 20)$model, "ambrisentanOkour")
  expect_identical(selectModel("ambrisentan", 40, 70)$model, "ambrisentanOkour")
})

test_that("the group functions return the chosen model and say so", {
  child <- sildenafil(18, 110, 5, "male", adjustToFFM = FALSE)
  expect_identical(child$modelChosen, "sildenafilMiura")
  expect_equal(child$PK, sildenafilMiura(18, 110, 5, "male", adjustToFFM = FALSE)$PK)
  expect_match(child$reference, "chose Miura 2026", fixed = TRUE)
  adult <- sildenafil(70, 170, 35, "male", adjustToFFM = FALSE, pulmonaryHypertension = TRUE)
  expect_identical(adult$modelChosen, "sildenafilFDA")
  expect_equal(adult$PK, sildenafilFDA(70, 170, 35, "male", FALSE, pulmonaryHypertension = TRUE)$PK)
  expect_identical(tadalafil(9, 75, 1, "female", adjustToFFM = FALSE)$modelChosen, "tadalafilMiura")
  expect_identical(ambrisentan(15, 100, 4, "male", adjustToFFM = FALSE)$modelChosen, "ambrisentanMiura")
  # the Pulmonary hypertension field still reaches the groups that read it
  expect_true("pulmonaryHypertension" %in% names(formals(sildenafil)))
  expect_true("pulmonaryHypertension" %in% names(formals(tadalafil)))
})

test_that("a child's oral and intravenous sildenafil are simulated and added", {
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  run <- function(units) {
    dt <- data.frame(Drug = "sildenafil", Time = 0, Dose = 10, Units = units)
    simulateDrugsWithCovariates(dt, events, 18, 110, 5, "male", 600, FALSE,
                                adjustToFFM = FALSE)$sildenafil$wide
  }
  po <- run("mg PO")
  iv <- run("mg")
  expect_gt(max(po$Plasma), 0)
  expect_gt(max(iv$Plasma), 0)
  # the intravenous peak is the FDA model's, not Miura's apparent volume
  fda <- sildenafilFDA(18, 110, 5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(max(iv$Plasma), 10 * 1000 / fda$v1, tolerance = 0.02)
})
