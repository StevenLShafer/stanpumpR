# The Bayesian update (R/bayesian.R, R/bayesian-server.R): the maximum a
# posteriori estimate of a drug's clearance and volume from measured
# concentrations.  Drafted by Claude Code, 2026-11-13, at the request of
# Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

patient <- list(age = 50, weight = 70, height = 170, sex = "male")
noEvents <- data.frame(Time = numeric(0), Event = character(0))

priorDrugs <- function(DT) {
  recalculatePK(NULL, getDrugDefaultsGlobal(), DT, age = patient$age, weight = patient$weight,
                height = patient$height, sex = patient$sex)
}
rebuildFor <- function(DT) {
  function(drug, individual) {
    recalculatePK(NULL, getDrugDefaultsGlobal(), DT[DT$Drug == drug, , drop = FALSE],
                  age = patient$age, weight = patient$weight, height = patient$height,
                  sex = patient$sex, individual = stats::setNames(list(individual), drug))[[drug]]
  }
}

# Vancomycin, 1 g over an hour every 12 hours, four doses
vancomycinDT <- function() {
  data.frame(Drug = "vancomycin", Time = c(0, 60, 720, 780, 1440, 1500, 2160, 2220),
             Dose = rep(c(1000, 0), 4), Units = "mg/hr", stringsAsFactors = FALSE)
}

# The drug's plotted plasma concentration at `times` (minutes), for a set of
# individual factors (NULL for the model's patient)
plasmaAt <- function(DT, drug, times, individual = NULL) {
  drugs <- recalculatePK(NULL, getDrugDefaultsGlobal(), DT, age = patient$age,
                         weight = patient$weight, height = patient$height, sex = patient$sex,
                         individual = if (!is.null(individual)) stats::setNames(list(individual), drug))
  out <- withOutputTimes(times, processdoseTable(DT, noEvents, drugs, max(times) + 1, FALSE))
  wide <- out[[drug]]$wide
  wide$Plasma[match(times, wide$Time)]
}

test_that("the individual scales volumes and intercompartmental clearances together", {
  model <- getDrugPK("amiodarone", 70, 170, 50, "male")
  updated <- getDrugPK("amiodarone", 70, 170, 50, "male",
                       individual = list(clearance = 0.6, volume = 1.3))
  a <- model$PK$default
  b <- updated$PK$default
  expect_equal(b$v1, 1.3 * a$v1)
  expect_equal(b$v2, 1.3 * a$v2)
  expect_equal(b$cl1, 0.6 * a$cl1)
  expect_equal(b$cl2, 1.3 * a$cl2)
  # The distribution rate constants are the model's (BAYES.C held them fixed)
  expect_equal(b$k12, a$k12)
  expect_equal(b$k21, a$k21)
  expect_equal(b$k10, a$k10 * 0.6 / 1.3)
  expect_equal(updated$individual, list(clearance = 0.6, volume = 1.3))
  # The metabolite's formation moves with the parent's k10
  X <- individualizeModel(amiodarone(70, 170, 50, "male"), list(clearance = 0.6, volume = 1.3))
  expect_equal(X$metabolite$kFormation, amiodarone(70, 170, 50, "male")$metabolite$kFormation * 0.6 / 1.3)
})

test_that("parallel systems and saturable elimination take the same factors", {
  ind <- list(clearance = 0.5, volume = 2)
  w <- warfarin(70, 170, 50, "male")
  wi <- individualizeModel(w, ind)
  expect_equal(wi$PK$default$cl1, 0.5 * w$PK$default$cl1)
  expect_equal(wi$parallelSystems[[1]]$PK$default$cl1, 0.5 * w$parallelSystems[[1]]$PK$default$cl1)
  expect_equal(wi$parallelSystems[[1]]$PK$default$v1, 2 * w$parallelSystems[[1]]$PK$default$v1)
  # the INR link is the model's
  expect_identical(wi$inr, w$inr)

  p <- phenytoin(70, 170, 50, "male")
  pi <- individualizeModel(p, ind)
  expect_equal(pi$michaelisMenten$vmax, 0.5 * p$michaelisMenten$vmax)
  expect_equal(pi$michaelisMenten$km, p$michaelisMenten$km)
  expect_equal(pi$PK$default$v1, 2 * p$PK$default$v1)
  # absorption is the model's
  expect_identical(pi$oralFormulations, p$oralFormulations)
  expect_equal(pi$PK$default$ka_PO, p$PK$default$ka_PO)

  # a target capacity held as an amount moves with the volume
  v <- vildagliptin(70, 170, 50, "male")
  vi <- individualizeModel(v, ind)
  expect_equal(vi$michaelisMenten$targetBinding$rmaxCentral, 2 * v$michaelisMenten$targetBinding$rmaxCentral)
  expect_equal(vi$michaelisMenten$targetBinding$kd, v$michaelisMenten$targetBinding$kd)
})

test_that("the model's own patient is the model, and nonsense is refused", {
  expect_null(validateIndividual(NULL, "x"))
  expect_null(validateIndividual(list(clearance = 1, volume = 1), "x"))
  expect_error(validateIndividual(list(clearance = 0, volume = 1), "fentanyl"), "fentanyl")
  expect_error(validateIndividual(list(clearance = Inf, volume = 1), "x"))
  expect_error(validateIndividual(list(clearance = 1), "x"))
  expect_identical(getDrugPK("fentanyl", 70, 170, 50, "male",
                             individual = list(clearance = 1, volume = 1)),
                   getDrugPK("fentanyl", 70, 170, 50, "male"))
})

test_that("the variabilities are log-normal and the objective is Vozeh's", {
  expect_equal(bayesianSD(25), sqrt(log(1 + 0.25^2)))
  expect_equal(bayesianSD(5), sqrt(log(1 + 0.05^2)))
  # Half and twice the prediction are equally far
  expect_equal(bayesianObjective(c(0, 0), 2, 1, 1, 0.1), bayesianObjective(c(0, 0), 0.5, 1, 1, 0.1))
  expect_equal(bayesianObjective(c(0.1, -0.2), c(2, 3), c(2.2, 2.7), 0.25, 0.05),
               (0.01 + 0.04) / 0.0625 + (log(2 / 2.2)^2 + log(3 / 2.7)^2) / 0.0025)
  expect_equal(bayesianObjective(c(0, 0), 1, 0, 1, 1), Inf)
  expect_equal(bayesianObjective(c(0, 0), 1, NA, 1, 1), Inf)
})

test_that("bayesianObservations() keeps complete, positive rows", {
  obs <- data.frame(Drug = c("vancomycin", "", "vancomycin", "vancomycin", "warfarin"),
                    Time = c("690", "100", "", "780", "-5"),
                    Concentration = c("9", "1", "2", "0", "2"), stringsAsFactors = FALSE)
  out <- bayesianObservations(obs)
  expect_equal(nrow(out), 1)
  expect_equal(out$Time, 690)
  expect_equal(out$Concentration, 9)
  expect_equal(nrow(bayesianObservations(NULL)), 0)
  expect_equal(nrow(bayesianObservations(data.frame(x = 1))), 0)
})

test_that("measurements made by a known individual give that individual back", {
  DT <- vancomycinDT()
  times <- c(30, 90, 240, 690, 750, 900, 1400, 2150, 2400, 2800)
  truth <- list(clearance = 0.6, volume = 1.3)
  measured <- plasmaAt(DT, "vancomycin", times, truth)
  obs <- data.frame(Drug = "vancomycin", Time = times, Concentration = measured)
  # A broad prior and a tight residual: the data decide
  res <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs, pkVariability = 100,
                             residualVariability = 1, rebuild = rebuildFor(DT))
  fit <- res$fits$vancomycin
  expect_true(fit$converged)
  expect_equal(fit$clearance, 0.6, tolerance = 0.01)
  expect_equal(fit$volume, 1.3, tolerance = 0.01)
  expect_equal(fit$observations$Individual, measured, tolerance = 0.005)
  expect_length(res$problems, 0)
})

# mapbayr 0.10.2 (Le Louedec 2021) on mrgsolve 2.0.1, run 2026-11-13 on the
# same two-compartment vancomycin model (the library's at 70 kg, 170 cm, 50
# years, male), the same doses, the same log-normal priors (omega^2 =
# log(1 + 0.25^2) on CL and on V1, Q and V2 together) and a log-transformed
# residual error (sigma^2 = log(1 + 0.05^2)): ETA 0.6111885 and 0.7970002
# as factors.  The live comparison below reruns it where mapbayr is installed.
mapbayrCase <- list(
  times = c(11.5, 13, 35.5) * 60,
  concentrations = c(9, 32, 14),
  clearance = 0.6111885,
  volume = 0.7970002,
  individual = c(8.355861, 32.387775, 14.463835),
  population = c(5.316084, 24.300841, 8.614497)
)

test_that("the estimate agrees with mapbayr's", {
  DT <- vancomycinDT()
  obs <- data.frame(Drug = "vancomycin", Time = mapbayrCase$times,
                    Concentration = mapbayrCase$concentrations)
  res <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs, 25, 5, rebuild = rebuildFor(DT))
  fit <- res$fits$vancomycin
  expect_equal(fit$clearance, mapbayrCase$clearance, tolerance = 1e-5)
  expect_equal(fit$volume, mapbayrCase$volume, tolerance = 1e-5)
  expect_equal(fit$observations$Individual, mapbayrCase$individual, tolerance = 1e-5)
  expect_equal(fit$observations$Population, mapbayrCase$population, tolerance = 1e-6)
  # The updated drug and the model's beside it
  expect_equal(res$drugs$vancomycin$bayesian$clearance, fit$clearance)
  companion <- res$drugs[["vancomycin (population)"]]
  expect_equal(companion$companionOf, "vancomycin")
  expect_null(companion$individual)
  expect_false(identical(companion$Color, res$drugs$vancomycin$Color))
  expect_equal(alternativeModelParent(c("vancomycin (population)", "vancomycin (population) TCI")),
               c("vancomycin", "vancomycin TCI"))
  # Only the ratio of the variabilities matters
  res2 <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs, 50, 10, rebuild = rebuildFor(DT))
  expect_equal(res2$fits$vancomycin$clearance, fit$clearance, tolerance = 0.01)
})

test_that("the estimate agrees with mapbayr's, run live", {
  skip_on_cran()
  skip_if_not_installed("mapbayr")
  skip_if_not_installed("mrgsolve")
  s <- getDrugPK("vancomycin", 70, 170, 50, "male")$PK$default
  omega2 <- log(1 + 0.25^2)
  code <- sprintf(paste(
    "$PARAM TVCL = %.15g, TVV1 = %.15g, TVQ = %.15g, TVV2 = %.15g, ETA1 = 0, ETA2 = 0",
    "$OMEGA %.15g %.15g", "$SIGMA 0 %.15g",
    "$CMT @annotated", "CENT : central [ADM, OBS]", "PERI : peripheral",
    "$MAIN", "double CL = TVCL * exp(ETA(1) + ETA1);", "double V1 = TVV1 * exp(ETA(2) + ETA2);",
    "double Q = TVQ * exp(ETA(2) + ETA2);", "double V2 = TVV2 * exp(ETA(2) + ETA2);",
    "$ODE", "dxdt_CENT = -CL/V1*CENT - Q/V1*CENT + Q/V2*PERI;", "dxdt_PERI = Q/V1*CENT - Q/V2*PERI;",
    "$TABLE", "double CP = CENT/V1;", "double DV = CP * exp(EPS(2));", "$CAPTURE DV CP", sep = "\n"),
    s$cl1 * 60, s$v1, s$cl2 * 60, s$v2, omega2, omega2, log(1 + 0.05^2))
  mod <- mrgsolve::mcode("stanpumpRbayesxcheck", code, quiet = TRUE)
  data <- data.frame(ID = 1, time = c(0, 12, 24, 36, mapbayrCase$times / 60),
                     evid = c(1, 1, 1, 1, 0, 0, 0), amt = c(rep(1000, 4), 0, 0, 0),
                     rate = c(rep(1000, 4), 0, 0, 0), cmt = 1, mdv = c(1, 1, 1, 1, 0, 0, 0),
                     DV = c(NA, NA, NA, NA, mapbayrCase$concentrations))
  data <- data[order(data$time, -data$evid), ]
  est <- mapbayr::mapbayest(mod, data = data, verbose = FALSE, progress = FALSE)
  eta <- unlist(mapbayr::get_eta(est))
  expect_equal(unname(exp(eta)), c(mapbayrCase$clearance, mapbayrCase$volume), tolerance = 1e-5)
})

test_that("measurements that cannot be used are said, and fits are reused", {
  DT <- vancomycinDT()
  obs <- data.frame(Drug = c("vancomycin", "vancomycin", "fentanyl"),
                    Time = c(0, 780, 60), Concentration = c(3, 30, 1))
  calls <- 0
  rebuild <- function(drug, individual) { calls <<- calls + 1; rebuildFor(DT)(drug, individual) }
  res <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs, rebuild = rebuild)
  expect_match(res$problems, "fentanyl is not in the dose table", all = FALSE)
  expect_match(res$problems, "vancomycin: 1 measurement at or before the first dose is not used",
               all = FALSE)
  expect_equal(nrow(res$fits$vancomycin$observations), 1)
  # Unchanged inputs: the estimate is reused, and only the updated drug is rebuilt
  calls <- 0
  again <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs, rebuild = rebuild, fits = res$fits)
  expect_equal(calls, 1)
  expect_identical(again$fits$vancomycin$clearance, res$fits$vancomycin$clearance)
  # A new variability is a new estimate
  calls <- 0
  third <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs, pkVariability = 50,
                               rebuild = rebuild, fits = res$fits)
  expect_gt(calls, 1)
  # Nothing measured: the drugs as they were
  none <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs[0, ], rebuild = rebuild)
  expect_identical(none$drugs, priorDrugs(DT))
})

test_that("a parent's population line forms no metabolite of its own", {
  DT <- data.frame(Drug = "amiodarone", Time = 0, Dose = 400, Units = "mg/day PO",
                   stringsAsFactors = FALSE)
  obs <- data.frame(Drug = "amiodarone", Time = c(7, 21) * 1440, Concentration = c(0.8, 1.3))
  res <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs, rebuild = rebuildFor(DT))
  expect_true("amiodarone (population)" %in% names(res$drugs))
  updated <- processdoseTable(DT, noEvents, res$drugs, 30 * 1440, FALSE)
  # desethylamiodarone is what the updated amiodarone forms, and only that
  alone <- res$drugs[setdiff(names(res$drugs), "amiodarone (population)")]
  expected <- processdoseTable(DT, noEvents, alone, 30 * 1440, FALSE)
  expect_equal(updated$desethylamiodarone$wide, expected$desethylamiodarone$wide)
  expect_identical(updated$desethylamiodarone$formedFrom, "amiodarone")
})

test_that("warfarin is updated from its INR", {
  DT <- data.frame(Drug = "warfarin", Time = 0, Dose = 5, Units = "mg PO qd",
                   stringsAsFactors = FALSE)
  obs <- data.frame(Drug = "warfarin", Time = c(5, 10) * 1440 + 480, Concentration = c(1.8, 2.2))
  res <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs, rebuild = rebuildFor(DT))
  fit <- res$fits$warfarin
  expect_true(fit$converged)
  o <- fit$observations
  # The model's INR is above the measured, so the patient clears more
  expect_true(all(o$Population > o$Measured))
  expect_gt(fit$clearance, 1)
  expect_lt(sum(log(o$Individual / o$Measured)^2), sum(log(o$Population / o$Measured)^2) / 10)
  expect_equal(res$drugs$warfarin$parallelSystems[[1]]$PK$default$cl1,
               fit$clearance * priorDrugs(DT)$warfarin$parallelSystems[[1]]$PK$default$cl1)
})

test_that("the table's text is read in the format it was typed in", {
  rows <- data.frame(Drug = c("vancomycin", "vancomycin", "vancomycin"),
                     Time = c("11.5", "13", ""), Concentration = c("9", "32", "5"),
                     stringsAsFactors = FALSE)
  obs <- bayesRowsToObservations(rows, timeFormat("hours", "relative"), "08:00")
  expect_equal(obs$Time, c(690, 780))
  clock <- data.frame(Drug = "vancomycin", Time = "19:30", Concentration = "9",
                      stringsAsFactors = FALSE)
  expect_equal(bayesRowsToObservations(clock, timeFormat("hours", "clock"), "08:00")$Time, 690)
  # Converted through minutes when the format changes; unreadable text kept
  shown <- bayesRowsInFormat(rbind(rows, data.frame(Drug = "vancomycin", Time = "x", Concentration = "1")),
                             timeFormat("hours", "relative"), timeFormat("minutes", "relative"), "08:00")
  expect_equal(shown$Time, c("690", "780", "", "x"))
  back <- bayesObservationsToRows(obs, timeFormat("hours", "relative"), "08:00")
  expect_equal(back$Time, c("11.5", "13"))
  expect_equal(back$Concentration, c("9", "32"))
})

test_that("a bookmark's measurements are read as untrusted input", {
  drugsKnown <- getDrugDefaultsGlobal()$Drug
  good <- data.frame(Drug = "vancomycin", Time = 690, Concentration = 9)
  r <- restoredBayesObservations(good, drugsKnown)
  expect_false(r$changed)
  expect_equal(r$table$Time, 690)
  r <- restoredBayesObservations(data.frame(Drug = c("vancomycin", "<script>"),
                                            Time = c(690, 1), Concentration = c(9, 1)), drugsKnown)
  expect_true(r$changed)
  expect_equal(r$table$Drug, "vancomycin")
  expect_true(restoredBayesObservations("abc", drugsKnown)$changed)
  expect_equal(nrow(restoredBayesObservations("abc", drugsKnown)$table), 0)
  many <- data.frame(Drug = "vancomycin", Time = seq_len(100), Concentration = 1)
  r <- restoredBayesObservations(many, drugsKnown)
  expect_equal(nrow(r$table), MAX_BAYES_OBSERVATIONS)
  expect_true(r$changed)
  expect_false(restoredBayesObservations(NULL, drugsKnown)$changed)
})

test_that("the plot draws the measurements on their drug's panel", {
  DT <- vancomycinDT()
  obs <- data.frame(Drug = "vancomycin", Time = mapbayrCase$times,
                    Concentration = mapbayrCase$concentrations)
  res <- bayesianUpdateDrugs(priorDrugs(DT), DT, noEvents, obs, rebuild = rebuildFor(DT))
  drugs <- processdoseTable(DT, noEvents, res$drugs, 2880, FALSE)
  plot <- simulationPlot(drugs, noEvents, getDrugDefaultsGlobal(), getEventDefaults(),
                         xBreaks = seq(0, 2880, 480), xLabels = seq(0, 48, 8),
                         plasmaLinetype = "solid", effectsiteLinetype = "blank",
                         observations = obs)
  layers <- vapply(plot$plotObject$layers, function(l) class(l$geom)[1], "")
  expect_true("GeomPoint" %in% layers)
  points <- plot$plotObject$layers[[which(layers == "GeomPoint")]]$data
  expect_equal(points$Time, mapbayrCase$times)
  # the population line is drawn in the same panel
  expect_true("vancomycin (population)" %in% plot$plotResults$Drug)
  # not under normalization
  normalized <- simulationPlot(drugs, noEvents, getDrugDefaultsGlobal(), getEventDefaults(),
                               xBreaks = seq(0, 2880, 480), xLabels = seq(0, 48, 8),
                               plasmaLinetype = "solid", effectsiteLinetype = "blank",
                               normalization = "Peak plasma", observations = obs)
  layers <- vapply(normalized$plotObject$layers, function(l) class(l$geom)[1], "")
  expect_false("GeomPoint" %in% layers)
})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

bayesAppInputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "1440",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "solid", effectsiteLinetype = "blank",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 50, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE,
  bayesPKVariability = 25, bayesResidualVariability = 5
)

test_that("the sidebar's measurements update the drug in the app", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, bayesAppInputs)
    dt <- doseTableInit
    dt[1:4, ] <- list("vancomycin", c("0", "60", "720", "780"), c("1000", "0", "1000", "0"), "mg/hr")
    dt[5:6, ] <- list("", "", "", "")
    setDoseTable(dt, timeFormat("minutes", "relative"))
    session$flushReact()
    expect_null(drugs()$vancomycin$bayesian)
    expect_match(as.character(output$bayesTableUI$html), "Measured concentrations")

    session$setInputs(bayesTable = list(
      rows = list(list("vancomycin", "690", "9"), list("vancomycin", "780", "32"),
                  list("", "", "")),
      tag = "minutes/relative", seq = 1))
    session$elapse(2000)
    fit <- bayesUpdate()$fits$vancomycin
    expect_equal(fit$observations$Time, c(690, 780))
    expect_equal(drugs()$vancomycin$bayesian$clearance, fit$clearance)
    expect_true("vancomycin (population)" %in% names(drugs()))
    expect_match(as.character(output$bayesSummary$html), "vancomycin: clearance x")
    expect_equal(bayesPoints()$Concentration, c(9, 32))

    # A forged drug or a cut table changes nothing it should not
    session$setInputs(bayesTable = list(rows = list(list("nonsense", "690", "9")),
                                        tag = "minutes/relative", seq = 2))
    session$elapse(2000)
    expect_null(drugs()$vancomycin$bayesian)
    expect_match(paste(bayesUpdate()$problems, collapse = " "), "nonsense is not in the dose table")

    session$setInputs(bayesClear = 1)
    session$elapse(2000)
    expect_equal(nrow(bayes$observations()), 0)
    expect_false("vancomycin (population)" %in% names(drugs()))
  })
})

.sprglobals$config <- oldConfig
