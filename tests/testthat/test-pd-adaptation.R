# Tolerance and tachyphylaxis (R/pdAdaptation.R): the panel that starts at
# 100% and falls (Steven L. Shafer, 2026-11-18).  The tolerance compartment is
# checked against its closed forms and against an independent integration of
# its differential equation, sharing no code with pdToleranceCompartment();
# the panel and the Graph Option against the plot and relevantFields().
# (Claude Code, 2026-11-18, at the request of Steven L. Shafer.)

# A table with one synthetic row of each structure, in place of the CSV for
# the length of one test
withAdaptationTable <- function(rows, code) {
  old <- .pdAdaptationCache$table
  on.exit(.pdAdaptationCache$table <- old, add = TRUE)
  .pdAdaptationCache$table <- rows
  force(code)
}

adaptationRow <- function(drug = "fentanyl", kind = "tolerance",
                          structure = "compartment", driver = "Effect Site",
                          k = 0.01, c50 = 0.002, hill = 1, maxFold = NA,
                          floor = NA, verification = "read_full_text",
                          scaleTo = NA_character_, c50PerEc50 = NA,
                          species = "human", weightKg = NA, exponent = NA) {
  data.frame(Drug = drug, scale_to = scaleTo, kind = kind, structure = structure,
             driver = driver, k_per_min = k, c50_mcg_per_ml = c50,
             c50_per_ec50 = c50PerEc50, hill = hill, max_fold = maxFold,
             floor = floor, endpoint = "test", species = species,
             weight_kg = weightKg, time_exponent = exponent, population = "test", source = "test", pmid = NA,
             verification = verification, notes = "", stringsAsFactors = FALSE)
}

test_that("the table has the columns the code reads, and only known structures", {
  table <- pdAdaptationTable()
  expect_true(all(c("Drug", "scale_to", "kind", "structure", "driver", "k_per_min",
                    "c50_mcg_per_ml", "c50_per_ec50", "hill", "max_fold", "floor",
                    "species", "weight_kg", "time_exponent", "source", "pmid",
                    "verification") %in% names(table)))
  # an applied animal row says how big the animals were, so its time can be
  # scaled (a pending row may not know yet)
  isApplied <- table$verification %in% PD_ADAPTATION_APPLIED_VERIFICATION
  expect_true(all(!isApplied | table$species %in% "human" | !is.na(table$weight_kg)))
  expect_true(all(is.na(table$weight_kg) | table$weight_kg > 0))
  # a class row names a Drug Library category and gives its c50 by potency
  dd <- getDrugDefaultsGlobal()
  expect_true(all(is.na(table$scale_to) | table$scale_to %in% dd$Category))
  # a pending row may record a source with no model yet
  expect_true(all(is.na(table$structure) | table$structure %in% PD_ADAPTATION_STRUCTURES))
  expect_true(all(table$kind %in% c("tolerance", "tachyphylaxis")))
  expect_true(all(is.na(table$driver) | table$driver %in% c("Effect Site", "Plasma")))
  # every applied row names its source and a drug in the library
  applied <- table[table$verification %in% PD_ADAPTATION_APPLIED_VERIFICATION, ]
  expect_true(all(nzchar(applied$source)))
  # (a class row may come from a drug, or a species' drug, not in the library)
  expect_true(all(applied$Drug %in% dd$Drug | !is.na(applied$scale_to)))
  expect_true(all(is.na(applied$scale_to) | !is.na(applied$c50_per_ec50) |
                  applied$structure != "compartment"))
  expect_true(all(applied$k_per_min > 0))
  expect_true(all(applied$structure %in% PD_ADAPTATION_STRUCTURES))
  expect_true(all(table$verification %in% c(PD_ADAPTATION_APPLIED_VERIFICATION,
                                            "pending_verification")))
})

test_that("a constant concentration fills the compartment as 1 - exp(-k t)", {
  t <- c(0, 0, seq(1, 600, by = 7))
  conc <- c(0, rep(2, length(t) - 1))  # a step at time 0
  k <- 0.013
  ct <- pdToleranceCompartment(t, conc, k)
  expect_equal(ct, c(0, 0, 2 * (1 - exp(-k * t[-(1:2)]))), tolerance = 1e-12)
})

test_that("a ramp is followed exactly, whatever the step", {
  # C = a t: Ct = a (t - (1 - exp(-k t)) / k)
  a <- 0.5; k <- 0.02
  exact <- function(t) a * (t - (1 - exp(-k * t)) / k)
  coarse <- c(0, 37, 200, 201, 900)
  expect_equal(pdToleranceCompartment(coarse, a * coarse, k), exact(coarse), tolerance = 1e-12)
})

test_that("the compartment matches an independent integration of dCt/dt = k (C - Ct)", {
  # A bolus-shaped curve, sampled densely as the engines' grid is, against
  # RK4 on the same piecewise-linear driver with a much finer step
  t <- sort(unique(c(seq(0, 30, by = 0.25), seq(30, 720, by = 2))))
  conc <- 3 * (exp(-0.01 * t) - exp(-0.4 * t))
  k <- 0.007
  ct <- pdToleranceCompartment(t, conc, k)
  driver <- function(s) stats::approx(t, conc, s, rule = 2)$y
  h <- 0.01
  grid <- seq(0, 720, by = h)
  y <- 0
  out <- numeric(length(grid))
  for (i in seq_along(grid)[-1]) {
    s <- grid[i - 1]
    f <- function(s, y) k * (driver(s) - y)
    k1 <- f(s, y); k2 <- f(s + h / 2, y + h / 2 * k1)
    k3 <- f(s + h / 2, y + h / 2 * k2); k4 <- f(s + h, y + h * k3)
    y <- y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    out[i] <- y
  }
  expect_equal(ct, stats::approx(grid, out, t)$y, tolerance = 1e-6)
})

test_that("potency and efficacy losses give the share of the naive effect left", {
  t <- c(0, 0, 60, 120)
  conc <- c(0, 0.004, 0.004, 0.004)  # mcg/mL
  row <- adaptationRow(k = 0.01, c50 = 0.002)
  ct <- pdToleranceCompartment(t, conc, 0.01)
  expect_equal(pdAdaptationRemaining(t, conc, row), 100 / (1 + ct / 0.002))
  # saturating at max_fold
  row <- adaptationRow(k = 0.01, c50 = 0.002, maxFold = 3)
  x <- ct / 0.002
  expect_equal(pdAdaptationRemaining(t, conc, row), 100 / (1 + 2 * x / (1 + x)))
  expect_true(all(pdAdaptationRemaining(c(0, 1e6), c(1, 1), row) >= 100 / 3 - 1e-9))
  # attenuation from the first exposure, towards a floor
  row <- adaptationRow(structure = "attenuation", kind = "tachyphylaxis",
                       k = 0.05, floor = 0.4)
  t <- c(0, 10, 20, 50)
  conc <- c(0, 0, 1, 1)
  expect_equal(pdAdaptationRemaining(t, conc, row),
               100 * (0.4 + 0.6 * exp(-0.05 * c(0, 0, 0, 30))))
  # nothing given, nothing lost
  expect_equal(pdAdaptationRemaining(t, c(0, 0, 0, 0), row), rep(100, 4))
})

test_that("the competitive form is Ouellet and Pollack's partial agonist read as wind chill", {
  # Their Eq 10 with Imax 0 (they estimated 5.45 +/- 12.56 %MPR): the effect
  # of Ce with the inhibitor at Ci, and the concentration that would give the
  # same effect in a naive animal, as a share of Ce, is the panel's value
  ec50 <- 0.390; ic50 <- 0.015; g <- 2.61
  row <- adaptationRow("morphine", structure = "competitive", k = 0.128 / 1440,
                       c50 = ic50, hill = g, driver = "Plasma")
  t <- c(0, 0, seq(60, 7200, by = 60))
  conc <- c(0, rep(0.4, length(t) - 1))
  ci <- pdToleranceCompartment(t, conc, row$k_per_min)
  e <- 100 * conc^g * ic50^g /
    (ec50^g * ic50^g + ec50^g * ci^g + ic50^g * conc^g)
  naive <- ifelse(conc > 0, ec50 * (e / (100 - e))^(1 / g), 0)
  expected <- ifelse(conc > 0, 100 * naive / conc, 100)
  expect_equal(pdAdaptationRemaining(t, conc, row), expected, tolerance = 1e-9)
  # fast onset, slow recovery, from one rate constant: most of the loss in
  # the first 12 h, and the compartment still near full a day after stopping
  left <- pdAdaptationRemaining(t, conc, row)
  expect_lt(left[t == 720], 70)
  stopped <- c(t, 7200 + c(1, 1440))
  after <- pdToleranceCompartment(stopped, c(conc, 0, 0), row$k_per_min)
  expect_gt(after[length(after)], 0.8 * after[length(t)])
})

test_that("the panel does not depend on the Drug Library's concentration unit", {
  row <- adaptationRow(k = 0.02, c50 = 0.001)
  t <- seq(0, 300, by = 5)
  results <- function(scale) rbind(
    data.frame(Drug = "fentanyl", Time = t, Site = "Plasma", Y = 2 * exp(-0.01 * t) * scale),
    data.frame(Drug = "fentanyl", Time = t, Site = "Effect Site",
               Y = 2 * (exp(-0.01 * t) - exp(-0.3 * t)) * scale))
  ng  <- pdAdaptationSeries(list(results = results(1),    Concentration.Units = "ng"), row)
  mcg <- pdAdaptationSeries(list(results = results(1e-3), Concentration.Units = "mcg"), row)
  expect_equal(ng$Y, mcg$Y, tolerance = 1e-12)
  expect_true(all(diff(ng$Y[t >= 10 & t <= 100]) < 0))
  # driven by plasma where the source says so
  plasma <- pdAdaptationSeries(list(results = results(1), Concentration.Units = "ng"),
                               adaptationRow(driver = "Plasma", k = 0.02, c50 = 0.001))
  expect_false(isTRUE(all.equal(plasma$Y, ng$Y)))
})

test_that("an animal row is moved to human physiological time", {
  expect_equal(pdAdaptationTimeScale(NA), 1)
  expect_equal(pdAdaptationTimeScale(70), 1)
  expect_equal(pdAdaptationTimeScale(0.3), (70 / 0.3)^0.25)
  expect_equal(pdAdaptationTimeScale(0.3, 0.1), (70 / 0.3)^0.1)
  expect_equal(pdAdaptationTimeScale(c(NA, 0.3), c(0.1, NA)), c(1, (70 / 0.3)^0.25))
  withAdaptationTable(adaptationRow(k = 0.02, species = "rat", weightKg = 0.3, exponent = 0), {
    expect_equal(pdAdaptationRow("fentanyl")$k_per_min, 0.02)
  })
  rat <- adaptationRow(k = 0.02, species = "rat", weightKg = 0.3)
  withAdaptationTable(rat, {
    row <- pdAdaptationRow("fentanyl")
    # a rat's half-time of 35 min is about 2.3 h in a 70 kg human
    expect_equal(row$k_per_min, 0.02 * (0.3 / 70)^0.25)
    expect_equal(log(2) / row$k_per_min, log(2) / 0.02 * (70 / 0.3)^0.25)
  })
})

test_that("a class row scales to every opioid by its MEAC", {
  dd <- getDrugDefaultsGlobal()
  meac <- function(d) {
    r <- dd[match(d, dd$Drug), ]
    r$MEAC * mcgPerMlScale(r$Concentration.Units)
  }
  rows <- adaptationRow("alfentanil", c50 = NA, c50PerEc50 = 2, scaleTo = "Opioids",
                        species = "rat", weightKg = 0.3)
  withAdaptationTable(rows, {
    expect_equal(pdAdaptationRow("morphine")$c50_mcg_per_ml, 2 * meac("morphine"))
    expect_equal(pdAdaptationRow("sufentanil")$c50_mcg_per_ml, 2 * meac("sufentanil"))
    expect_equal(pdAdaptationRow("alfentanil")$c50_mcg_per_ml, 2 * meac("alfentanil"))
    expect_equal(pdAdaptationRow("morphine")$source_drug, "alfentanil")
    # the same time course for every opioid
    expect_equal(pdAdaptationRow("morphine")$k_per_min, pdAdaptationRow("fentanyl")$k_per_min)
    # no MEAC, no potency anchor; not an opioid, not in the class
    expect_null(pdAdaptationRow("codeine"))
    expect_null(pdAdaptationRow("propofol"))
    expect_equal(pdAdaptationDrugs(c("propofol", "codeine", "fentanyl", "morphine")),
                 c("fentanyl", "morphine"))
    # equipotent concentrations lose the same share of their effect
    t <- seq(0, 600, by = 5)
    shape <- pmin(t / 30, 1) * exp(-0.002 * t)
    units <- function(d) dd$Concentration.Units[match(d, dd$Drug)]
    entry <- function(d) list(results = data.frame(
      Drug = d, Time = t, Site = "Effect Site",
      Y = 3 * meac(d) * shape / mcgPerMlScale(units(d))),
      Concentration.Units = units(d))
    f <- pdAdaptationSeries(entry("fentanyl"), pdAdaptationRow("fentanyl"))
    m <- pdAdaptationSeries(entry("morphine"), pdAdaptationRow("morphine"))
    expect_equal(f$Y, m$Y, tolerance = 1e-10)
    expect_lt(min(f$Y), 100)
  })
  # a drug's own row comes before its class's
  withAdaptationTable(rbind(rows, adaptationRow("morphine", c50 = 0.05)), {
    expect_equal(pdAdaptationRow("morphine")$c50_mcg_per_ml, 0.05)
    expect_equal(pdAdaptationRow("fentanyl")$c50_mcg_per_ml, 2 * meac("fentanyl"))
  })
})

test_that("the shipped opioid row is Ouellet and Pollack's, in a 70 kg adult", {
  # Shafer, 2026-11-19: time exponent 0.25, so that it matches the volunteer
  # studies that found no acute tolerance over hours (Gustorff 2002, Angst 2009)
  dd <- getDrugDefaultsGlobal()
  meac <- function(d) {
    r <- dd[match(d, dd$Drug), ]
    r$MEAC * mcgPerMlScale(r$Concentration.Units)
  }
  row <- pdAdaptationRow("fentanyl")
  expect_equal(row$source_drug, "morphine")
  expect_equal(row$structure, "competitive")
  expect_equal(row$k_per_min, 0.128 / 1440 * (0.3 / 70)^0.25, tolerance = 1e-3)
  expect_equal(row$c50_mcg_per_ml, 15.0 / 390 * meac("fentanyl"), tolerance = 1e-3)
  expect_null(pdAdaptationRow("codeine"))
  expect_true(relevantFields("fentanyl")[["showAdaptation"]])
  expect_false(relevantFields("propofol")[["showAdaptation"]])
  # held at 2.5 x MEAC: about 1% lost by 3 h, P = 1.5 (67%) near 14.5 h
  t <- c(0, 0, 180, 14.5 * 60)
  conc <- c(0, rep(2.5 * meac("fentanyl"), 3))
  left <- pdAdaptationRemaining(t, conc, row)
  expect_gt(left[3], 98.5)
  expect_lt(left[3], 99.5)
  expect_equal(left[4], 100 / 1.5, tolerance = 0.02)
})

test_that("only applied rows are drawn, and the series name maps back to its drug", {
  rows <- rbind(adaptationRow("fentanyl"),
                adaptationRow("nitroglycerin", kind = "tachyphylaxis",
                              verification = "pending_verification"))
  withAdaptationTable(rows, {
    expect_equal(pdAdaptationDrugs(c("propofol", "fentanyl", "nitroglycerin")), "fentanyl")
    expect_null(pdAdaptationRow("nitroglycerin"))
    expect_equal(pdAdaptationName("fentanyl", rows[1, ]), "fentanyl tolerance")
    expect_equal(pdAdaptationSeriesDrug(c("fentanyl tolerance", "nitroglycerin tachyphylaxis",
                                          "fentanyl V2", "Jo tolerance")),
                 c("fentanyl", "nitroglycerin", NA, NA))
  })
})

test_that("the Graph Option shows only while a drug with a model is drawn", {
  withAdaptationTable(adaptationRow("fentanyl"), {
    expect_false(relevantFields("propofol")[["showAdaptation"]])
    expect_true(relevantFields("fentanyl")[["showAdaptation"]])
    # in the table at 0, not drawn
    expect_false(relevantFields(c("propofol", "fentanyl"), plotted = "propofol")[["showAdaptation"]])
  })
})

test_that("the Interaction plot needs propofol and an opioid both drawn", {
  expect_true(relevantFields(c("propofol", "remifentanil"))[["plotInteraction"]])
  # the startup table: both in it, neither drawn, or only one
  expect_false(relevantFields(c("propofol", "remifentanil"), plotted = character(0))[["plotInteraction"]])
  expect_false(relevantFields(c("propofol", "remifentanil"), plotted = "propofol")[["plotInteraction"]])
  expect_false(relevantFields(c("propofol", "remifentanil"), plotted = "remifentanil")[["plotInteraction"]])
  expect_false(relevantFields(c("propofol", "rocuronium"))[["plotInteraction"]])
  # the MEAC choices still follow the table, as before
  expect_true(relevantFields(c("propofol", "remifentanil"), plotted = character(0))[["plotMEAC"]])
})

test_that("the plot draws the panel under the drug, from 0 to 100%", {
  withAdaptationTable(adaptationRow("fentanyl", k = 0.02, c50 = 0.0005), {
    dd <- getDrugDefaultsGlobal()
    ev <- data.frame(Time = numeric(0), Event = character(0))
    DT <- data.frame(Drug = "fentanyl", Time = c(0, 60), Dose = c(2, 0),
                     Units = "mcg/kg/hr")
    sim <- simulateDrugsWithCovariates(DT, ev, 70, 170, 50, "male", maximum = 240,
                                       plotRecovery = FALSE)
    plotted <- function(on) suppressMessages(simulationPlot(
      drugs = sim, events = ev, drugDefaults = dd,
      eventDefaults = getEventDefaults(), xBreaks = seq(0, 240, 60),
      xLabels = seq(0, 240, 60), xMaximum = 240, plotAdaptation = on))
    off <- plotted(FALSE)
    on <- plotted(TRUE)
    expect_false(any(grepl("tolerance", off$plotResults$Drug)))
    panel <- on$plotResults[as.character(on$plotResults$Drug) == "fentanyl tolerance", ]
    expect_gt(nrow(panel), 10)
    expect_equal(panel$Y[1], 100)
    expect_lt(min(panel$Y), 100)
    expect_true(all(panel$Y > 0 & panel$Y <= 100))
    # directly under fentanyl's own panel
    wraps <- levels(on$plotResults$Wrap)
    expect_equal(which(grepl("^fentanyl tolerance", wraps)), which(grepl("^fentanyl\n", wraps)) + 1)
  })
})
