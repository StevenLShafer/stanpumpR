# Single Paper Review on the Duzitol editorial (Shafer and Stanski,
# Anesthesiology 1992;76:327-30): a fictional drug, "a drug that does it
# all", in normal subjects and in hepatic failure, beside propofol, given in
# its Table 1 as three half-lives, metabolic clearance, V1 and Vdss.  The
# example was built to show that tables of pharmacokinetic parameters cannot
# be read by eye: the half-lives say hepatic failure will recover slowest,
# and the model says it recovers fastest.  Shafer supplied the spreadsheets
# he built the models with; the values pinned here are theirs.

duzitolTable <- list(
  "Duzitol, normal subjects" = c(CL = 0.68, V1 = 7.6, Vss = 66, t1 = 1.0, t2 = 10.0, t3 = 100),
  "Duzitol, hepatic failure" = c(CL = 0.51, V1 = 5.0, Vss = 93, t1 = 4.0, t2 = 40.0, t3 = 400),
  "Propofol, normal subjects" = c(CL = 1.9, V1 = 6.3, Vss = 284, t1 = 0.8, t2 = 7.8, t3 = 198))
# Table 1 exactly as printed: the digits are the rounding the review allows.
duzitolPrinted <- list(
  "Duzitol, normal subjects" = c(CL = "0.68", V1 = "7.6", Vss = "66", t1 = "1.0", t2 = "10.0", t3 = "100"),
  "Duzitol, hepatic failure" = c(CL = "0.51", V1 = "5.0", Vss = "93", t1 = "4.0", t2 = "40.0", t3 = "400"),
  "Propofol, normal subjects" = c(CL = "1.9", V1 = "6.3", Vss = "284", t1 = "0.8", t2 = "7.8", t3 = "198"))
# The text: 50% decrements after steady-state infusions of 41, 10 and 20 min.
duzitolDecrement <- c(41, 10, 20)

test_that("the three-compartment closure reproduces Shafer's spreadsheets", {
  # Micro constants and coefficients from the spreadsheets' "(3). V1, Vdss,
  # Cl, half-lives" sheets, to the digits they print.
  sheet <- list(
    c(k21 = 0.217689586, k31 = 0.017097925, k13 = 0.104640961, k12 = 0.340491214,
      A = 0.098797945, B = 0.026195157, C = 0.006585846),
    c(k21 = 0.020938793, k31 = 0.002436378, k13 = 0.039707715, k12 = 0.027265457,
      A = 0.19456923, B = 0.004420758, C = 0.001010012),
    c(k21 = 0.125688153, k31 = 0.007110812, k13 = 0.300788494, k12 = 0.223624982,
      A = 0.150580618, B = 0.00719905, C = 0.000950491))
  for (i in seq_along(duzitolTable)) {
    t <- duzitolTable[[i]]
    m <- threeCompartmentFromHalfLives(t[["CL"]], t[["V1"]], t[["Vss"]], t[["t1"]], t[["t2"]], t[["t3"]])
    k <- c(k21 = m$cl2 / m$v2, k31 = m$cl3 / m$v3, k13 = m$cl3 / m$v1, k12 = m$cl2 / m$v1)
    # The sheet prints k31 to eight figures.
    expect_equal(k, sheet[[i]][names(k)], tolerance = 1e-7)
    expect_equal(m$v1 + m$v2 + m$v3, t[["Vss"]], tolerance = 1e-12)
    # stanpumpR's own eigenvalues and bolus coefficients from the model agree.
    lambda <- sort(cube(m$cl1 / m$v1, k[["k12"]], k[["k13"]], k[["k21"]], k[["k31"]]), decreasing = TRUE)
    expect_equal(log(2) / lambda, unname(t[c("t1", "t2", "t3")]), tolerance = 1e-9)
  }
  expect_error(threeCompartmentFromHalfLives(1, 10, 11, 1, 10, 100), "no physically admissible")
})

# An extraction of Table 1, as Claude would return it with model groups.
duzitolExtraction <- function() {
  param <- function(g, role, value, units, loc = "Table 1, p. 328", text = format(value))
    list(name_in_paper = role, group = g, role = role, value = value,
         value_text = text, units = units,
         uncertainty = list(type = "not_reported"), status = "estimated", location = loc)
  derived <- function(g, q, value, units, loc = "Table 1, p. 328", text = format(value))
    list(quantity = q, group = g, value = value, value_text = text, units = units,
         statistic = "typical_value", uncertainty = list(type = "not_reported"), location = loc)
  ps <- list(); ds <- list()
  for (g in names(duzitolTable)) {
    t <- duzitolTable[[g]]; txt <- duzitolPrinted[[g]]
    ps <- c(ps, list(param(g, "CL", t[["CL"]], "L/min", text = txt[["CL"]]),
                     param(g, "V1", t[["V1"]], "L", text = txt[["V1"]])))
    ds <- c(ds, list(derived(g, "Vss", t[["Vss"]], "L", text = txt[["Vss"]]),
                     derived(g, "t_half_1", t[["t1"]], "min", text = txt[["t1"]]),
                     derived(g, "t_half_2", t[["t2"]], "min", text = txt[["t2"]]),
                     derived(g, "t_half_3", t[["t3"]], "min", text = txt[["t3"]]),
                     derived(g, "decrement_50", duzitolDecrement[which(names(duzitolTable) == g)],
                             "min", "Text, p. 328")))
  }
  x <- paperReviewExample()
  x$paper <- list(title = "Improving the clinical utility of anesthetic drug pharmacokinetics",
                  first_author = "Shafer", year = 1992, journal = "Anesthesiology")
  x$drug$name <- "Duzitol"
  x$drug$concentration_units <- "mcg/mL"
  x$structure$effect_site <- "none"
  x$structure$routes <- list("iv_infusion")
  x$parameters <- ps
  x$derived_quantities <- ds
  x$covariate_effects <- list()
  x$regimens <- list()
  x$figures <- list()
  x$reported_differences <- list()
  x$interpretive_claims <- list()
  x
}

test_that("each of Table 1's three models is reviewed on its own, and closes", {
  review <- paperReviewRun(duzitolExtraction())
  expect_equal(names(review$models), names(duzitolTable))
  for (g in names(duzitolTable)) {
    m <- review$models[[g]]
    expect_equal(m$identifiability$verdict, "derived")
    expect_equal(m$identifiability$route, "CL, V1, Vss and the three half-lives")
    expect_true(m$drug$ok)
    expect_equal(m$drug$defaults$Drug, paste0(g, " (paper)"))
  }
  # Three models' values of one parameter are not a conflict.
  titles <- vapply(review$findings, `[[`, "", "title")
  expect_false(any(grepl("printed with different values", titles)))
})

test_that("the editorial's decrement times agree with its Table 1 within the table's rounding", {
  review <- paperReviewRun(duzitolExtraction())
  status <- vapply(review$models, function(m) {
    t <- m$consistency
    t$status[t$quantity == "decrement_50"]
  }, "")
  # 41.3 and 10.1 min at the centre; propofol's 20 lies inside the 17-27 the
  # rounding of its column allows, though its centre is 21.8.
  expect_equal(unname(status[1:2]), c("matches", "matches"))
  expect_equal(unname(status[3]), "matches within rounding")
  t <- review$models[[3]]$consistency
  expect_match(t$note[t$quantity == "decrement_50"], "rounding of the printed inputs")
})

test_that("a figure of decrement times against infusion duration is checked against the model", {
  x <- duzitolExtraction()
  m <- threeCompartmentFromHalfLives(0.68, 7.6, 66, 1, 10, 100)
  # The model's own curve at a few durations, as Figure 1 plots it.
  k12 <- m$cl2 / m$v1; k13 <- m$cl3 / m$v1; k21 <- m$cl2 / m$v2; k31 <- m$cl3 / m$v3
  l <- sort(cube(m$cl1 / m$v1, k12, k13, k21, k31), decreasing = TRUE)
  A <- vapply(1:3, function(j) { o <- setdiff(1:3, j)
    (k21 - l[j]) * (k31 - l[j]) / prod(l[o] - l[j]) / m$v1 }, 0)
  # Figure 1 holds the plasma concentration constant (TCI), then stops.
  durations <- c(30, 120, 240, 480, 600)
  y <- vapply(durations, function(T)
    paperReviewTCIDecrementTime(list(v1 = m$v1, v2 = m$v2, v3 = m$v3, cl1 = m$cl1, cl2 = m$cl2,
                                     cl3 = m$cl3), 0.5, T), 0)
  x$figures <- list(list(figure_id = "Fig 1", caption = "50% decrement times", regimen_id = NULL,
    y_quantity = "decrement_time", decrement_percent = 50, decrement_site = "plasma",
    y_units = "other", time_units = "min",
    log_y = "no", data_kind = "model_prediction", one_regimen = "yes", error_bars = "none",
    series = list(list(label = "Duzitol, normal subjects", group = "Duzitol, normal subjects",
                       analyte = "parent",
                       points = lapply(seq_along(durations), function(i)
                         list(time = durations[i], value = round(y[i], 2))))),
    digitization_note = "exact", location = "Fig 1, p. 328"))
  review <- paperReviewRun(x)
  f <- Filter(function(z) grepl("Fig 1", z$title), review$findings)
  expect_length(f, 1)
  expect_equal(f[[1]]$group, "Duzitol, normal subjects")
  expect_equal(f[[1]]$severity, "ok")
  expect_match(f[[1]]$detail, "5 points compared")
  fig <- review$models[["Duzitol, normal subjects"]]$figures[[1]]
  expect_equal(fig$kind, "decrement")
  expect_lt(fig$mdape, 0.01)
  # Steady state is the limit of a long infusion: 41.3 min.
  expect_equal(round(paperReviewDecrementTime(A, l, 0.5), 1), 41.3)
  # The report renders it with its own axes.
  expect_true(grepl("Fig 1", paperReviewReportPage(review)))
})

test_that("decrement times hold the concentration constant (TCI), and equal steady state in the limit", {
  m <- threeCompartmentFromHalfLives(0.68, 7.6, 66, 1, 10, 100)
  set <- list(v1 = m$v1, v2 = m$v2, v3 = m$v3, cl1 = m$cl1, cl2 = m$cl2, cl3 = m$cl3)
  expect_equal(paperReviewTCIDecrementTime(set, 0.5, Inf), 41.3, tolerance = 2e-3)
  expect_equal(paperReviewTCIDecrementTime(set, 0.5, 1e6), paperReviewTCIDecrementTime(set, 0.5, Inf),
               tolerance = 1e-8)
  # Held for 120 min, the plasma concentration falls 50% sooner than at
  # steady state: the peripheral compartments are not yet full.
  t120 <- paperReviewTCIDecrementTime(set, 0.5, 120)
  expect_lt(t120, 41.3)
  # An independent check: integrate the three compartments numerically,
  # plasma held at 1 for 120 min (amounts filled by Euler steps), then free.
  k10 <- m$cl1 / m$v1; k12 <- m$cl2 / m$v1; k13 <- m$cl3 / m$v1; k21 <- m$cl2 / m$v2; k31 <- m$cl3 / m$v3
  dt <- 0.001; a2 <- 0; a3 <- 0
  for (i in seq_len(120 / dt)) { a2 <- a2 + dt * (k12 * m$v1 - k21 * a2); a3 <- a3 + dt * (k13 * m$v1 - k31 * a3) }
  a1 <- m$v1; t <- 0
  while (a1 / m$v1 > 0.5) {
    d1 <- -(k10 + k12 + k13) * a1 + k21 * a2 + k31 * a3
    a2 <- a2 + dt * (k12 * a1 - k21 * a2); a3 <- a3 + dt * (k13 * a1 - k31 * a3)
    a1 <- a1 + dt * d1; t <- t + dt
  }
  expect_equal(t120, t, tolerance = 2e-3)
})

test_that("an effect-site decrement time runs through stanpumpR's effect-site TCI", {
  ex <- paperReviewExample()                     # exampleol has an effect site (tPeak 3 min)
  review <- paperReviewRun(ex)
  t <- paperReviewEffectSiteDecrementTime(review$drug, review$reference, 0.5, 60)
  expect_true(is.finite(t) && t > 0)
  # The effect site lags the plasma, so its fall is slower than the plasma's.
  expect_gt(t, paperReviewTCIDecrementTime(review$drug$model$PK$default, 0.5, 60))
})

test_that("with no SE printed, conditioning uses the printed rounding", {
  s <- paperReviewSE(66, list(type = "not_reported", valueText = "66"))
  expect_equal(s$se, 0.5)
  expect_match(s$basis, "printed rounding")
})

test_that("the simulator offers each model and simulates the one chosen", {
  # Built first: duzitolExtraction() itself starts from paperReviewExample().
  x <- duzitolExtraction()
  testthat::local_mocked_bindings(paperReviewExample = function() x,
                                  paperReviewApiKey = function(...) NULL)
  shiny::testServer(paperReviewServer, {
    session$setInputs(example = 1)
    expect_equal(names(review()$models), names(duzitolTable))
    expect_equal(currentReview()$drug$defaults$Drug, "Duzitol, normal subjects (paper)")
    session$setInputs(model = "Propofol, normal subjects")
    expect_equal(currentReview()$drug$defaults$Drug, "Propofol, normal subjects (paper)")
    expect_equal(currentReview()$drug$model$PK$default$cl1, 1.9)
  })
  # The report has a section per model and one for the paper.
  page <- paperReviewReportPage(paperReviewRun(duzitolExtraction()))
  for (g in names(duzitolTable)) expect_true(grepl(g, page, fixed = TRUE))
  expect_true(grepl("The paper as a whole", page, fixed = TRUE))
})

# Table 2: each exponent's share of the fall after a bolus, A(i) / sum(A),
# printed as a percent.
duzitolFractions <- list(
  "Duzitol, normal subjects" = c("75", "20", "5"),
  "Duzitol, hepatic failure" = c("97.5", "2", "0.5"),
  "Propofol, normal subjects" = c("94.6", "4.9", "0.57"))

duzitolWithFractions <- function(x = duzitolExtraction()) {
  for (g in names(duzitolFractions))
    for (i in 1:3)
      x$derived_quantities[[length(x$derived_quantities) + 1]] <- list(
        quantity = paste0("coefficient_", i), group = g,
        value = as.numeric(duzitolFractions[[g]][i]), value_text = duzitolFractions[[g]][i],
        units = "percent", statistic = "typical_value",
        uncertainty = list(type = "not_reported"), location = "Table 2, p. 329")
  x
}

test_that("fractional coefficients printed as a percent are compared with A(i) / sum(A)", {
  review <- paperReviewRun(duzitolWithFractions())
  for (g in names(duzitolFractions)) {
    t <- review$models[[g]]$consistency
    rows <- t[grepl("^coefficient_", t$quantity), ]
    expect_equal(nrow(rows), 3)
    expect_true(all(rows$status %in% c("matches", "matches within rounding")),
                info = paste(g, paste(rows$status, collapse = ", ")))
  }
  # Shafer's spreadsheet: the normal subjects' fractions are 0.751, 0.199, 0.050.
  t <- review$models[[1]]$consistency
  expect_equal(as.numeric(t$model[t$quantity == "coefficient_1"]), 75.1, tolerance = 1e-3)
})

test_that("V1, the fractional coefficients and the half-lives close the model", {
  x <- duzitolWithFractions()
  # Drop CL and Vss, which the half-life route would otherwise use first.
  x$parameters <- Filter(function(p) p$role != "CL", x$parameters)
  x$derived_quantities <- Filter(function(d) d$quantity != "Vss", x$derived_quantities)
  review <- paperReviewRun(x)
  m <- review$models[["Duzitol, normal subjects"]]
  expect_equal(m$identifiability$route, "V1, the fractional coefficients and the half-lives")
  expect_true(m$drug$ok)
  # The printed percents carry two figures, so CL comes back near 0.68 L/min.
  expect_equal(m$identifiability$closed$cl1, 0.68, tolerance = 0.05)
})
