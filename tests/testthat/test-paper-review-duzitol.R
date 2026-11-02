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

test_that("the fractional coefficients never close the model", {
  # They are always derived, never primary (Shafer): V1, the percents and the
  # half-lives leave the model open.
  x <- duzitolWithFractions()
  x$parameters <- Filter(function(p) p$role != "CL", x$parameters)
  x$derived_quantities <- Filter(function(d) d$quantity != "Vss", x$derived_quantities)
  review <- paperReviewRun(x)
  m <- review$models[["Duzitol, normal subjects"]]
  expect_false(identical(m$identifiability$route, "V1, the fractional coefficients and the half-lives"))
  expect_false(isTRUE(m$drug$ok))
})

test_that("a short decrement curve read by eye is judged against the reading's precision", {
  m <- threeCompartmentFromHalfLives(0.68, 7.6, 66, 1, 10, 100)
  set <- list(v1 = m$v1, v2 = m$v2, v3 = m$v3, cl1 = m$cl1, cl2 = m$cl2, cl3 = m$cl3)
  durations <- c(30, 60, 120, 240)
  # The 30% curve runs 4.4 to 12.1 min; read 2 min low, as by eye, it is
  # about 30% off though every point is inside the +/-3 min of the reading.
  y <- vapply(durations, function(T) paperReviewTCIDecrementTime(set, 0.3, T), 0) - 2
  figure <- function(note, precision = NULL) {
    x <- duzitolExtraction()
    x$figures <- list(list(figure_id = "Fig 3 (30% curve)", caption = "30% decrement times",
      regimen_id = NULL, y_quantity = "decrement_time", decrement_percent = 30,
      decrement_site = "plasma", y_units = "other", time_units = "min", log_y = "no",
      data_kind = "model_prediction", one_regimen = "yes", error_bars = "none",
      reading_precision = precision,
      series = list(list(label = "Duzitol, normal subjects", group = "Duzitol, normal subjects",
                         analyte = "parent",
                         points = lapply(seq_along(durations), function(i)
                           list(time = durations[i], value = round(y[i], 2))))),
      digitization_note = note, location = "Fig 3, p. 330"))
    f <- Filter(function(z) grepl("Fig 3", z$title), paperReviewRun(x)$findings)
    f[[1]]
  }
  f <- figure("Read by eye, about \u00b13 min.")
  expect_equal(f$severity, "ok")
  expect_match(f$detail, "4 of 4 points are within")
  expect_match(f$detail, "from Claude's digitisation note")
  expect_equal(figure("Read by eye.", precision = 3)$severity, "ok")
  # Without a stated precision, 2% of the largest value read is far tighter.
  expect_false(identical(figure("Read by eye.")$severity, "ok"))
})

test_that("V1, the half-lives, k21 and k31 close the model (Convert.xls tab 5)", {
  x <- duzitolExtraction()
  x$parameters <- Filter(function(p) p$role != "CL", x$parameters)
  x$derived_quantities <- Filter(function(d) d$quantity != "Vss", x$derived_quantities)
  g <- "Duzitol, normal subjects"
  rate <- function(role, text)
    list(name_in_paper = role, group = g, role = role, value = as.numeric(text), value_text = text,
         units = "1/min", uncertainty = list(type = "not_reported"), status = "estimated",
         location = "Spreadsheet")
  x$parameters <- c(x$parameters, list(rate("k21", "0.217689586"), rate("k31", "0.017097925")))
  m <- paperReviewRun(x)$models[[g]]
  expect_equal(m$identifiability$route, "V1, the half-lives, k21 and k31")
  expect_equal(m$identifiability$closed$cl1, 0.68, tolerance = 1e-6)
  expect_equal(sum(unlist(m$identifiability$closed[c("v1", "v2", "v3")])), 66, tolerance = 1e-6)
})

test_that("the review lists every parameter of each model, at the reference patient", {
  review <- paperReviewRun(duzitolExtraction())
  t <- review$models[["Duzitol, normal subjects"]]$pkTable
  value <- function(p) as.numeric(t$Value[t$Parameter == p])
  # Shafer's spreadsheet for the normal subjects.
  expect_equal(value("k21"), 0.2177, tolerance = 1e-4)
  expect_equal(value("k13"), 0.1046, tolerance = 1e-3)
  expect_equal(value("A1 / sum(A)"), 75.1, tolerance = 1e-3)
  expect_equal(value("Vdss"), 66)
  expect_true(grepl("Every parameter of the model", paperReviewReportPage(review)))
})

test_that("a figure normalized to its peak is compared without any dose", {
  # Figure 2: the plasma concentration after a bolus as a percent of its
  # peak, which is independent of the dose.
  m <- threeCompartmentFromHalfLives(0.68, 7.6, 66, 1, 10, 100)
  k21 <- m$cl2 / m$v2; k31 <- m$cl3 / m$v3
  l <- sort(cube(m$cl1 / m$v1, m$cl2 / m$v1, m$cl3 / m$v1, k21, k31), decreasing = TRUE)
  A <- vapply(1:3, function(j) { o <- setdiff(1:3, j)
    (k21 - l[j]) * (k31 - l[j]) / prod(l[o] - l[j]) / m$v1 }, 0)
  times <- c(1, 5, 15, 30, 60, 120, 240)
  y <- vapply(times, function(t) 100 * sum(A * exp(-l * t)) / sum(A), 0)
  x <- duzitolExtraction()
  x$figures <- list(list(figure_id = "Fig 2", caption = "Percent of peak after a bolus", regimen_id = NULL,
    y_quantity = "concentration", y_normalization = "percent_of_peak", concentration_site = "plasma",
    y_units = "other", time_units = "min", log_y = "yes", data_kind = "model_prediction",
    one_regimen = "yes", error_bars = "none",
    series = list(list(label = "Duzitol, normal subjects (solid)", group = "Duzitol, normal subjects",
                       analyte = "parent",
                       points = lapply(seq_along(times), function(i)
                         list(time = times[i], value = signif(y[i], 2))))),
    digitization_note = "exact to two figures", location = "Fig 2, p. 329"))
  review <- paperReviewRun(x)
  f <- Filter(function(z) grepl("Fig 2", z$title), review$findings)
  expect_length(f, 1)
  expect_equal(f[[1]]$severity, "ok")
  # The title names the curve, so three models' checks of one figure are told apart.
  expect_match(f[[1]]$title, "Fig 2, Duzitol, normal subjects (solid)", fixed = TRUE)
  expect_match(f[[1]]$detail, "a unit intravenous bolus")
  fig <- review$models[["Duzitol, normal subjects"]]$figures[[1]]
  expect_equal(fig$kind, "normalized")
  expect_lt(fig$mdape, 0.03)
  expect_true(grepl("Fig 2", paperReviewReportPage(review)))
})

test_that("an effect-site curve is divided by its own, later and lower, peak", {
  ex <- paperReviewExample()                     # exampleol: tPeak 3 min
  review <- paperReviewRun(ex)
  pk <- getDrugPK(review$drug$defaults$Drug, review$reference$weight, review$reference$height,
                  review$reference$age, review$reference$sex, review$drug$defaults,
                  model = review$drug$model)$PK$default
  l <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3); A <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)
  ce <- function(t) sum(A * pk$ke0 / (pk$ke0 - l) * (exp(-l * t) - exp(-pk$ke0 * t)))
  peak <- ce(3)
  times <- c(1, 3, 10, 30, 60)
  ex$figures <- list(list(figure_id = "Fig E", caption = "Effect site, percent of peak", regimen_id = NULL,
    y_quantity = "concentration", y_normalization = "percent_of_peak", concentration_site = "effect_site",
    y_units = "other", time_units = "min", log_y = "no", data_kind = "model_prediction",
    one_regimen = "yes", error_bars = "none",
    series = list(list(label = "Ce", analyte = "parent",
                       points = lapply(times, function(t) list(time = t, value = round(100 * ce(t) / peak, 1))))),
    digitization_note = "exact", location = "Fig E"))
  f <- Filter(function(z) grepl("Fig E", z$title), paperReviewRun(ex)$findings)
  expect_equal(f[[1]]$severity, "ok")
  expect_match(f[[1]]$detail, "effect-site")
  # Divided by the plasma's peak (the sum of the coefficients) instead, the
  # effect site would read far lower: that is the error the maximum avoids.
  expect_lt(peak / sum(A), 0.5)
})

test_that("each curve of a figure is read against the figure's axes, and steep curves along the duration", {
  m <- threeCompartmentFromHalfLives(0.68, 7.6, 66, 1, 10, 100)
  set <- list(v1 = m$v1, v2 = m$v2, v3 = m$v3, cl1 = m$cl1, cl2 = m$cl2, cl3 = m$cl3)
  dec <- function(fall, T) paperReviewTCIDecrementTime(set, fall, T)
  curve <- function(fall, points)
    list(figure_id = paste0("Fig 3 (", 100 * fall, "%)"), caption = "Decrement times", regimen_id = NULL,
         y_quantity = "decrement_time", decrement_percent = 100 * fall, decrement_site = "plasma",
         y_units = "other", time_units = "min", log_y = "no", data_kind = "model_prediction",
         one_regimen = "yes", error_bars = "none",
         series = list(list(label = paste0(100 * fall, "%"), group = "Duzitol, normal subjects",
                            analyte = "parent",
                            points = lapply(points, function(p) list(time = p[1], value = p[2])))),
         digitization_note = "Read by eye.", location = "Fig 3, p. 330")
  # The 10% curve, 0.5 to 1.3 min, read 0.7 min high: far off in percent, a
  # small fraction of an axis that the 50% curve takes to 32 min.
  ten <- lapply(c(30, 120), function(T) c(T, dec(0.1, T) + 0.7))
  fifty <- lapply(c(30, 60, 120), function(T) c(T, dec(0.5, T)))
  # The 80% curve is steep: read 2 min late in duration, it is 4 min out in
  # decrement time.
  T80 <- stats::uniroot(function(T) dec(0.8, T) - 60, c(1, 120))$root
  eighty <- list(c(T80 + 2, 60))
  expect_gt(abs(dec(0.8, T80 + 2) - 60), 4)
  x <- duzitolExtraction()
  x$figures <- list(curve(0.1, ten), curve(0.5, fifty), curve(0.8, eighty))
  review <- paperReviewRun(x)
  for (pc in c(10, 50, 80)) {
    f <- Filter(function(z) grepl(paste0("Fig 3 (", pc, "%)"), z$title, fixed = TRUE), review$findings)
    expect_equal(f[[1]]$severity, "ok", info = paste(pc, f[[1]]$detail))
    expect_match(f[[1]]$detail, "largest value read on the figure")
  }
  f80 <- Filter(function(z) grepl("Fig 3 (80%)", z$title, fixed = TRUE), review$findings)[[1]]
  expect_match(f80$detail, "1 of 1 points are within")
  # A printed axis is used before the values read.
  p <- paperReviewReadingPrecision(c(curve(0.1, ten), list(y_axis_max = 120)), axis = "y")
  expect_equal(p$value, 2.4)
})

test_that("a 'matches within rounding' is counted as compared, and a decrement time is read at steady state", {
  review <- paperReviewRun(duzitolExtraction())
  for (g in names(duzitolTable)) {
    f <- Filter(function(z) identical(z$group, g) && grepl("printed derived quantities", z$title),
                review$findings)[[1]]
    expect_match(f$title, "^5 of 5 ")
    expect_equal(f$severity, "ok")
    expect_false(grepl("could not be compared", f$detail))
  }
  note <- function(g) { t <- review$models[[g]]$consistency; t$note[t$quantity == "decrement_50"] }
  expect_match(note("Duzitol, normal subjects"), "At steady state")
  # Hepatic failure is 8.2 min after 10 h of TCI against 9.9 at steady state.
  expect_match(note("Duzitol, hepatic failure"), "within 5% of it after 20 h", fixed = TRUE)
})

test_that("each model's curve of one figure is judged by the figure's precision, not by its median error", {
  set <- function(g) { m <- do.call(threeCompartmentFromHalfLives, as.list(unname(duzitolTable[[g]])))
    list(v1 = m$v1, v2 = m$v2, v3 = m$v3, cl1 = m$cl1, cl2 = m$cl2, cl3 = m$cl3) }
  read <- function(g, durations, offset)
    lapply(durations, function(T) list(time = T, value = paperReviewTCIDecrementTime(set(g), 0.5, T) + offset))
  series <- function(g, label, points) list(label = label, group = g, analyte = "parent", points = points)
  x <- duzitolExtraction()
  x$figures <- list(list(figure_id = "Fig 1", caption = "50% decrement times", regimen_id = NULL,
    y_quantity = "decrement_time", decrement_percent = 50, decrement_site = "plasma",
    y_units = "other", time_units = "min", log_y = "no", data_kind = "model_prediction",
    one_regimen = "yes", error_bars = "none",
    series = list(series("Duzitol, normal subjects", "solid", read("Duzitol, normal subjects", c(60, 240, 600), 0.3)),
                  series("Duzitol, hepatic failure", "dashed", read("Duzitol, hepatic failure", c(30, 240, 600), 2.5)),
                  # Propofol read 2 min high: a median error under 20%.
                  series("Propofol, normal subjects", "dotted", read("Propofol, normal subjects", c(240, 360, 600), 2))),
    digitization_note = "Read by eye from a linear axis (0\u201350 min).", location = "Fig 1, p. 328"))
  review <- paperReviewRun(x)
  f <- function(g) Filter(function(z) identical(z$group, g) && grepl("^Fig 1", z$title), review$findings)[[1]]
  expect_lt(review$models[["Propofol, normal subjects"]]$figures[[1]]$mdape, 0.2)
  expect_equal(f("Duzitol, normal subjects")$severity, "ok")
  expect_equal(f("Propofol, normal subjects")$severity, "concern")
  expect_equal(f("Duzitol, hepatic failure")$severity, "concern")
  # One precision for the whole figure: 2% of the 0-50 min axis.
  for (g in names(duzitolTable))
    expect_match(f(g)$detail, "+/-1 min in the decrement time (2% of the axis, 0 to 50", fixed = TRUE)
})
