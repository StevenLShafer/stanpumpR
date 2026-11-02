# Single Paper Review: the checks (R/paper-review-checks.R) against fixtures
# whose answers are known.  The fixtures are built by
# tests/testthat/fixtures/make-paper-review-fixtures.R with base R
# (eigen() of the rate matrix), not with stanpumpR, and the numbers pinned
# here are that script's output.

readFixture <- function(name)
  jsonlite::fromJSON(test_path("fixtures", paste0(name, ".json")), simplifyVector = FALSE)

findingsOf <- function(review, section)
  Filter(function(f) identical(f$section, section), review$findings)

severities <- function(findings) vapply(findings, function(f) f$severity, character(1))

test_that("the example's printed parameters close the model, and only the misprint disagrees", {
  review <- paperReviewRun(paperReviewExample())
  expect_equal(review$identifiability$verdict, "printed")
  t <- review$consistency
  expect_equal(t$status[t$quantity == "t_half_2"], "disagrees")
  others <- t[t$quantity != "t_half_2", ]
  expect_true(all(others$status == "matches"), info = paste(others$quantity, others$status))
  # The error cites the table it came from.
  err <- Filter(function(f) f$severity == "error", findingsOf(review, "Internal consistency"))
  expect_length(err, 1)
  expect_equal(err[[1]]$locations, "Table 3, p. 6")
  expect_true(err[[1]]$computed)
})

test_that("recomputed half-lives match an independent eigen() calculation", {
  ex <- paperReviewExample()
  reference <- paperReviewReferencePatient(ex)
  drug <- paperReviewDrug(ex, reference)
  pk <- getDrugPK(drug$defaults$Drug, reference$weight, reference$height, reference$age,
                  reference$sex, drug$defaults, model = drug$model)$PK$default
  # From the fixture script: eigen() of the rate matrix
  expect_equal(c(pk$lambda_1, pk$lambda_2, pk$lambda_3),
               c(0.533393937932, 0.043363495467, 0.003242566601), tolerance = 1e-9)
  expect_equal(c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3),
               c(0.1101863309487, 0.0138143541574, 0.0009993148939), tolerance = 1e-9)
})

test_that("CL, Vss and both half-lives close two compartments; wide SEs make it ill-conditioned", {
  well <- paperReviewRun(readFixture("paper-review-well-conditioned"))
  ill  <- paperReviewRun(readFixture("paper-review-ill-conditioned"))
  expect_equal(well$identifiability$verdict, "derived")
  expect_equal(ill$identifiability$verdict, "ill_conditioned")
  expect_equal(well$identifiability$route, "CL, Vss and both half-lives")
  # The closure is the existing helper's, per kilogram times the reference weight.
  expected <- twoCompartmentFromHalfLives(0.285, 1.9, 2.0, 9.2)
  closed <- well$identifiability$closed
  expect_equal(closed$v1, expected$v1 * 70, tolerance = 1e-12)
  expect_equal(closed$cl2, expected$cl2 * 70, tolerance = 1e-12)
  expect_true(closed$perKg)
  # The closed model reproduces the half-lives it was built from.  (Checked
  # by row: an all() over an empty table passed while the two-compartment
  # model could not be built at all, which the first live review found.)
  t <- well$consistency
  expect_false(is.null(t))
  for (q in c("t_half_1", "t_half_2", "Vss", "CL"))
    expect_equal(t$status[t$quantity == q], "matches", info = q)
})

test_that("CL, Vss and the terminal half-life alone are underdetermined (the hydralazine case)", {
  x <- readFixture("paper-review-well-conditioned")
  x$derived_quantities <- Filter(function(d) d$quantity != "t_half_1", x$derived_quantities)
  x$derived_quantities[[3]]$quantity <- "t_half_terminal"
  review <- paperReviewRun(x)
  expect_equal(review$identifiability$verdict, "underdetermined")
  expect_null(review$identifiability$closed)
  f <- findingsOf(review, "Identifiability")
  expect_true(any(grepl("does not close", vapply(f, `[[`, "", "title"))))
})

test_that("the biexponential helper inverts a known two-compartment model", {
  V1 <- 10; k10 <- 0.1; k12 <- 0.2; k21 <- 0.05
  K <- matrix(c(-(k10 + k12), k21, k12, -k21), 2, byrow = TRUE)
  l <- sort(-eigen(K)$values, decreasing = TRUE)
  A <- c((k21 - l[1]) / (l[2] - l[1]), (k21 - l[2]) / (l[1] - l[2])) / V1
  out <- twoCompartmentFromBiexponential(A, l)
  expect_equal(out$v1, V1, tolerance = 1e-12)
  expect_equal(out$cl1, k10 * V1, tolerance = 1e-12)
  expect_equal(out$cl2, k12 * V1, tolerance = 1e-12)
  expect_equal(out$v2, k12 * V1 / k21, tolerance = 1e-12)
  expect_error(twoCompartmentFromBiexponential(c(0.1, -0.2), l), "no physically admissible")
})

test_that("a model closed from exponentials is found by the identifiability check", {
  ex <- paperReviewExample()
  # Remove the printed volumes and clearances: the coefficients (per litre)
  # and exponents printed in Table 3 still close it, through the triexponential.
  ex$parameters <- Filter(function(p) p$role %in% c("tPeak"), ex$parameters)
  lambda <- c(0.533393937932, 0.043363495467, 0.003242566601)
  ex$derived_quantities <- c(ex$derived_quantities, lapply(1:3, function(i)
    list(quantity = paste0("lambda_", i), value = lambda[i], value_text = format(lambda[i], digits = 10),
         units = "1/min", regimen_id = NULL, statistic = "typical_value",
         uncertainty = list(type = "not_reported"), location = "Table 3, p. 6")))
  review <- paperReviewRun(ex)
  expect_equal(review$identifiability$verdict, "derived")
  expect_match(review$identifiability$route, "triexponential")
  expect_equal(review$identifiability$closed$v1, 8, tolerance = 1e-3)
  expect_equal(review$identifiability$closed$cl1, 1.2, tolerance = 1e-3)
})

test_that("a covariate equation that is not finite and positive for a legal patient is an error", {
  ex <- paperReviewExample()
  ex$covariate_effects <- list(
    list(parameter_role = "CL", covariate = "age", form = "power", coefficient = -0.4,
         reference_value = 40, covariate_units = "years", equation_as_printed = "(AGE/40)^-0.4",
         location = "Equation 4"),
    list(parameter_role = "V1", covariate = "weight", form = "linear", coefficient = 0.02,
         reference_value = 70, covariate_units = "kg", equation_as_printed = "1 + 0.02 (WT - 70)",
         location = "Equation 5"))
  f <- paperReviewCovariateChecks(ex)
  titles <- vapply(f, `[[`, "", "title")
  bad <- f[severities(f) == "error"]
  expect_length(bad, 2)
  # (0/40)^-0.4 is infinite at age 0; 1 + 0.02 (3 - 70) is negative at 3 kg.
  expect_true(any(grepl("CL ~ age", vapply(bad, `[[`, "", "title"))))
  expect_true(any(grepl("V1 ~ weight", vapply(bad, `[[`, "", "title"))))
  expect_true(any(grepl("negative power of age", titles)))
  expect_true(all(vapply(bad, function(x) length(x$locations) == 1, logical(1))))
})

test_that("a covariate the simulator does not take is recorded and held at reference", {
  ex <- paperReviewExample()
  ex$covariate_effects <- list(list(parameter_role = "CL", covariate = "creatinine_clearance",
    form = "power", coefficient = 0.6, reference_value = 100, covariate_units = "mL/min",
    equation_as_printed = "(CrCL/100)^0.6", location = "Equation 6"))
  f <- paperReviewCovariateChecks(ex)
  expect_true(any(grepl("^Not applied", vapply(f, `[[`, "", "title"))))
  drug <- paperReviewDrug(ex, list(weight = 50, height = 160, age = 80, sex = "female"))
  expect_equal(drug$model$PK$default$cl1, 1.2)
})

test_that("a difference of independent means is tested against both variances, and pairing is detected", {
  d <- list(description = "CL", group1_value = 1.1, group1_se = 0.08, group2_value = 1.3,
            group2_se = 0.09, difference = 0.2, difference_se = NULL, paired = "no",
            p_value = 0.04, location = "Results")
  f <- paperReviewDifferenceFinding(d, "x")
  expect_equal(f$severity, "concern")
  expect_match(f$detail, "t = 1.7")
  # Hui's case: a printed SE of the difference well below the independent sum
  # implies a positive correlation, i.e. paired data.
  d$difference_se <- 0.05
  f <- paperReviewDifferenceFinding(d, "x")
  rho <- (0.08^2 + 0.09^2 - 0.05^2) / (2 * 0.08 * 0.09)
  expect_match(f$detail, paste0("correlation of ", signif(rho, 2)))
  expect_match(f$detail, "(paired)", fixed = TRUE)
  expect_equal(f$severity, "ok")
})

test_that("the library's rules are applied to the paper's own statements", {
  ex <- paperReviewExample()
  ex$bioavailability <- list(source = "fitted_to_other_study_exposure", note = "", location = "Methods, p. 4")
  ex$drug$dose_basis <- "unclear"
  f <- paperReviewAssumptionChecks(ex)
  titles <- vapply(f, `[[`, "", "title")
  expect_equal(severities(f)[grepl("Bioavailability was fitted", titles)], "error")
  expect_equal(severities(f)[grepl("dose basis is not stated", titles)], "concern")
  anatomy <- f[grepl("anatomical", titles)][[1]]
  expect_false(anatomy$computed)
  expect_equal(anatomy$locations, "Discussion, p. 9")
})

test_that("the figure check reproduces the fixture's points and flags a figure that disagrees", {
  review <- paperReviewRun(paperReviewExample())
  expect_length(review$figures, 2)
  for (fig in review$figures) expect_lt(fig$mdape, 0.06)
  f <- findingsOf(review, "Figures")
  expect_true(all(severities(f) == "ok"))
  expect_true(all(grepl("approximately", vapply(f, `[[`, "", "detail"))))

  ex <- paperReviewExample()
  for (i in seq_along(ex$figures[[1]]$series[[1]]$points))
    ex$figures[[1]]$series[[1]]$points[[i]]$value <- ex$figures[[1]]$series[[1]]$points[[i]]$value * 2
  f <- findingsOf(paperReviewRun(ex), "Figures")
  expect_equal(severities(f)[1], "error")
})

# The first real paper (an intravenous active drug and two oral doses of its
# prodrug, with no absorption constant) produced false alarms that these pin
# down: oral exposures compared against an intravenous-only model, a figure's
# oral series scored against the intravenous curve, and coefficients left
# unchecked because the intravenous dose was a short infusion.
oralRegimen <- function(id = "R3")
  list(id = id, description = "100 mg orally",
       doses = list(list(route = "oral", time = 0, amount = 100, mass_unit = "mg", per_kg = "no",
                         per_time = "none", duration = NULL, time_units = "min")),
       location = "Methods, p. 3")
derivedRow <- function(q, value, units, regimen, statistic = "mean_of_individuals")
  list(quantity = q, value = value, value_text = as.character(value), units = units,
       regimen_id = regimen, statistic = statistic, uncertainty = list(type = "SE", value = 1),
       location = "Table 1")

# Exampleol's disposition, from the fixture script's eigen() calculation, and
# an independent closed form for a first-order oral dose on it (mg in, ng/mL
# out), so the absorption derivations are checked against something
# stanpumpR did not compute.
exLambda <- c(0.533393937932, 0.043363495467, 0.003242566601)
exA <- c(0.1101863309487, 0.0138143541574, 0.0009993148939)
oralNgPerMl <- function(t, dose, F, ka)
  1000 * dose * F * colSums(exA * ka / (ka - exLambda) *
                              (exp(-outer(exLambda, t)) - matrix(exp(-ka * t), 3, length(t), byrow = TRUE)))

test_that("a regimen whose absorption cannot be derived is not compared, and is not called a disagreement", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "iv_infusion", "oral")
  ex$regimens[[3]] <- oralRegimen()
  # An AUC gives F, but nothing gives ka: no ka, MAT, MRT, tmax or figure.
  ex$derived_quantities <- c(ex$derived_quantities, list(
    derivedRow("AUC_inf", 250, "ng*h/mL", "R3"), derivedRow("Cmax", 99, "ng/mL", "R3")))
  review <- paperReviewRun(ex)
  expect_false(review$absorptions$R3$ok)
  t <- review$consistency
  oral <- t[grepl("^(250|99)$", t$printed), ]
  expect_equal(nrow(oral), 2)
  expect_true(all(oral$status == "not comparable"))
  expect_true(all(grepl("nothing in the paper gives an absorption constant", oral$note)))
  titles <- vapply(review$findings, `[[`, "", "title")
  expect_false(any(grepl("^(AUC_inf|Cmax) as printed", titles)))
  expect_true(any(grepl("No absorption can be derived for R3", titles)))
})

test_that("ka comes from the mean absorption time, F from the paper's AUCs, and the dose basis from its statement", {
  # A test paper's pattern: no ka, but F (or the AUCs), MAT and an equimolar
  # prodrug dose.  100 mg of which 0.8 is the analyte, true F 0.5, ka 1/50 min.
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "iv_infusion", "oral")
  ex$regimens[[3]] <- oralRegimen()
  ex$regimens[[3]]$doses[[1]]$analyte_fraction <- 0.8
  aucPO <- 0.5 * 0.8 * 100 / 1.2 * 1000 / 60            # ng.h/mL
  grid <- seq(0.5, 600, by = 0.5)
  cmax <- max(oralNgPerMl(grid, 100, 0.4, 0.02))
  ex$derived_quantities <- c(ex$derived_quantities, list(
    derivedRow("MAT", 50, "min", "R3", "typical_value"),
    derivedRow("AUC_inf", round(aucPO, 1), "ng*h/mL", "R3", "typical_value"),
    derivedRow("Cmax", signif(cmax, 4), "ng/mL", "R3", "typical_value")))
  review <- paperReviewRun(ex)
  a <- review$absorptions$R3
  expect_true(a$ok)
  expect_equal(a$ka, 1 / 50)
  expect_equal(a$fraction, 0.8)
  expect_equal(a$F, 0.5, tolerance = 1e-3)               # the IV AUC is printed rounded, 972
  expect_match(a$sources[["F"]], "dose-normalised AUCs")
  t <- review$consistency
  # The AUC built F, so it cannot also check it; Cmax is a genuine check.
  expect_equal(t$status[t$quantity == "AUC_inf" & t$printed == as.character(round(aucPO, 1))],
               "used to build the model")
  expect_true(t$status[t$quantity == "Cmax"] %in% c("matches", "close"))
  drug <- paperReviewDrugFor(review$drug, ex, ex$regimens[[3]], review$reference)
  expect_equal(drug$model$PK$default$bioavailability_PO, 0.4, tolerance = 1e-3)
})

test_that("ka is solved from tmax when nothing else gives it", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "oral")
  ex$regimens[[3]] <- oralRegimen()
  ex$parameters <- c(ex$parameters, list(list(
    name_in_paper = "F", role = "F", route = "oral", regimen_id = "R3", value = 50, value_text = "50",
    units = "percent", uncertainty = list(type = "not_reported"), status = "estimated", location = "T4")))
  grid <- seq(0.05, 400, by = 0.05)
  tmax <- grid[which.max(oralNgPerMl(grid, 100, 0.5, 0.02))]
  ex$derived_quantities <- c(ex$derived_quantities, list(derivedRow("tmax", tmax, "min", "R3")))
  a <- paperReviewRun(ex)$absorptions$R3
  expect_true(a$ok)
  expect_equal(a$ka, 0.02, tolerance = 5e-3)
  expect_match(a$sources[["ka"]], "tmax")
})

test_that("ka and F are fitted to a regimen's digitised curve, which then does not check itself", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "iv_infusion", "oral")
  ex$regimens[[3]] <- oralRegimen()
  times <- c(15, 30, 60, 120, 240, 480)
  y <- oralNgPerMl(times, 100, 0.5, 0.02)
  ex$figures[[3]] <- list(figure_id = "Figure 4", caption = "oral", regimen_id = "R3",
    y_quantity = "concentration", y_units = "ng/mL", time_units = "min", log_y = "no",
    data_kind = "mean", error_bars = "none",
    series = list(list(label = "Oral", points = lapply(seq_along(times), function(i)
      list(time = times[i], value = signif(y[i], 6))))),
    digitization_note = "exact", location = "Figure 4, p. 8")
  review <- paperReviewRun(ex)
  a <- review$absorptions$R3
  expect_true(a$ok)
  expect_equal(a$ka, 0.02, tolerance = 1e-3)
  expect_equal(a$F, 0.5, tolerance = 1e-3)
  expect_match(a$sources[["ka"]], "Figure 4's digitised points")
  expect_true("Figure 4" %in% a$fittedFigures)
  f <- Filter(function(x) identical(x$section, "Figures") && grepl("Figure 4", x$title), review$findings)
  expect_length(f, 1)
  expect_match(f[[1]]$detail, "fitted to these points")
  # A printed ka is cross-checked against the same fit instead.
  ex$parameters <- c(ex$parameters, list(list(
    name_in_paper = "ka", role = "ka", route = "oral", value = 0.03, value_text = "0.03",
    units = "1/min", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2")))
  a <- paperReviewRun(ex)$absorptions$R3
  expect_equal(a$ka, 0.03)
  expect_match(a$crossCheck, "A fit to Figure 4's digitised points gives ka 1.2 /h against 1.8 /h")
})

test_that("an oral MRT is the disposition's Vss/CL plus 1/ka plus the lag", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "oral")
  ex$regimens[[3]] <- oralRegimen()
  route <- function(role, value, units)
    list(name_in_paper = role, role = role, route = "oral", value = value, value_text = as.character(value),
         units = units, uncertainty = list(type = "not_reported"), status = "estimated", location = "T2")
  ex$parameters <- c(ex$parameters, list(route("ka", 0.02, "1/min"), route("tlag", 15, "min"),
                                         route("F", 60, "percent")))
  expected <- 148 / 1.2 + 1 / 0.02 + 15                 # min
  ex$derived_quantities <- c(ex$derived_quantities,
                             list(derivedRow("MRT", round(expected), "min", "R3", "typical_value")))
  t <- paperReviewRun(ex)$consistency
  row <- t[t$quantity == "MRT", ]
  expect_equal(row$status, "matches")
  expect_equal(as.numeric(row$model), round(expected, 1), tolerance = 1e-3)
})

test_that("a short intravenous infusion scales the printed coefficients, as the paper's own fit does", {
  ex <- paperReviewExample()
  # 1 mg/kg over 0.1 min: a tenth of the fastest half-life (1.30 min) or less.
  ex$regimens[[1]]$doses[[1]] <- list(route = "iv_infusion", time = 0, amount = 1, mass_unit = "mg",
                                      per_kg = "yes", per_time = "none", duration = 0.1,
                                      time_units = "min")
  A1 <- exA[1] * 70 * 1000                               # ng/mL for 70 mg
  ex$derived_quantities <- c(ex$derived_quantities, list(
    list(quantity = "coefficient_1", value = round(A1), value_text = as.character(round(A1)),
         units = "ng/mL", regimen_id = "R1", statistic = "typical_value",
         uncertainty = list(type = "not_reported"), location = "Table 3")))
  t <- paperReviewRun(ex)$consistency
  row <- t[t$quantity == "coefficient_1" & t$units == "ng/mL", ]
  expect_equal(row$status, "matches")
  expect_match(row$note, "treated as a bolus")
  # A longer infusion is not a bolus: not recomputed, and not called a units problem.
  ex$regimens[[1]]$doses[[1]]$duration <- 1
  t <- paperReviewRun(ex)$consistency
  row <- t[t$quantity == "coefficient_1" & t$units == "ng/mL", ]
  expect_equal(row$status, "not recomputed")
})

test_that("each figure series is simulated with its own regimen", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "iv_infusion", "oral")
  ex$regimens[[3]] <- oralRegimen()
  ex$parameters <- c(ex$parameters, list(
    list(name_in_paper = "ka", role = "ka", route = "oral", value = 0.02, value_text = "0.02",
         units = "1/min", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2"),
    list(name_in_paper = "F", role = "F", route = "oral", value = 50, value_text = "50",
         units = "percent", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2")))
  times <- c(30, 60, 120, 240)
  y <- oralNgPerMl(times, 100, 0.5, 0.02)
  ex$figures[[1]]$series[[2]] <- list(label = "Oral", regimen_id = "R3",
    points = lapply(seq_along(times), function(i) list(time = times[i], value = signif(y[i], 6))))
  review <- paperReviewRun(ex)
  f <- Filter(function(x) identical(x$section, "Figures"), review$findings)[[1]]
  expect_equal(f$severity, "ok")
  expect_match(f$detail, "^13 points compared")
  pts <- review$figures[[1]]$points
  oral <- pts[pts$series == "Oral", ]
  # Interpolated on the simulated time line, so to about 1%.
  expect_equal(oral$predicted, oral$observed, tolerance = 0.02)
  expect_equal(length(unique(review$figures[[1]]$curve$regimen)), 2)
})

test_that("the number of exponentials a digitised intravenous curve supports is reported", {
  times <- c(1, 2, 4, 8, 15, 30, 60, 90, 120, 240, 360, 480)
  wiggle <- rep(c(1.02, 0.98, 1.01, 0.99), 3)
  tri <- 70 * 1000 * colSums(exA * exp(-outer(exLambda, times))) * wiggle
  fits <- lapply(1:3, function(k) paperReviewFitExponentials(times, tri, k))
  aic <- vapply(fits, `[[`, 0, "aic")
  expect_equal(which.min(aic), 3)
  expect_equal(fits[[3]]$lambda, exLambda, tolerance = 0.05)
  bi <- 1000 * (5 * exp(-0.1 * times) + 1 * exp(-0.005 * times)) * wiggle
  aic <- vapply(1:3, function(k) paperReviewFitExponentials(times, bi, k)$aic, 0)
  expect_equal(which.min(aic), 2)

  # Through the review: the model has 3, Figure 2's points (9, with 5% noise)
  # are reported against it, with the caveat.
  f <- Filter(function(x) grepl("supports", x$title), paperReviewRun(paperReviewExample())$findings)
  expect_length(f, 1)
  expect_match(f[[1]]$detail, "AIC prefers")
  expect_match(f[[1]]$detail, "approximate")
})

test_that("a model the print does not close is closed from a digitised intravenous curve", {
  ex <- paperReviewExample()
  ex$parameters <- Filter(function(p) p$role == "tPeak", ex$parameters)
  ex$derived_quantities <- list()
  times <- c(1, 2, 4, 8, 15, 30, 60, 90, 120, 240, 360, 480)
  y <- 70 * 1000 * colSums(exA * exp(-outer(exLambda, times)))
  ex$figures[[1]]$series[[1]]$points <- lapply(seq_along(times), function(i)
    list(time = times[i], value = signif(y[i], 6)))
  review <- paperReviewRun(ex)
  expect_equal(review$identifiability$verdict, "derived_from_curve")
  expect_match(review$identifiability$route, "Figure 2")
  expect_equal(review$identifiability$closed$v1, 8, tolerance = 0.01)
  expect_equal(review$identifiability$closed$cl1, 1.2, tolerance = 0.01)
  expect_true(review$drug$ok)
})

# A test paper dosed daily and modelled each day's dose as a constant 24-h
# input.  The schema had no way to say so; each daily dose became a single
# dose, and the absorption fit to the paper's own simulation figures drove
# ka to about 1e-4 /h.  (Fictional numbers below.)
apparentOneCompartment <- function() {
  x <- paperReviewExample()
  x$drug$concentration_units <- "mg/L"
  x$structure$compartments <- 1
  x$structure$routes <- list("oral")
  x$structure$apparent_oral <- "yes"
  x$structure$effect_site <- "none"
  x$covariate_effects <- list()
  p <- function(role, value, units)
    list(name_in_paper = role, role = role, value = value, value_text = as.character(value),
         units = units, uncertainty = list(type = "not_reported"), status = "estimated", location = "T2")
  x$parameters <- list(p("CL", 10, "L/h"), p("V1", 100, "L"))
  x$derived_quantities <- list()
  x$figures <- list()
  x$regimens <- list(list(id = "D1", description = "240 mg/day as a constant input for 30 days",
    doses = list(list(route = "oral", time = 0, amount = 240, mass_unit = "mg", per_kg = "no",
                      per_time = "per_day", duration = 30, time_units = "day")),
    location = "Methods"))
  x
}

test_that("a daily dose modelled as a constant input becomes an oral rate, with no absorption needed", {
  x <- apparentOneCompartment()
  d <- paperReviewRegimenDoses(x$regimens[[1]])$doses
  expect_equal(d$Units, c("mg/day PO", "mg/day PO"))
  expect_equal(d$Time, c(0, 30 * 1440))
  expect_equal(d$Dose, c(240, 0))
  review <- paperReviewRun(x)
  expect_length(review$absorptions, 0)
  expect_true(paperReviewRegimenRoutable(x$regimens[[1]], review$drug)$ok)
  expect_true("mg/day PO" %in% review$drug$units)
  # Css = rate / CL = (240 / 24 mg/h) / (10 L/h) = 1 mg/L, reached well
  # within 30 days (half-life 6.9 h).
  sim <- paperReviewSimulate(review$drug, review$reference, d, 30 * 1440)
  expect_equal(stats::approx(sim$curve$Time, sim$curve$Cp, 20 * 1440)$y, 1, tolerance = 1e-4)
})

test_that("a tablet repeated at an interval becomes one dose per interval", {
  r <- list(doses = list(list(route = "oral", time = 0, amount = 200, mass_unit = "mg", per_kg = "no",
                              per_time = "none", duration = 3, interval = 1, time_units = "day")))
  d <- paperReviewRegimenDoses(r)$doses
  expect_equal(d$Time, c(0, 1440, 2880))
  expect_equal(d$Units, rep("mg PO", 3))
  expect_true(paperReviewNeedsAbsorption(r))
})

test_that("a fitted ka slower than the terminal elimination is refused", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "oral")
  ex$regimens[[3]] <- oralRegimen()
  # Points that keep rising for a week after one dose: what daily dosing
  # read as a single dose looks like.
  times <- c(1, 2, 4, 7) * 1440
  ex$figures[[3]] <- list(figure_id = "Figure 9", caption = "rising", regimen_id = "R3",
    y_quantity = "concentration", y_units = "ng/mL", time_units = "min", log_y = "no",
    data_kind = "mean", error_bars = "none",
    series = list(list(label = "Oral", points = lapply(seq_along(times), function(i)
      list(time = times[i], value = 10 * i)))),
    digitization_note = "made up", location = "Figure 9")
  a <- paperReviewRun(ex)$absorptions$R3
  expect_false(a$ok)
  expect_match(a$crossCheck, "slower than the terminal elimination, and was not used")
})

test_that("the reference patient's defaults are noted only when a covariate needs them", {
  x <- apparentOneCompartment()
  titles <- vapply(paperReviewCovariateChecks(x), `[[`, "", "title")
  expect_false(any(grepl("No reference or typical", titles)))
  x$covariate_effects <- list(list(parameter_role = "CL", covariate = "weight", form = "power",
    coefficient = 0.75, reference_value = NULL, covariate_units = "kg",
    equation_as_printed = "(WT/?)^0.75", location = "Eq 1"))
  x$population$weight_kg <- list(central = NULL, central_kind = "unstated", min = NULL, max = NULL)
  titles <- vapply(paperReviewCovariateChecks(x), `[[`, "", "title")
  expect_true(any(grepl("No reference or typical weight", titles)))
  expect_false(any(grepl("No reference or typical height", titles)))
})

test_that("printed rounding is read from the printed digits", {
  expect_equal(paperReviewRounding("1.30"), 0.005)
  expect_equal(paperReviewRounding("148"), 0.5)
  expect_equal(paperReviewRounding("2.5e-3"), 0.5e-4)
  expect_equal(paperReviewRounding(""), 0)
})

test_that("every finding has a section, a severity and says whether it was computed", {
  review <- paperReviewRun(paperReviewExample())
  for (f in review$findings) {
    expect_true(f$severity %in% PAPER_REVIEW_SEVERITY_ORDER)
    expect_true(f$section %in% PAPER_REVIEW_SECTIONS)
    expect_type(f$computed, "logical")
  }
  claude <- findingsOf(review, "Claude's reading")
  expect_true(length(claude) > 0 && all(!vapply(claude, `[[`, TRUE, "computed")))
})

test_that("decrement times after a steady-state input are recomputed and checked", {
  # A fictional two-compartment model with an independent eigen() solution,
  # checked against a dense grid rather than a pinned number.
  CL <- 200; V1 <- 1000; V2 <- 10000; Q <- 500                 # L/day
  k10 <- CL / V1; k12 <- Q / V1; k21 <- Q / V2
  l <- sort(-eigen(matrix(c(-(k10 + k12), k21, k12, -k21), 2, byrow = TRUE))$values, decreasing = TRUE)
  A <- c((k21 - l[1]) / (l[2] - l[1]), (k21 - l[2]) / (l[1] - l[2])) / V1
  set <- list(v1 = V1, v2 = V2, cl1 = CL, cl2 = Q)
  t50 <- heldInfusionDecrementTime(set, Inf, 0.5)              # days, in these units
  w <- A / l
  expect_equal(sum(w * exp(-l * t50)) / sum(w), 0.5, tolerance = 1e-9)
  grid <- seq(0, 400, by = 1e-3)
  remaining <- colSums(w * exp(-outer(l, grid))) / sum(w)
  for (fall in c(0.25, 0.5, 0.75))
    expect_lt(abs(heldInfusionDecrementTime(set, Inf, fall) - grid[which.min(abs(remaining - (1 - fall)))]),
              1e-3)                                             # the grid's step

  # Through the review: a printed 50% decrement on the example is checked.
  ex <- paperReviewExample()
  t50 <- heldInfusionDecrementTime(paperReviewRun(ex)$drug$model$PK$default, Inf, 0.5)
  ex$derived_quantities <- c(ex$derived_quantities, list(list(
    quantity = "decrement_50", value = signif(t50, 3), value_text = format(signif(t50, 3)),
    units = "min", regimen_id = NULL, statistic = "typical_value",
    uncertainty = list(type = "not_reported"), location = "Figure 6")))
  t <- paperReviewRun(ex)$consistency
  expect_equal(t$status[t$quantity == "decrement_50"], "matches")
})

test_that("a metabolite's figure series is set aside, not scored against the parent", {
  ex <- paperReviewExample()
  fig <- ex$figures[[1]]
  fig$series[[2]] <- list(label = "Metabolite", analyte = "metabolite",
                          points = list(list(time = 60, value = 5), list(time = 120, value = 9)))
  ex$figures[[1]] <- fig
  review <- paperReviewRun(ex)
  f <- Filter(function(x) identical(x$section, "Figures"), review$findings)[[1]]
  expect_equal(f$severity, "ok")
  expect_match(f$detail, "^9 points compared")
  expect_match(f$detail, "Metabolite, which plots another analyte")
  expect_false("Metabolite" %in% review$figures[[1]]$points$series)
})

test_that("zero-order input simulated as constant input is not called a mismatch", {
  x <- apparentOneCompartment()
  x$structure$absorption <- "zero_order"
  f <- paperReviewAssumptionChecks(x)
  z <- Filter(function(y) grepl("zero order", y$title), f)[[1]]
  expect_equal(z$severity, "info")
  expect_match(z$detail, "exactly as the paper models it")
  # A discrete oral dose of a zero-order paper still gets the warning.
  x$regimens[[2]] <- oralRegimen("D2")
  z <- Filter(function(y) grepl("zero order", y$title), paperReviewAssumptionChecks(x))[[1]]
  expect_equal(z$severity, "concern")
})

test_that("a parameter printed twice with different values is a finding", {
  x <- apparentOneCompartment()
  x$parameters[[3]] <- list(name_in_paper = "CL (abstract)", role = "CL", value = 10.5, value_text = "10.5",
                            units = "L/h", uncertainty = list(type = "not_reported"), status = "estimated",
                            location = "Abstract")
  f <- Filter(function(y) grepl("printed with different values", y$title), paperReviewAssumptionChecks(x))
  expect_length(f, 1)
  expect_match(f[[1]]$detail, "uses the first, 10")
  expect_equal(f[[1]]$locations, c("T2", "Abstract"))
  # The same value printed twice, in other units, is not.
  x$parameters[[3]]$value <- 10 * 1000 / 60; x$parameters[[3]]$units <- "mL/min"
  expect_length(Filter(function(y) grepl("printed with different values", y$title),
                       paperReviewAssumptionChecks(x)), 0)
})

test_that("time axes switch to hours, days or weeks as the plot lengthens", {
  expect_equal(paperReviewTimeAxis(240)$label, "minutes")
  expect_equal(paperReviewTimeAxis(2 * 1440)$label, "hours")
  expect_equal(paperReviewTimeAxis(30 * 1440)$label, "days")
  expect_equal(paperReviewTimeAxis(730 * 1440)$label, "weeks")
})

test_that("a figure of individually dosed subjects is not compared with one regimen", {
  ex <- paperReviewExample()
  ex$figures[[1]]$one_regimen <- "no"
  review <- paperReviewRun(ex)
  f <- Filter(function(x) identical(x$section, "Figures") && grepl("Figure 2", x$title), review$findings)
  expect_length(f, 1)
  expect_equal(f[[1]]$severity, "info")
  expect_match(f[[1]]$detail, "dosed individually")
  expect_length(Filter(function(s) s$figure == "Figure 2",
                       paperReviewFigureSeries(ex, "ng")), 0)
})

test_that("every nullable number in the schema is parsed back to a number", {
  # schemaNullable() sends a nullable number as a string; one missing from
  # PAPER_REVIEW_NUMERIC_FIELDS reached the checks as "3" and was ignored
  # (reading_precision, y_axis_max and x_axis_max were, CodeRabbit on #673).
  nullableNumbers <- function(x) {
    if (!is.list(x)) return(character(0))
    own <- if (is.list(x$properties))
      names(Filter(function(p) is.list(p) && grepl("A plain decimal number", p$description %or% ""),
                   x$properties)) else character(0)
    unique(c(own, unlist(lapply(x, nullableNumbers), use.names = FALSE)))
  }
  found <- nullableNumbers(paperReviewSchema())
  expect_true(all(c("reading_precision", "y_axis_max", "x_axis_max") %in% found))
  expect_true(all(found %in% PAPER_REVIEW_NUMERIC_FIELDS), info = paste(setdiff(found, PAPER_REVIEW_NUMERIC_FIELDS)))
  x <- paperReviewNormalize(list(figures = list(list(reading_precision = "3", y_axis_max = "", x_axis_max = "600"))))
  expect_identical(x$figures[[1]]$reading_precision, 3)
  expect_null(x$figures[[1]]$y_axis_max)
  expect_identical(x$figures[[1]]$x_axis_max, 600)
})

test_that("a missing compartment count leaves the printed quantities unchecked", {
  ex <- paperReviewExample()
  ex$structure$compartments <- "three"
  review <- expect_no_error(paperReviewRun(ex))
  t <- review$consistency
  expect_gt(nrow(t), 0)
  expect_true(all(t$status == "not comparable"))
  titles <- vapply(review$findings, `[[`, "", "title")
  expect_false(any(grepl("(differs|disagrees) with the model", titles)))
})

test_that("a constant oral input's MRT includes its mean input time", {
  # A daily oral dose given as a constant input needs no absorption; the
  # printed MRT carries half the input's duration.
  regimen <- list(id = "R9", doses = list(list(route = "po", per_time = "day", duration = 1440)))
  drug <- list(route = NA_character_)
  expect_equal(paperReviewRegimenMRT(regimen, drug, 100), 100 + 720)
  regimen$doses[[1]]$duration <- 1; regimen$doses[[1]]$time_units <- "day"
  expect_equal(paperReviewRegimenMRT(regimen, drug, 100), 100 + 720)
  # With no duration the input time, and so the MRT, is unknown.
  regimen$doses[[1]]$duration <- NULL
  expect_true(is.na(paperReviewRegimenMRT(regimen, drug, 100)))
})

test_that("values outside a field's closed choices are named, not passed over in silence", {
  x <- paperReviewValidate(paperReviewNormalize(paperReviewExample()))
  expect_false(grepl("not among their field's choices", x$extraction_limitations))
  ex <- paperReviewExample()
  ex$parameters[[1]]$role <- "Vmaxx"
  x <- paperReviewValidate(paperReviewNormalize(ex))
  expect_match(x$extraction_limitations, 'parameters[1].role ("Vmaxx")', fixed = TRUE)
})
