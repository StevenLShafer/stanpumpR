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
  # With no studied range printed, every legal patient counts as studied.
  ex$population$age_years[c("min", "max")] <- list(NULL, NULL)
  ex$population$weight_kg[c("min", "max")] <- list(NULL, NULL)
  ex$population$height_cm[c("min", "max")] <- list(NULL, NULL)
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

test_that("an equation that fails only outside the population studied is a concern, not an error", {
  # The calibration of 2026-11-10: rivaroxaban's adult model has a power of
  # age on V1, infinite at age 0, which is outside the adults it studied.
  ex <- paperReviewExample()   # studied 25-65 y, 55-110 kg, 158-192 cm
  ex$covariate_effects <- list(
    list(parameter_role = "CL", covariate = "age", form = "power", coefficient = -0.4,
         reference_value = 40, covariate_units = "years", equation_as_printed = "(AGE/40)^-0.4",
         location = "Equation 4"))
  f <- paperReviewCovariateChecks(ex)
  hit <- Filter(function(x) grepl("not finite and positive", x$title), f)
  expect_length(hit, 1)
  expect_equal(hit[[1]]$severity, "concern")
  expect_match(hit[[1]]$detail, "outside the population the paper studied")
  expect_false(any(severities(f) == "error"))
  # A term that fails inside the studied range stays an error.
  ex$covariate_effects[[1]] <- list(parameter_role = "V1", covariate = "age", form = "linear",
    coefficient = -0.05, reference_value = 40, covariate_units = "years",
    equation_as_printed = "1 - 0.05 (AGE - 40)", location = "Equation 5")
  f <- paperReviewCovariateChecks(ex)   # 1 - 0.05 (65 - 40) = -0.25 at 65 y, a studied age
  hit <- Filter(function(x) grepl("not finite and positive", x$title), f)
  expect_equal(hit[[1]]$severity, "error")
  expect_match(hit[[1]]$detail, "within the population the paper studied")
  expect_equal(paperReviewStudied(data.frame(age = c(0, 40), weight = c(70, 70), height = c(170, 170)),
                                  ex$population), c(FALSE, TRUE))
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
  # It has a model, though it prints no volume or clearance (CodeRabbit on #776).
  expect_false(isTRUE(review$noModel))
  html <- as.character(paperReviewReportTags(review, plots = FALSE))
  expect_false(grepl("found no pharmacokinetic model", html))
  expect_match(html, "Internal consistency")
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

# Devin's stress test (2026-11-08): a paediatric paper's Table 3 gave CL/F,
# volumes and half-lives at 10, 20, 30 and 40 kg, and each was compared with
# the 70 kg reference patient, so 18 of 19 "errors" were the paper being right.
# A value printed for a stated patient carries it (`at`), and is compared there.
test_that("a derived quantity printed for a stated patient is compared at that patient", {
  ex <- paperReviewExample()
  cl10 <- list(quantity = "CL", value = round(1.2 * (10 / 70)^0.75, 3),
               value_text = format(round(1.2 * (10 / 70)^0.75, 3)), units = "L/min",
               statistic = "typical_value", location = "Table 3, p. 6")
  v60 <- list(quantity = "V1", value = 7.2, value_text = "7.2", units = "L",
              statistic = "typical_value", location = "Table 3, p. 6")
  row <- function(x, q, loc = "Table 3, p. 6") {
    t <- paperReviewRun(x)$consistency
    t[t$quantity == q & t$location == loc, , drop = FALSE]
  }
  # Without its patient, the 10 kg clearance is a false error against 70 kg.
  bare <- ex; bare$derived_quantities <- c(ex$derived_quantities, list(cl10))
  expect_equal(row(bare, "CL")$status, "disagrees")
  # With it, the model is rebuilt at 10 kg and matches; so does a V1 at 60 years.
  at <- ex
  at$derived_quantities <- c(ex$derived_quantities,
                             list(c(cl10, list(at = list(weight = 10))),
                                  c(v60, list(at = list(age = 60)))))
  r <- row(at, "CL")
  expect_equal(r$status, "matches")
  expect_match(r$note, "Printed for 10 kg")
  expect_equal(row(at, "V1")$status, "matches")
  # The reference patient's own values in `at` are the reference.
  same <- ex
  same$derived_quantities[[4]]$at <- list(weight = 70, age = 40)
  expect_equal(row(same, "Vss", same$derived_quantities[[4]]$location)$status, "matches")
  expect_false(grepl("Printed for", row(same, "Vss", same$derived_quantities[[4]]$location)$note))
  # A value for another patient never closes the model at the reference.
  only <- ex
  only$derived_quantities <- list(c(cl10, list(at = list(weight = 10))))
  expect_length(paperReviewDerived(only, "CL", 70), 0)
  expect_length(paperReviewDerived(bare, "CL", 70), 1)
  # And the schema asks for it, so a reply can carry it through validation.
  v <- paperReviewValidate(paperReviewNormalize(at))
  expect_equal(v$derived_quantities[[length(v$derived_quantities) - 1]]$at$weight, 10)
})

test_that("a value printed under conditions is not compared; as a mean, or for one individual, a concern", {
  # The calibration of 2026-11-10 (docs/paper-review-calibration.md) found
  # each of these reported as an error on papers whose models are verified.
  ex <- paperReviewExample()
  find <- function(x) Filter(function(f) grepl("^Vss as printed \\(74", f$title), paperReviewRun(x)$findings)
  wrong <- ex$derived_quantities[[4]]          # Vss 148 L
  wrong$value <- 74; wrong$value_text <- "74"; wrong$location <- "Text, p. 9"
  x <- ex; x$derived_quantities <- c(ex$derived_quantities, list(wrong))
  n <- length(x$derived_quantities)
  expect_equal(find(x)[[1]]$severity, "error")
  # Printed under conditions the model does not carry: not compared at all
  # (Shafer's test of 2026-11-17, where a concern for each was still a false
  # finding), the model's value shown in the table beside the paper's.
  x$derived_quantities[[n]]$conditions <- "during cardiopulmonary bypass"
  expect_length(find(x), 0)
  t <- paperReviewRun(x)$consistency
  r <- t[t$quantity == "Vss" & t$location == "Text, p. 9", ]
  expect_equal(r$status, "not comparable")
  expect_match(r$note, "cardiopulmonary bypass")
  expect_true(nzchar(r$model))
  # A mean of individuals' values.
  x$derived_quantities[[n]]$conditions <- ""
  x$derived_quantities[[n]]$statistic <- "mean_of_individuals"
  f <- find(x)[[1]]
  expect_equal(f$severity, "concern")
  expect_match(f$detail, "mean of individuals' values")
  # One individual's, beside the typical value, from a model with no
  # covariate that separates them (hydralazine's subjects, atropine's).
  y <- ex
  y$covariate_effects <- list()
  ind <- wrong; ind$at <- list(age = 31, sex = "female")
  y$derived_quantities <- c(ex$derived_quantities, list(ind))
  f <- find(y)[[1]]
  expect_equal(f$severity, "concern")
  expect_match(f$detail, "one individual's own")
  # Alone, a value for a stated patient is that patient's, and still an error.
  z <- y
  z$derived_quantities <- c(ex$derived_quantities[-4], list(ind))
  expect_equal(find(z)[[1]]$severity, "error")
})

test_that("a decrement time printed after a stated infusion is compared at that infusion", {
  # Bailey 1994's context-sensitive half-times of milrinone after 30 to
  # 600 min were compared with the steady-state value (+178% to +793%).
  expect_equal(paperReviewInfusionMinutes(list(infusion_minutes = 45)), 45)
  expect_equal(paperReviewInfusionMinutes(list(value_text = "43", location = "p. 621 (45-min infusion)")), 45)
  expect_equal(paperReviewInfusionMinutes(list(value_text = "61 min after a 3 h infusion")), 180)
  expect_equal(paperReviewInfusionMinutes(list(value_text = "43", location = "Table 3")), Inf)
  ex <- paperReviewExample()
  dq <- list(quantity = "decrement_50", value = 10, value_text = "10", units = "min",
             statistic = "typical_value", location = "Discussion, p. 7")
  rowOf <- function(d) {
    x <- ex; x$derived_quantities <- c(ex$derived_quantities, list(d))
    t <- paperReviewRun(x)$consistency
    t[t$quantity == "decrement_50", , drop = FALSE]
  }
  steady <- rowOf(dq)
  after60 <- rowOf(c(dq, list(infusion_minutes = 60)))
  expect_match(after60$note, "After a 60 min infusion")
  expect_false(identical(steady$model, after60$model))
  expect_lt(as.numeric(after60$model), as.numeric(steady$model))
})

test_that("one group spelled two ways is one group", {
  expect_identical(paperReviewGroupKey("R(−)-methadone"), paperReviewGroupKey(" r(-)-Methadone "))
  expect_identical(paperReviewGroupKey("children 7–11 y (NCA)"), paperReviewGroupKey("Children 7-11  y (NCA)"))
  expect_false(identical(paperReviewGroupKey("R-methadone"), paperReviewGroupKey("S-methadone")))
  x <- list(parameters = list(list(role = "CL", group = "R(−)-methadone"),
                              list(role = "V1", group = "R(-)-methadone")),
            derived_quantities = list(list(quantity = "t_half_terminal", group = "r(-)-methadone")),
            figures = list(list(series = list(list(label = "a", group = "R(-)-Methadone")))))
  y <- paperReviewCanonicalGroups(x)
  expect_length(paperReviewGroups(y), 1)
  expect_identical(y$figures[[1]]$series[[1]]$group, "R(−)-methadone")
})

test_that("a group that is not a disposition model is not reported as failing to close one", {
  ex <- list(parameters = list(list(role = "CL", group = "PK"), list(role = "V1", group = "PK"),
                               list(role = "ke0", group = "Heart rate"),
                               list(role = "other", group = "Heart rate"),
                               list(role = "other", group = "Children (NCA)")))
  closeError <- paperReviewFinding("Identifiability", "error",
                                   "The paper does not close the 2-compartment model", "", "Table 2")
  ke0Error <- paperReviewFinding("Identifiability", "error",
                                 "The paper describes an effect site but prints no ke0", "", "Methods")
  f <- paperReviewGroupFindings(list(closeError, ke0Error), "Children (NCA)", ex)
  expect_equal(vapply(f, `[[`, "", "severity"), c("info", "info"))
  expect_equal(f[[1]]$title, "No disposition model is reported for this group")
  expect_match(f[[2]]$detail, "Heart rate")
  # A group with disposition parameters of its own keeps the error, and the
  # group that owns the ke0 is not excused from printing it.
  f <- paperReviewGroupFindings(list(closeError, ke0Error), "PK", ex)
  expect_equal(f[[1]]$severity, "error")
  expect_equal(f[[2]]$severity, "info")
  f <- paperReviewGroupFindings(list(ke0Error), "Heart rate", ex)
  expect_equal(f[[1]]$severity, "error")
})

test_that("a value for a stated patient is not compared through a covariate the review could not apply", {
  # Shafer's test of 2026-11-17 (BJA-D-26-02959): ke0 = 0.221 x (1 - 0.00979
  # (AGE - 54)) x (1 + 0.231 SEX), SEX 1 for men.  Held at its reference, it
  # made every ke0 printed at 20 and 75 years "disagree" with the paper's own
  # parameters, which reproduce them exactly.  Here the reference is 40 years.
  ex <- paperReviewExample()
  ex$structure$effect_site <- "ke0"
  ex$parameters <- c(Filter(function(p) p$role != "tPeak", ex$parameters),
                     list(list(role = "ke0", value = 0.221, value_text = "0.221", units = "1/min",
                               status = "estimated", location = "Table 3")))
  ke0At <- function(age, sex, value)
    list(quantity = "ke0", value = value, value_text = format(value), units = "1/min",
         statistic = "typical_value", at = list(age = age, sex = sex),
         location = paste("Text,", age, "y", sex))
  printed <- list(ke0At(20, "female", 0.264), ke0At(75, "female", 0.145))
  ex$derived_quantities <- printed
  rows <- function(x) { t <- paperReviewRun(x)$consistency; t[t$quantity == "ke0", ] }
  bad <- function(x) Filter(function(f) grepl("^ke0 as printed", f$title), paperReviewRun(x)$findings)

  # The equation as Claude could not read it: one effect of form 'other'.
  other <- ex
  other$covariate_effects <- c(ex$covariate_effects, list(list(
    parameter_role = "ke0", covariate = "age", form = "other", coefficient = NULL,
    reference_value = 40, covariate_units = "years",
    equation_as_printed = "ke0 = 0.221 x (1 - 0.00979 (AGE - 40)) x (1 + 0.231 SEX)",
    location = "Table 3")))
  r <- rows(other)
  expect_equal(r$status, c("not comparable", "not comparable"))
  expect_match(r$note[1], "held at its reference")
  expect_length(bad(other), 0)

  # As the prompt now asks: one effect per factor, sex as sex_male.  The
  # model then gives the paper's values: 0.221 x 1.1958 = 0.264 at 20 years,
  # 0.221 x 0.6574 = 0.145 at 75, and 0.272 for the reference man.
  factors <- ex
  factors$covariate_effects <- c(ex$covariate_effects, list(
    list(parameter_role = "ke0", covariate = "age", form = "linear", coefficient = -0.00979,
         reference_value = 40, covariate_units = "years", equation_as_printed = "", location = "Table 3"),
    list(parameter_role = "ke0", covariate = "sex_male", form = "linear", coefficient = 0.231,
         reference_value = 0, covariate_units = "", equation_as_printed = "", location = "Table 3")))
  r <- rows(factors)
  expect_equal(r$status, c("matches", "matches"))
  expect_equal(as.numeric(r$model), c(0.221 * (1 + 0.00979 * 20), 0.221 * (1 - 0.00979 * 35)),
               tolerance = 1e-3)
  ref <- paperReviewReferencePatient(factors)
  d <- paperReviewDrug(factors, ref)
  expect_equal(d$model$ke0, 0.221 * 1.231, tolerance = 1e-9)

  # A figure of patients the reference may not be is no finding while such an
  # effect is held (the ICU factor of AA-D-26-00064, +73%).
  fig <- paperReviewExample()
  fig$covariate_effects <- c(fig$covariate_effects, list(list(
    parameter_role = "CL", covariate = "other", form = "category_multiplier", coefficient = 0.458,
    reference_value = NULL, covariate_units = "", equation_as_printed = "CL x 0.458 in ICU patients",
    location = "Table 2")))
  for (i in seq_along(fig$figures))
    for (j in seq_along(fig$figures[[i]]$series))
      fig$figures[[i]]$series[[j]]$points <- lapply(fig$figures[[i]]$series[[j]]$points,
        function(p) { p$value <- p$value * 3; p })
  ff <- Filter(function(f) f$section == "Figures" && grepl("digitised points", f$title),
               paperReviewRun(fig)$findings)
  expect_true(length(ff) > 0)
  expect_true(all(vapply(ff, function(f) f$severity, "") == "info"))
  expect_match(ff[[1]]$title, "cannot be compared")
  expect_match(ff[[1]]$detail, "ICU")
})

test_that("a paper with no pharmacokinetic model gets no model's findings", {
  # Shafer's test of 2026-11-17: a response surface of doses (ALN-D-25-00690)
  # was reported three times over as "the number of compartments is not 1, 2
  # or 3" and "elimination is not linear"; a regression on awakening time
  # (BJA-D-26-00577) as failing to close a one-compartment model.
  surface <- list(
    paper = list(title = "A response surface"),
    drug = list(name = "remimazolam and sufentanil", dose_basis = "unclear",
                dose_basis_note = "Salt not stated.", concentration_units = "other", location = "p. 5"),
    structure = list(compartments = 0, elimination = "other", absorption = "other",
                     effect_site = "pd_without_effect_site", location = "Equations 1-6"),
    parameters = lapply(c("B0", "B1", "B2", "B3"), function(b)
      list(name_in_paper = b, group = "Sedation", role = "other", value = 1, value_text = "1",
           units = "unitless", location = "Table 3")),
    reviewer_notes = list(list(severity = "error", topic = "Thresholds inconsistent with Table 3",
                               finding = "0.25, not 0.54", location = "Table 3")),
    published_models = list(), extraction_limitations = "")
  expect_true(paperReviewNoModel(surface))
  review <- paperReviewRun(surface)
  expect_true(review$noModel)
  titles <- vapply(review$findings, function(f) f$title, "")
  expect_setequal(titles, c("The dose basis is not stated", "Thresholds inconsistent with Table 3"))
  html <- as.character(paperReviewReportTags(review, plots = FALSE))
  expect_match(html, "found no pharmacokinetic model")
  expect_match(html, "What Claude extracted")
  expect_match(html, "1 error, 1 concern")
  expect_false(grepl("not 1, 2 or 3|does not close|Elimination is not linear", html))
  # A lone rate constant is not a model; a noncompartmental CL is.
  rate <- surface
  rate$parameters <- list(list(role = "k10", value = 0.045, value_text = "0.045", units = "1/min"))
  expect_true(paperReviewNoModel(rate))
  nca <- surface
  nca$derived_quantities <- list(list(quantity = "CL", value = 22.8, units = "mL/min/kg"))
  expect_false(paperReviewNoModel(nca))
  # A paper that uses a published model has one.
  pub <- surface
  pub$published_models <- list(list(name = "Eleveld 2018", drug = "propofol"))
  expect_false(paperReviewNoModel(pub))
})

test_that("only saturable elimination is reported as not linear", {
  ex <- paperReviewExample()
  title <- function(x) {
    f <- Filter(function(f) grepl("^Elimination", f$title), paperReviewRun(x)$findings)
    if (length(f)) paste(f[[1]]$severity, f[[1]]$title) else ""
  }
  expect_equal(title(ex), "")
  ex$structure$elimination <- "michaelis_menten"
  expect_equal(title(ex), "concern Elimination is not linear")
  ex$structure$elimination <- "other"
  expect_equal(title(ex), "info Elimination is not described as linear")
})

test_that("a note every model shares is printed once, and a covariate stanpumpR cannot compute is said once", {
  # Shafer's test of 2026-11-17 (BJA-D-26-02959): eighteen "not finite and
  # positive ... NA at age 0 y, 40 kg, 50 cm" concerns, one per LBM term per
  # model, from James's lean body mass, which is negative there.
  ex <- paperReviewExample()
  lbm <- function(role) list(parameter_role = role, covariate = "lean_body_mass", form = "power",
                             coefficient = 0.384, reference_value = 48, covariate_units = "kg",
                             equation_as_printed = paste(role, "x (LBM/48)^0.384"), location = "Table 2")
  ex$covariate_effects <- lapply(c("V1", "V2", "V3"), lbm)
  one <- paperReviewCovariateChecks(ex)
  titles <- vapply(one, function(f) f$title, "")
  expect_false(any(grepl("not finite and positive", titles)))
  expect_equal(sum(grepl("is not defined for every legal patient", titles)), 1)
  # Three models sharing those effects: each note once in all.
  groups <- c("TCI PD", "Final PD", "LOC")
  three <- ex
  three$parameters <- unlist(lapply(groups, function(g)
    lapply(ex$parameters, function(p) { p$group <- g; p })), recursive = FALSE)
  three$derived_quantities <- list()
  review <- paperReviewRun(three)
  cov <- Filter(function(f) f$section == "Covariates", review$findings)
  keys <- vapply(cov, function(f) paste(f$title, sub(" The same holds for.*", "", f$detail)), "")
  expect_false(any(duplicated(keys)))
  undefinedNote <- Filter(function(f) grepl("not defined for every legal patient", f$title), cov)
  expect_length(undefinedNote, 1)
  expect_match(undefinedNote[[1]]$detail, "The same holds for Final PD, LOC")
})

test_that("a parameter printed for men and for women is a sex effect, not a conflict", {
  # Shafer's test of 2026-11-17 (BJA-D-25-00628): the pooled model's Q, 0.66
  # l/kg/min in males and 0.56 in females, was "printed with different
  # values" and the sex difference was dropped from the simulator.
  ex <- paperReviewExample()
  q2 <- which(vapply(ex$parameters, function(p) p$role, "") == "Q2")
  male <- ex$parameters[[q2]]; male$name_in_paper <- "Q (male)"; male$value <- 2; male$value_text <- "2"
  female <- male; female$name_in_paper <- "Q (female)"; female$value <- 1.7; female$value_text <- "1.7"
  ex$parameters <- c(ex$parameters[-q2], list(female, male))
  review <- paperReviewRun(ex)
  titles <- vapply(review$findings, function(f) f$title, "")
  expect_false(any(grepl("printed with different values", titles)))
  expect_match(review$extraction$extraction_limitations, "Q2 is printed for men \\(2\\) and women \\(1.7\\)")
  # The man's value is the typical value, and a woman's is 0.85 of it.
  man <- list(weight = 70, height = 174, age = 40, sex = "male")
  woman <- modifyList(man, list(sex = "female"))
  x <- review$extraction
  expect_equal(paperReviewDisposition(x, man)$cl2, 2, tolerance = 1e-9)
  expect_equal(paperReviewDisposition(x, woman)$cl2, 1.7, tolerance = 1e-9)
  # Two values of a parameter that already has a covariate effect are its
  # values at different covariates, said as a note.
  cov <- paperReviewExample()
  cl <- which(vapply(cov$parameters, function(p) p$role, "") == "CL")
  other <- cov$parameters[[cl]]; other$value <- 0.6; other$value_text <- "0.6"; other$name_in_paper <- "CL at 35 kg"
  cov$parameters <- c(cov$parameters, list(other))
  f <- Filter(function(f) grepl("^CL is printed", f$title), paperReviewAssumptionChecks(cov))
  expect_equal(f[[1]]$severity, "info")
  expect_equal(f[[1]]$title, "CL is printed for different patients")
})

test_that("a published model used as published is pointed to the library, not rebuilt from a figure", {
  # Shafer's test of 2026-11-17 (BJA-D-26-00728): Eleveld's and Schnider's
  # propofol models, used to simulate effect-site concentrations, were
  # "closed" by fitting exponentials to a figure (V1 45.6 L and 14.9 L) and
  # then reported as not reproducing it.
  ex <- paperReviewExample()
  ex$parameters <- list()
  ex$derived_quantities <- list()
  ex$covariate_effects <- list()
  ex$drug$name <- "propofol"
  ex$published_models <- list(
    list(name = "Eleveld 2018", drug = "propofol", group = "Eleveld", location = "Methods, p. 6"),
    list(name = "Schnider 1998", drug = "Propofol", group = "Schnider", location = "Methods, p. 7"))
  for (i in seq_along(ex$figures))
    ex$figures[[i]]$series <- c(lapply(ex$figures[[i]]$series, function(s) { s$group <- "Eleveld"; s }),
                                lapply(ex$figures[[i]]$series, function(s) { s$group <- "Schnider"; s }))
  expect_false(paperReviewNoModel(ex))
  review <- paperReviewRun(ex)
  expect_setequal(names(review$models), c("Eleveld", "Schnider"))
  for (g in names(review$models)) {
    m <- review$models[[g]]
    expect_equal(m$identifiability$verdict, "published")
    titles <- vapply(m$findings, function(f) f$title, "")
    expect_false(any(grepl("does not close|Closed instead|not 1, 2 or 3|prints no ke0|complete, positive", titles)))
    expect_false(any(vapply(m$findings, function(f) f$severity == "error", logical(1))))
  }
  pub <- function(g) Filter(function(f) grepl("^A published model", f$title), review$models[[g]]$findings)[[1]]
  expect_match(pub("Eleveld")$detail, "holds it as propofol \\(Eleveld")
  expect_match(pub("Schnider")$detail, "holds it as propofol \\(Schnider\\)")
  # A paper with its own model that only compares with a published one
  # (no group named) keeps its own model's checks.
  own <- paperReviewExample()
  own$published_models <- list(list(name = "Eleveld 2018", drug = "propofol"))
  expect_equal(paperReviewRun(own)$identifiability$verdict, "printed")
})

test_that("every model of one paper prints its parameters in one time unit", {
  # Shafer's test of 2026-11-17 (ALN-D-25-01106): the on-ECMO model in L/h,
  # the post-ECMO model in L/min.
  ex <- paperReviewExample()
  slow <- function(p) { if (p$role %in% c("CL", "Q2", "Q3")) { p$value <- p$value / 20 }; p }
  ex$parameters <- c(lapply(ex$parameters, function(p) { p$group <- "fast"; p }),
                     lapply(ex$parameters, function(p) { p <- slow(p); p$group <- "slow"; p }))
  ex$derived_quantities <- list()
  review <- paperReviewRun(ex)
  units <- lapply(review$models, function(m) unique(m$pkTable$Units[grepl("^CL", m$pkTable$Parameter)]))
  expect_equal(units$fast, units$slow)
  expect_equal(unname(units$fast), "L/h")
})


test_that("CodeRabbit on #776: grouped published models alone, same-named entries, roles, normalized figures", {
  # A paper whose only models are published ones, each grouped, with nothing
  # else grouped: each is recognised as published, never closed as its own.
  ex <- paperReviewExample()
  ex$parameters <- list(); ex$derived_quantities <- list(); ex$covariate_effects <- list()
  ex$published_models <- list(list(name = "Eleveld 2018", drug = "propofol", group = "Eleveld"))
  r <- paperReviewRun(ex)
  expect_equal(r$identifiability$verdict, "published")
  expect_false(any(vapply(r$findings, function(f) f$severity == "error", logical(1))))
  ex$published_models <- c(ex$published_models,
                           list(list(name = "Schnider 1998", drug = "propofol", group = "Schnider")))
  r <- paperReviewRun(ex)
  expect_setequal(names(r$models), c("Eleveld", "Schnider"))
  expect_true(all(vapply(r$models, function(m) m$identifiability$verdict == "published", logical(1))))

  # Two entries named alike conflict, whatever covariates the parameter has.
  cov <- paperReviewExample()
  cl <- which(vapply(cov$parameters, function(p) p$role, "") == "CL")
  dup <- cov$parameters[[cl]]; dup$value <- 1.26; dup$value_text <- "1.26"
  cov$parameters <- c(cov$parameters, list(dup))
  f <- Filter(function(f) grepl("^CL is printed", f$title), paperReviewAssumptionChecks(cov))
  expect_equal(f[[1]]$title, "CL is printed with different values")

  # A printed CL depends on CL alone; a derived one on every disposition role.
  ex <- paperReviewExample()
  expect_equal(paperReviewQuantityRoles("CL", ex), "CL")
  expect_true(all(c("V1", "CL") %in% paperReviewQuantityRoles("Vss", ex)))
  noCL <- ex; noCL$parameters <- Filter(function(p) p$role != "CL", ex$parameters)
  expect_true("V1" %in% paperReviewQuantityRoles("CL", noCL))
  # So an unapplied effect on V1 does not hide a misprinted CL for a stated patient.
  v1 <- ex
  v1$covariate_effects <- list(list(parameter_role = "V1", covariate = "age", form = "other",
                                    equation_as_printed = "V1 x f(AGE)", location = "Eq 3"))
  v1$derived_quantities <- list(list(quantity = "CL", value = 3, value_text = "3", units = "L/min",
                                     statistic = "typical_value", at = list(age = 70), location = "Text"))
  t <- paperReviewRun(v1)$consistency
  expect_equal(t$status[t$quantity == "CL"], "disagrees")

  # A peak-normalized figure is not scored while a shape-changing effect is held.
  norm <- paperReviewExample()
  norm$figures[[1]]$y_normalization <- "percent_of_peak"
  norm$figures[[1]]$y_units <- "other"
  mx <- max(vapply(norm$figures[[1]]$series[[1]]$points, function(p) p$value, 0))
  norm$figures[[1]]$series[[1]]$points <- lapply(norm$figures[[1]]$series[[1]]$points, function(p) {
    p$value <- 100 * (p$value / mx)^0.3; p })
  score <- function(x) Filter(function(f) grepl("^Figure 2", f$title), paperReviewRun(x)$findings)[[1]]
  base <- score(norm)
  expect_true(base$severity %in% c("concern", "error"))
  norm$covariate_effects <- c(norm$covariate_effects, list(list(
    parameter_role = "CL", covariate = "other", form = "category_multiplier", coefficient = 0.5,
    covariate_units = "", equation_as_printed = "CL x 0.5 in ICU", location = "Table 2")))
  held <- score(norm)
  expect_equal(held$severity, "info")
  expect_match(held$detail, "ICU")
})


test_that("CodeRabbit's second look at #776: derived own models, intravenous figures, group spelling", {
  # A paper whose own model is closed from derived quantities keeps its own
  # model's checks beside an ungrouped published comparator.
  ex <- paperReviewExample()
  ex$published_models <- list(list(name = "Eleveld 2018", drug = "propofol"))
  ex$parameters <- Filter(function(p) p$role == "tPeak", ex$parameters)
  expect_true(any(vapply(ex$derived_quantities, function(d) d$quantity %in% c("CL", "Vss"), logical(1))))
  expect_length(paperReviewPublishedModels(ex), 0)
  expect_false(identical(paperReviewRun(ex)$identifiability$verdict, "published"))

  # An intravenous figure does not depend on absorption; an oral one does.
  ex <- paperReviewExample()
  iv <- paperReviewFigureRoles(ex$figures[[1]], ex)
  expect_false(any(c("ka", "F", "tlag") %in% iv))
  oral <- ex
  oral$regimens[[1]]$doses[[1]]$route <- "oral"
  rid <- oral$regimens[[1]]$id
  oral$figures[[1]]$regimen_id <- rid
  expect_true(all(c("ka", "F", "tlag") %in% paperReviewFigureRoles(oral$figures[[1]], oral)))
  expect_false("F" %in% paperReviewFigureRoles(oral$figures[[1]], oral, normalized = TRUE))

  # One group spelled two ways on a figure and a published model is one model.
  ex <- paperReviewExample()
  ex$parameters <- list(); ex$derived_quantities <- list(); ex$covariate_effects <- list()
  ex$published_models <- list(list(name = "Schnider 1998", drug = "propofol", group = "Schnider"),
                              list(name = "Eleveld 2018", drug = "propofol", group = "Eleveld"))
  for (i in seq_along(ex$figures))
    ex$figures[[i]]$series <- lapply(ex$figures[[i]]$series, function(s) { s$group <- "schnider"; s })
  expect_setequal(names(paperReviewRun(ex)$models), c("Schnider", "Eleveld"))
})


test_that("CodeRabbit's third look at #776: names that differ only by source, effect-site figures", {
  # A weight effect on CL does not make "CL" and "CL (abstract)" two patients'.
  cov <- paperReviewExample()
  cl <- which(vapply(cov$parameters, function(p) p$role, "") == "CL")
  other <- cov$parameters[[cl]]; other$value <- 1.26; other$value_text <- "1.26"
  other$name_in_paper <- "CL (abstract)"
  cov$parameters <- c(cov$parameters, list(other))
  title <- function(x) Filter(function(f) grepl("^CL is printed", f$title), paperReviewAssumptionChecks(x))[[1]]$title
  expect_equal(title(cov), "CL is printed with different values")
  # Named for a stated patient, they are that patient's.
  cov$parameters[[length(cov$parameters)]]$name_in_paper <- "CL at 35 kg"
  expect_equal(title(cov), "CL is printed for different patients")
  # An effect-site figure depends on ke0 and tPeak.
  ex <- paperReviewExample()
  f <- ex$figures[[1]]
  expect_false("ke0" %in% paperReviewFigureRoles(f, ex))
  f$concentration_site <- "effect_site"
  expect_true(all(c("ke0", "tPeak") %in% paperReviewFigureRoles(f, ex, normalized = TRUE)))
})

test_that("CodeRabbit's later looks at #776: the follow-up items", {
  ex <- paperReviewExample()
  # A CL derived as k10 V1 depends on k10 and V1 alone, not on k12.
  micro <- ex
  micro$parameters <- list(
    list(role = "V1", value = 8, value_text = "8", units = "L"),
    list(role = "k10", value = 0.15, value_text = "0.15", units = "1/min"))
  expect_setequal(paperReviewQuantityRoles("CL", micro), c("k10", "V1"))
  expect_false("k12" %in% paperReviewQuantityRoles("CL", micro))

  # A coefficient given as a fraction of the sum is no model of the paper's own.
  frac <- list(derived_quantities = list(list(quantity = "coefficient_1", value = 80, units = "percent")))
  expect_false(paperReviewOwnModelEvidence(frac))
  abs <- list(derived_quantities = list(list(quantity = "coefficient_1", value = 0.1, units = "1/L")))
  expect_true(paperReviewOwnModelEvidence(abs))
  expect_true(paperReviewNoModel(frac))

  # A metabolite series that is set aside brings no absorption into the figure.
  met <- ex
  met$regimens <- c(met$regimens, list(list(id = "RO", doses = list(list(route = "oral", time = 0,
    amount = 10, mass_unit = "mg", per_kg = "no", per_time = "none", time_units = "min")))))
  f <- met$figures[[1]]
  f$series <- c(f$series, list(list(label = "metabolite", analyte = "metabolite", regimen_id = "RO",
                                     points = list())))
  expect_false(any(c("ka", "F") %in% paperReviewFigureRoles(f, met)))
  # Nor does a parent series with no points, which the check does not score.
  f <- met$figures[[1]]
  f$series <- c(f$series, list(list(label = "oral, not digitised", analyte = "parent", regimen_id = "RO",
                                     points = list())))
  expect_false(any(c("ka", "tlag", "F") %in% paperReviewFigureRoles(f, met)))
  f$series[[length(f$series)]]$points <- list(list(time = 30, value = 1))
  expect_true(all(c("ka", "tlag", "F") %in% paperReviewFigureRoles(f, met)))

  # A normalized figure of a regimen mixing IV and oral doses keeps F.
  mix <- ex
  mix$regimens[[1]]$doses <- c(mix$regimens[[1]]$doses, list(list(route = "oral", time = 60,
    amount = 10, mass_unit = "mg", per_kg = "no", per_time = "none", time_units = "min")))
  expect_true("F" %in% paperReviewFigureRoles(mix$figures[[1]], mix, normalized = TRUE))
  oral <- ex
  oral$regimens[[1]]$doses[[1]]$route <- "oral"
  expect_false("F" %in% paperReviewFigureRoles(oral$figures[[1]], oral, normalized = TRUE))

  # Two names giving one patient are a conflict.
  cov <- paperReviewExample()
  cl <- which(vapply(cov$parameters, function(p) p$role, "") == "CL")
  a <- cov$parameters[[cl]]; a$name_in_paper <- "CL at 35 kg"
  b <- a; b$value <- 1.26; b$value_text <- "1.26"; b$name_in_paper <- "CL at 35 kg (abstract)"
  cov$parameters <- c(cov$parameters[-cl], list(a, b))
  t <- Filter(function(f) grepl("^CL is printed", f$title), paperReviewAssumptionChecks(cov))[[1]]$title
  expect_equal(t, "CL is printed with different values")

  # A decrement time after an infusion written in the conditions is compared.
  dq <- list(quantity = "decrement_50", value = 10, value_text = "10", units = "min",
             statistic = "typical_value", conditions = "after a 60-min infusion", location = "Text")
  x <- ex; x$derived_quantities <- c(ex$derived_quantities, list(dq))
  r <- paperReviewRun(x)$consistency
  row <- r[r$quantity == "decrement_50", ]
  expect_equal(nrow(row), 1)
  expect_false(identical(row$status, "not comparable"))
  expect_match(row$note, "After a 60 min infusion")
  infused <- paperReviewRun(modifyList(x, list(derived_quantities = c(ex$derived_quantities,
    list(modifyList(dq, list(conditions = "", infusion_minutes = 60)))))))$consistency
  expect_equal(row$model, infused$model[infused$quantity == "decrement_50"])
  # Another quantity's infusion condition is not carried, so it stays not comparable.
  tp <- list(quantity = "tPeak", value = 3, value_text = "3", units = "min",
             statistic = "typical_value", conditions = "after a 60-min infusion", location = "Text")
  y <- ex; y$derived_quantities <- c(ex$derived_quantities, list(tp))
  r <- paperReviewRun(y)$consistency
  expect_equal(r$status[r$quantity == "tPeak" & r$location == "Text"], "not comparable")
  dq$conditions <- "after a 60-min infusion during cardiopulmonary bypass"
  x <- ex; x$derived_quantities <- c(ex$derived_quantities, list(dq))
  r <- paperReviewRun(x)$consistency
  expect_equal(r$status[r$quantity == "decrement_50"], "not comparable")

  # A decrement-time figure's miss is a note while an effect is held.
  held <- paperReviewHeldFigure(list(severity = "error", detail = "miss"),
    list(covariate_effects = list(list(parameter_role = "CL", covariate = "other",
      form = "category_multiplier", coefficient = 0.5, equation_as_printed = "CL x 0.5 in ICU"))),
    paperReviewReferencePatient(ex), PAPER_REVIEW_DISPOSITION_ROLES)
  expect_equal(held$severity, "info")
  expect_match(held$detail, "ICU")
})
