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

test_that("a regimen the model cannot give is not compared, and is not called a disagreement", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "iv_infusion", "oral")
  ex$regimens[[3]] <- oralRegimen()
  ex$derived_quantities <- c(ex$derived_quantities, list(
    derivedRow("AUC_inf", 250, "ng*h/mL", "R3"), derivedRow("Cmax", 99, "ng/mL", "R3"),
    derivedRow("tmax", 2.8, "h", "R3"), derivedRow("MRT", 7.4, "h", "R3")))
  review <- paperReviewRun(ex)
  t <- review$consistency
  oral <- t[t$quantity %in% c("AUC_inf", "Cmax", "tmax", "MRT") & grepl("^(250|99|2.8|7.4)$", t$printed), ]
  expect_equal(nrow(oral), 4)
  expect_true(all(oral$status == "not comparable"))
  expect_true(all(grepl("no oral route", oral$note)))
  titles <- vapply(review$findings, `[[`, "", "title")
  expect_false(any(grepl("^(AUC_inf|Cmax|tmax|MRT) as printed", titles)))
  expect_true(any(grepl("compared agree", titles)))
})

test_that("an oral MRT is the disposition's Vss/CL plus 1/ka plus the lag", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "oral")
  ex$regimens[[3]] <- oralRegimen()
  ex$parameters <- c(ex$parameters, list(
    list(name_in_paper = "ka", role = "ka", route = "oral", value = 0.02, value_text = "0.02",
         units = "1/min", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2"),
    list(name_in_paper = "tlag", role = "tlag", route = "oral", value = 15, value_text = "15",
         units = "min", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2")))
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
  A1 <- 0.1101863309487 * 70 * 1000                     # ng/mL for 70 mg
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

test_that("each figure series is simulated with its own regimen, and one the model cannot give is set aside", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "iv_infusion", "oral")
  ex$regimens[[3]] <- oralRegimen()
  fig <- ex$figures[[1]]
  fig$series[[2]] <- list(label = "Oral", regimen_id = "R3",
                          points = list(list(time = 60, value = 50), list(time = 120, value = 80)))
  ex$figures[[1]] <- fig
  review <- paperReviewRun(ex)
  f <- Filter(function(x) identical(x$section, "Figures"), review$findings)[[1]]
  expect_equal(f$severity, "ok")
  expect_match(f$detail, "^9 points compared")
  expect_match(f$detail, "Oral \\(R3\\): The model has no oral route")
  pts <- review$figures[[1]]$points
  expect_true(all(is.na(pts$predicted[pts$series == "Oral"])))
  expect_true(all(is.finite(pts$predicted[pts$series == "Mean"])))
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
