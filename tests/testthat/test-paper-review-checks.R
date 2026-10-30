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
  # The closed model reproduces the half-lives it was built from.
  t <- well$consistency
  expect_true(all(t$status[t$quantity %in% c("t_half_1", "t_half_2", "Vss", "CL")] == "matches"))
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
