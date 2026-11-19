# Single Paper Review of a paper with front-end kinetics
# (R/paper-review-front-end.R): the extraction's front_end block becomes a
# model of the front-end engine, which is checked against the paper's
# arterial and venous figures.  The extraction below is what a faithful
# reading of Henthorn 2018 (Anesthesiology 129:260-70) gives; its figure
# points are those digitised from Figure 6 for test-front-end-kinetics.R.
# Claude Code, 2026-11-19.

henthornQuantity <- function(value, units) list(value = value, value_text = format(value), units = units)

henthornParameter <- function(role, value, units)
  list(name_in_paper = role, role = role, value = value, value_text = format(value), units = units,
       uncertainty = list(type = "unstated"), status = "estimated", location = "Table 2, p. 265")

henthornExtraction <- function()
{
  ex <- paperReviewExample()
  ex$drug$name <- "S(+)-ketamine"
  ex$drug$concentration_units <- "ng/mL"
  ex$structure$compartments <- 3
  ex$structure$routes <- list("iv_infusion")
  ex$structure$effect_site <- "none"
  ex$covariate_effects <- list()
  ex$derived_quantities <- list()
  ex$parameters <- list(
    henthornParameter("V1", 44.93, "L"), henthornParameter("V2", 136.58, "L"),
    henthornParameter("V3", 377.97, "L"), henthornParameter("CL", 1.23, "L/min"),
    henthornParameter("Q2", 3.17, "L/min"), henthornParameter("Q3", 0.65, "L/min"))
  ex$regimens <- list(list(id = "R1", description = "7 mg S(+)-ketamine hydrochloride over 30 min",
    doses = list(list(route = "iv_infusion", time = 0, amount = 7, analyte_fraction = 0.867,
                      mass_unit = "mg", per_kg = "no", per_time = "none", duration = 30,
                      time_units = "min")), location = "Figure 6"))
  fig <- utils::read.csv(test_path("fixtures", "front-end", "henthorn2018-figure6.csv"))
  series <- function(name, site)
    list(label = name, analyte = "parent", sampling_site = site,
         points = lapply(which(fig$series == name), function(i) list(time = fig$time[i], value = fig$conc[i])))
  ex$figures <- list(list(figure_id = "Figure 6", caption = "Final model simulation", y_quantity = "concentration",
    y_units = "ng/mL", time_units = "min", log_y = "yes", one_regimen = "yes", y_normalization = "none",
    data_kind = "model_prediction", regimen_id = "R1", digitization_note = "read from the curves",
    series = list(series("arterial", "arterial"), series("venous", "venous")), location = "Figure 6, p. 269"))
  ex$front_end <- list(
    model_type = "mixing", description = "Unmixed compartment cleared by total pharmacokinetic flow",
    sampling_site = "arterial",
    total_flow = henthornQuantity(7.59, "L/min"), unmixed_volume = henthornQuantity(1, "L"),
    arm = list(present = "yes", rate_constant = henthornQuantity(0.18, "1/min"), exchanging_fraction = 0.04,
               partition_rule = "v2_over_v1", driver = "central", location = "Equation 4"),
    location = "Figure 1 and Table 2, p. 263-265")
  ex
}

test_that("a mixing model is built from the extraction as the paper prints it", {
  ex <- henthornExtraction()
  built <- paperReviewFrontEndSpec(ex, paperReviewReferencePatient(ex))
  ref <- frontEndHenthorn2018Ketamine("S")
  for (f in c("type", "unmixed_volume", "flow", "v1", "v2", "v3", "cl1", "cl2", "cl3"))
    expect_equal(built$spec[[f]], ref[[f]], info = f)
  expect_equal(built$spec$arm$partition, ref$arm$partition)
  expect_length(built$problems, 0)
})

test_that("the review compares Figure 6 at its sampling sites and reports the section", {
  review <- paperReviewRun(henthornExtraction())
  fe <- Filter(function(f) identical(f$section, PAPER_REVIEW_FRONT_END_SECTION), review$findings)
  titles <- vapply(fe, function(f) f$title, "")
  expect_true(any(grepl("mixing model is built", titles)))
  figure <- fe[[grep("Figure 6", titles)]]
  expect_equal(figure$severity, "ok")
  expect_true(any(vapply(review$figures, function(f) grepl("front-end model", f$id), logical(1))))
  # The report renders the section.
  html <- as.character(paperReviewReportTags(review, plots = FALSE))
  expect_match(html, PAPER_REVIEW_FRONT_END_SECTION, fixed = TRUE)
})

test_that("a recirculatory model's flows must balance, and a missing block is no front end", {
  ex <- henthornExtraction()
  ex$front_end <- list(model_type = "recirculatory", total_flow = henthornQuantity(3, "L/min"),
    elimination_clearance = henthornQuantity(0.17, "L/min"), elimination_in_flow = "yes",
    central = list(list(flow_fraction = 1, volume = henthornQuantity(1.11, "L"), compartments = 10)),
    pathways = list(list(name = "fast", kind = "tissue", flow = henthornQuantity(1.29, "L/min"),
                         volume = henthornQuantity(14.26, "L"))))
  built <- paperReviewFrontEndSpec(ex, paperReviewReferencePatient(ex))
  expect_null(built$spec)
  expect_match(built$problems, "blood would be created or lost")
  ex$front_end <- NULL
  expect_null(paperReviewFrontEndSpec(ex, paperReviewReferencePatient(ex)))
  ex$front_end <- list(model_type = "none")
  expect_null(paperReviewFrontEndSpec(ex, paperReviewReferencePatient(ex)))
})

test_that("a reply without the front-end block is not told it lacks one", {
  # A reply made before the block existed (a saved review, an old fixture).
  ex <- paperReviewExample()
  ex$front_end <- NULL
  ex <- paperReviewValidate(ex)
  expect_false(grepl("front_end", ex$extraction_limitations %or% ""))
  expect_null(paperReviewFrontEndSpec(ex, paperReviewReferencePatient(ex)))
})

test_that("the example's empty front-end block is no front-end model", {
  ex <- paperReviewValidate(paperReviewExample())
  expect_equal(ex$front_end$model_type, "none")
  expect_false(grepl("front_end", ex$extraction_limitations %or% ""))
  expect_null(paperReviewFrontEndSpec(ex, paperReviewReferencePatient(ex)))
})

test_that("supplied unusable optional blocks are reported", {
  for (bad in list("bad", 42, NULL, list())) {
    ex <- paperReviewExample()
    ex["front_end"] <- list(bad)
    expect_match(paperReviewValidate(ex)$extraction_limitations, "front_end")
  }
})

test_that("front-end dose conversion uses the comparison horizon and repeats boluses", {
  patient <- list(weight = 70)
  dose <- list(route = "iv_infusion", time = 2, amount = 60, mass_unit = "mg",
               per_time = "per_h", time_units = "min", duration = "")
  convert <- function(d) paperReviewFrontEndDoses(list(doses = list(d)), patient, horizon = 12)
  result <- convert(dose)
  expect_length(result$problems, 0)
  expect_equal(result$doses$infusion$end, 12)
  expect_equal(result$doses$infusion$rate, 1)
  dose$duration <- 3
  expect_equal(convert(dose)$doses$infusion$end, 5)
  for (bad in list("unreadable", Inf, NaN)) {
    dose$duration <- bad
    expect_length(convert(dose)$problems, 1)
  }
  dose$route <- "iv_bolus"; dose$per_time <- "none"; dose$duration <- ""; dose$interval <- 5
  expect_equal(convert(dose)$doses$bolus$time, c(2, 7, 12))
  dose$duration <- 10
  expect_equal(convert(dose)$doses$bolus$time, c(2, 7))
  dose$interval <- NULL
  expect_equal(convert(dose)$doses$bolus$time, 2)
})

test_that("effect-site figures and partially converted regimens are set aside", {
  ex <- henthornExtraction()
  ex$figures[[1]]$concentration_site <- "effect_site"
  expect_length(paperReviewFrontEndChecks(ex, paperReviewReferencePatient(ex))$figures, 0)
  ex$figures[[1]]$concentration_site <- "plasma"
  ex$regimens[[1]]$doses[[2]] <- list(route = "oral")
  result <- paperReviewFrontEndChecks(ex, paperReviewReferencePatient(ex))
  expect_length(result$figures, 0)
  expect_true(any(vapply(result$findings, function(f) grepl("not intravenous", f$detail %or% ""), logical(1))))
})

test_that("a front-end model with no mammillary parameters is not described as one", {
  ex <- henthornExtraction()
  ex$parameters <- list()
  for (compartments in c(0, 3)) {
    ex$structure$compartments <- compartments
    review <- paperReviewRun(ex)
    expect_identical(review$identifiability$verdict, "front_end")
    expect_false(any(vapply(review$findings, function(f) identical(f$section, "Identifiability"), logical(1))))
    html <- as.character(paperReviewReportTags(review, plots = FALSE))
    expect_match(html, "a front-end (mixing) model", fixed = TRUE)
    expect_false(grepl("compartment(s)", html, fixed = TRUE))
    expect_false(grepl("does not print enough to close the model", html, fixed = TRUE))
    expect_match(html, PAPER_REVIEW_FRONT_END_SECTION, fixed = TRUE)
  }
  # With its own parameters the mammillary model is still checked.
  expect_false(identical(paperReviewRun(henthornExtraction())$identifiability$verdict, "front_end"))
})
