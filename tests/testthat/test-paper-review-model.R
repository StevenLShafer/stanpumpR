# Single Paper Review: the paper's drug built in memory
# (R/paper-review-model.R), run through getDrugPK(model = ...) and simCpCe().

test_that("units convert to stanpumpR's internal units", {
  expect_equal(paperReviewUnit("mL/min/kg")$factor, 1e-3)
  expect_true(paperReviewUnit("mL/min/kg")$perKg)
  expect_equal(paperReviewUnit("L/h")$factor, 1 / 60)
  expect_equal(paperReviewUnit("1/h")$kind, "rate")
  expect_equal(paperReviewUnit("h")$factor, 60)
  expect_true(is.na(paperReviewUnit("other")$kind))
  expect_equal(paperReviewConcentrationFactor("mg/L", "ng"), 1000)
  expect_equal(paperReviewConcentrationFactor("ng/L", "ng"), 1e-3)
  expect_equal(paperReviewConcentrationFactor("nmol/L", "ng", 300), 0.3)
  expect_true(is.na(paperReviewConcentrationFactor("nmol/L", "ng", NULL)))
})

test_that("a parameter printed in a unit of the wrong kind is refused, not converted", {
  ex <- paperReviewExample()
  ex$parameters[[4]]$units <- "L"          # CL printed as a volume
  v <- paperReviewParameterValue(ex, "CL", 70)
  expect_true(is.na(v$value))
  expect_match(v$problem, "not a clearance unit")
  drug <- paperReviewDrug(ex, paperReviewReferencePatient(ex))
  expect_false(drug$ok)
})

test_that("the reference patient comes from the printed reference values first", {
  ref <- paperReviewReferencePatient(paperReviewExample())
  expect_equal(ref$weight, 70)          # CL's weight reference, not the population mean 75
  expect_equal(ref$age, 40)             # V1's age reference
  expect_equal(ref$height, 174)         # no height covariate: population mean
  expect_match(ref$source[["height"]], "population mean")
})

test_that("covariates scale the typical values for another patient", {
  ex <- paperReviewExample()
  p <- list(weight = 90, height = 180, age = 60, sex = "female")
  set <- paperReviewDrug(ex, p)$model$PK$default
  expect_equal(set$cl1, 1.2 * (90 / 70)^0.75)
  expect_equal(set$v1, 8 * (1 - 0.005 * (60 - 40)))
  expect_equal(set$v3, 120)
})

test_that("the in-memory drug simulates exactly the analytical triexponential", {
  ex <- paperReviewExample()
  ref <- paperReviewReferencePatient(ex)
  drug <- paperReviewDrug(ex, ref)
  sim <- paperReviewSimulate(drug, ref, data.frame(Time = 0, Dose = 1, Units = "mg/kg"), 360)
  lambda <- c(0.533393937932, 0.043363495467, 0.003242566601)
  A <- c(0.1101863309487, 0.0138143541574, 0.0009993148939)
  t <- sim$curve$Time
  analytical <- 70 * colSums(A * exp(-outer(lambda, t))) * 1000     # ng/mL
  expect_equal(sim$curve$Cp, analytical, tolerance = 1e-8)
  # tPeak 3 min: the effect site peaks there.
  expect_equal(sim$curve$Time[which.max(sim$curve$Ce)], 3, tolerance = 0.05)
})

test_that("one- and two-compartment models build and simulate (an absent compartment is not a zero volume)", {
  # The first live review, of a two-compartment paper, failed here:
  # getDrugPK() refuses v3 = 0, and the library writes an absent compartment
  # as a placeholder volume with no clearance into it.
  two <- jsonlite::fromJSON(test_path("fixtures", "paper-review-well-conditioned.json"),
                            simplifyVector = FALSE)
  review <- paperReviewRun(two)
  expect_true(review$drug$ok)
  set <- review$drug$model$PK$default
  expect_equal(set$v3, PAPER_REVIEW_ABSENT_VOLUME)
  expect_equal(set$cl3, 0)
  ref <- review$reference
  sim <- paperReviewSimulate(review$drug, ref,
                             data.frame(Time = c(0, 3), Dose = c(1, 0), Units = c("mg/kg/min", "mg/kg/min")), 120)
  expect_gt(max(sim$curve$Cp), 0)
  expect_false(any(vapply(review$findings, function(f) grepl("could not", f$title), logical(1))))

  one <- paperReviewExample()
  one$structure$compartments <- 1
  one$parameters <- Filter(function(p) p$role %in% c("CL", "V1"), one$parameters)
  one$derived_quantities <- list(list(quantity = "t_half_terminal", value = log(2) * 8 / 1.2,
    value_text = format(log(2) * 8 / 1.2, digits = 6), units = "min", statistic = "typical_value",
    uncertainty = list(type = "not_reported"), location = "Table 3"))
  r1 <- paperReviewRun(one)
  expect_equal(r1$consistency$status, "matches")
  expect_equal(r1$drug$model$PK$default$v2, PAPER_REVIEW_ABSENT_VOLUME)
})

test_that("the drug never takes a library drug's name, and getDrugPK still refuses unknown drugs", {
  ex <- paperReviewExample()
  ex$drug$name <- "propofol"
  drug <- paperReviewDrug(ex, paperReviewReferencePatient(ex))
  expect_equal(drug$defaults$Drug, "propofol (paper)")
  expect_false(drug$defaults$Drug %in% getDrugDefaultsGlobal()$Drug)
  expect_error(getDrugPK("propofol (paper)", 70, 170, 40, "male", drug$defaults), "Unknown drug")
})

test_that("an extravascular paper offers its route with ka, F and lag", {
  ex <- paperReviewExample()
  ex$structure$routes <- list("oral")
  ex$parameters <- c(ex$parameters, list(
    list(name_in_paper = "ka", role = "ka", route = "oral", value = 1.2, value_text = "1.2",
         units = "1/h", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2"),
    list(name_in_paper = "F", role = "F", route = "oral", value = 60, value_text = "60",
         units = "percent", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2"),
    list(name_in_paper = "tlag", role = "tlag", route = "oral", value = 0.25, value_text = "0.25",
         units = "h", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2")))
  review <- paperReviewRun(ex)
  ref <- review$reference
  drug <- review$drug
  # No oral regimen: the route's printed ka, F and lag make one absorption.
  expect_true(review$absorptions$route$ok)
  expect_equal(review$absorptions$route$sources[["ka"]], "printed")
  set <- drug$model$PK$default
  expect_equal(set$ka_PO, 1.2 / 60)
  expect_equal(set$bioavailability_PO, 0.6)
  expect_equal(set$tlag_PO, 15)
  expect_true("mg PO" %in% drug$units)
  expect_false("mg" %in% drug$units)
  sim <- paperReviewSimulate(drug, ref, data.frame(Time = 0, Dose = 100, Units = "mg PO"), 600)
  expect_equal(max(sim$curve$Cp[sim$curve$Time < 15]), 0)
  expect_gt(max(sim$curve$Cp), 0)
})

test_that("a paper's regimen becomes a stanpumpR dose table", {
  ex <- paperReviewExample()
  infusion <- paperReviewRegimenDoses(ex$regimens[[2]])$doses
  expect_equal(infusion$Time, c(0, 60))
  expect_equal(infusion$Dose, c(0.1, 0))
  expect_equal(infusion$Units, c("mg/kg/min", "mg/kg/min"))
  hours <- list(doses = list(list(route = "iv_infusion", time = 1, amount = 30, mass_unit = "mg",
                                  per_kg = "no", per_time = "none", duration = 0.5, time_units = "h")))
  d <- paperReviewRegimenDoses(hours)$doses
  expect_equal(d$Time, c(60, 90))
  expect_equal(d$Dose, c(1, 0))       # 30 mg over 30 min
  expect_equal(d$Units[1], "mg/min")
})

test_that("an effect site from ke0 is carried as ke0", {
  ex <- paperReviewExample()
  ex$structure$effect_site <- "ke0"
  ex$parameters <- c(Filter(function(p) p$role != "tPeak", ex$parameters), list(
    list(name_in_paper = "ke0", role = "ke0", route = NULL, value = 0.2732108072,
         value_text = "0.273", units = "1/min", uncertainty = list(type = "not_reported"),
         status = "estimated", location = "T2")))
  ref <- paperReviewReferencePatient(ex)
  drug <- paperReviewDrug(ex, ref)
  expect_equal(drug$model$ke0, 0.2732108072)
  expect_null(drug$model$tPeak)
  pk <- getDrugPK(drug$defaults$Drug, ref$weight, ref$height, ref$age, ref$sex,
                  drug$defaults, model = drug$model)
  expect_equal(pk$tPeak, 3, tolerance = 1e-4)
})

test_that("the simulator's time unit follows the paper's for each regimen", {
  ex <- paperReviewExample()
  # Its regimens and figures are in minutes.
  expect_equal(paperReviewRegimenTimeUnit(ex, ex$regimens[[1]]), "min")
  # A figure showing the regimen sets the unit before its doses do.
  ex$regimens[[2]]$doses[[1]]$time_units <- "day"
  ex$figures[[2]]$time_units <- "week"
  expect_equal(paperReviewRegimenTimeUnit(ex, ex$regimens[[2]]), "week")
  ex$figures[[2]]$regimen_id <- "other"
  expect_equal(paperReviewRegimenTimeUnit(ex, ex$regimens[[2]]), "day")
  expect_equal(paperReviewRegimenTimeUnit(ex, NULL), "min")
  # Weeks are a unit the extraction can now carry.
  expect_equal(paperReviewMinutes("week"), 7 * 1440)
  r <- list(doses = list(list(route = "oral", time = 1, amount = 400, mass_unit = "mg", per_kg = "no",
                              per_time = "per_day", duration = 2, interval = NULL, time_units = "week")))
  d <- paperReviewRegimenDoses(r)$doses
  expect_equal(d$Time, c(1, 3) * 7 * 1440)
})

test_that("the dose table converts when the unit changes, and runs in minutes underneath", {
  testthat::local_mocked_bindings(paperReviewApiKey = function(...) NULL)
  shiny::testServer(paperReviewServer, {
    session$setInputs(example = 1)
    expect_equal(tableUnit(), "min")
    session$setInputs(regimen = "R2", loadRegimen = 1)
    expect_equal(doseTable()$Time, c(0, 60))
    session$setInputs(timeUnit = "h")
    expect_equal(tableUnit(), "h")
    expect_equal(doseTable()$Time, c(0, 60))          # still minutes underneath
    expect_equal(dosesNow()$Time, c(0, 60))
  })
})
