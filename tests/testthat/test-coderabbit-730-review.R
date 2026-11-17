# CodeRabbit's review of PR #730 (2026-11-15).  Each lead was reproduced on
# f197d63 before it was fixed, and each test here fails there.

# --- A regimen id repeated across model groups -------------------------------
#
# paperReviewNameRegimens() (Carbon's fourth UI audit, M5) renamed the second
# of two regimens called "R1" whatever model they belonged to.  Two groups
# that each have an "R1" are reviewed apart (paperReviewGroupExtraction()),
# so renaming the second group's regimen to "R1 (2)" cut it off from its own
# ka and F, its derived quantities and its figure series, which still name
# "R1": the Elderly model below lost its oral absorption.

twoModelPaper <- function() {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "oral")
  ex$derived_quantities <- list()
  ex$figures <- list()
  absorption <- function(g, ka, F, regimen = "R1") list(
    list(name_in_paper = "ka", group = g, role = "ka", route = "oral", regimen_id = regimen, value = ka,
         value_text = format(ka), units = "1/h", uncertainty = list(type = "not_reported"),
         status = "estimated", location = "Table 2"),
    list(name_in_paper = "F", group = g, role = "F", route = "oral", regimen_id = regimen, value = F,
         value_text = format(F), units = "percent", uncertainty = list(type = "not_reported"),
         status = "estimated", location = "Table 2"))
  disposition <- ex$parameters
  grouped <- function(g, scale) lapply(disposition, function(p) {
    p$group <- g
    if (identical(p$role, "CL")) p$value <- p$value * scale
    p
  })
  ex$parameters <- c(grouped("Adults", 1), absorption("Adults", 1.2, 50),
                     grouped("Elderly", 0.7), absorption("Elderly", 0.6, 80))
  ex$regimens <- list(oralRegimen("R1", "Adults", 100), oralRegimen("R1", "Elderly", 200))
  ex
}
oralRegimen <- function(id, group, amount) {
  r <- list(id = id, description = paste(amount, "mg by mouth"),
            doses = list(list(route = "oral", time = 0, amount = amount, mass_unit = "mg", per_kg = "no",
                              per_time = "none", duration = NULL, time_units = "min")),
            location = "Methods")
  if (!is.null(group)) r$group <- group
  r
}
regimenIds <- function(x) vapply(x$regimens, function(r) r$id, "")

test_that("two model groups that each have an R1 keep their own regimen and absorption", {
  ex <- twoModelPaper()
  # A figure series of each model, tied to its own R1
  series <- function(g) list(label = g, group = g, analyte = "parent", regimen_id = "R1",
                             points = list(list(time = 60, value = 1), list(time = 120, value = 1.5),
                                           list(time = 240, value = 1)))
  ex$figures <- list(list(figure_id = "Figure 4", caption = "Oral doses", regimen_id = NULL,
                          y_quantity = "concentration", y_units = "ng/mL", y_normalization = "none",
                          concentration_site = "plasma", time_units = "min", log_y = "no",
                          data_kind = "mean", one_regimen = "yes", error_bars = "none",
                          series = list(series("Adults"), series("Elderly")),
                          digitization_note = "read by eye", location = "Figure 4"))
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  expect_identical(names(review$models), c("Adults", "Elderly"))
  # Nothing is renamed, and the paper says nothing of it
  expect_identical(regimenIds(review$extraction), c("R1", "R1"))
  expect_false(grepl("same id", review$extraction$extraction_limitations %or% "", fixed = TRUE))
  for (g in names(review$models)) {
    m <- review$models[[g]]
    expect_identical(regimenIds(m$extraction), "R1")
    expect_true(isTRUE(m$absorptions[["R1"]]$ok), info = g)
    # its own figure series found its regimen and was simulated
    expect_length(m$figures, 1)
    expect_false(any(vapply(m$findings, function(f) grepl("names no regimen", f$detail %or% "", fixed = TRUE),
                            logical(1))), info = g)
  }
  # Each model's own ka and F (the Elderly's were lost: no usable absorption)
  expect_equal(review$models$Adults$absorptions$R1$ka * 60, 1.2)
  expect_equal(review$models$Adults$absorptions$R1$F, 0.5)
  expect_equal(review$models$Elderly$absorptions$R1$ka * 60, 0.6)
  expect_equal(review$models$Elderly$absorptions$R1$F, 0.8)
  # and its own doses
  expect_equal(paperReviewRegimen(review$models$Elderly$extraction, "R1")$doses[[1]]$amount, 200)
})

test_that("the Simulator loads the Elderly model's R1 with that model's absorption", {
  ex <- twoModelPaper()
  testthat::local_mocked_bindings(paperReviewExample = function() ex)
  shiny::testServer(paperReviewServer, {
    session$setInputs(example = 1)
    session$setInputs(model = "Elderly")
    session$setInputs(regimen = "R1")
    session$setInputs(loadRegimen = 1)
    expect_equal(doseTable()$Dose, 200)
    expect_equal(simAbsorption()$F, 0.8)
    expect_equal(simAbsorption()$ka * 60, 0.6)
  })
})

test_that("two regimens of one id in one model of several are still told apart (M5)", {
  ex <- twoModelPaper()
  ex$regimens <- c(ex$regimens, list(oralRegimen("R1", "Elderly", 300)))
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  expect_identical(regimenIds(review$extraction), c("R1", "R1", "R1 (2)"))
  expect_match(review$extraction$extraction_limitations,
               "the next \"R1\" in the model \"Elderly\" is called \"R1 (2)\"", fixed = TRUE)
  expect_identical(regimenIds(review$models$Elderly$extraction), c("R1", "R1 (2)"))
  expect_identical(regimenIds(review$models$Adults$extraction), "R1")
  # what the Elderly model ties to R1 is read as its first R1
  expect_equal(paperReviewRegimen(review$models$Elderly$extraction, "R1")$doses[[1]]$amount, 200)
  expect_equal(paperReviewRegimen(review$models$Elderly$extraction, "R1 (2)")$doses[[1]]$amount, 300)
  expect_true(isTRUE(review$models$Adults$absorptions$R1$ok))
  expect_true(isTRUE(review$models$Elderly$absorptions$R1$ok))
  # A paper with one model renames as before
  one <- paperReviewExample()
  one$regimens <- list(oralRegimen("R1", NULL, 100), oralRegimen("R1", NULL, 200))
  expect_identical(regimenIds(paperReviewNameRegimens(one)), c("R1", "R1 (2)"))
  # and one whose regimens name two groups has two models: not renamed
  one$regimens <- list(oralRegimen("R1", "Adults", 100), oralRegimen("R1", "Elderly", 200))
  expect_length(paperReviewGroups(one), 2)
  expect_identical(regimenIds(paperReviewNameRegimens(one)), c("R1", "R1"))
})

test_that("a model's own regimen with a shared regimen's id is renamed in that model only", {
  ex <- twoModelPaper()
  # A shared intravenous R1 before the Elderly model's oral R1
  shared <- paperReviewExample()$regimens[[1]]
  shared$id <- "R1"
  shared$group <- NULL
  ex$regimens <- list(shared, oralRegimen("R1", "Elderly", 200))
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  # The paper's own list is untouched; the Elderly model's part has both, apart
  expect_identical(regimenIds(review$extraction), c("R1", "R1"))
  expect_identical(regimenIds(review$models$Adults$extraction), "R1")
  expect_identical(regimenIds(review$models$Elderly$extraction), c("R1", "R1 (2)"))
  expect_match(review$extraction$extraction_limitations,
               "the next \"R1\" in the model \"Elderly\" is called \"R1 (2)\"", fixed = TRUE)
  # what the Elderly model ties to R1 is read as the first R1 it has (M5)
  expect_equal(paperReviewRegimen(review$models$Elderly$extraction, "R1 (2)")$doses[[1]]$amount, 200)
})
