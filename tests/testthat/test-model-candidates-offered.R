# A model group's candidates are not offered for dosing (R/modelSelection.R,
# AGENTS.md "The program chooses the model, never the physician").  Devin's
# review of PR #1032 found them in the dose grid, Add a dose and the
# Simulation API.  Drafted by Claude Code, 2026-11-28.

dd <- getDrugDefaultsGlobal()
candidates <- modelCandidates()

test_that("the dose grid and Add a dose offer no candidate, and every group by name", {
  for (illicit in c(FALSE, TRUE)) {
    offered <- visibleDrugNames(illicit, dd)
    expect_length(intersect(offered, candidates), 0)
  }
  expect_true(all(names(MODEL_GROUPS) %in% c(visibleDrugNames(TRUE, dd), names(MODEL_GROUP_METABOLITES),
                                              MODEL_GROUP_METABOLITES)))
  expect_true(all(c("etoposide", "caspofungin", "teicoplanin") %in% visibleDrugNames(FALSE, dd)))
  # some candidates do carry dose units, which is why they had been offered
  expect_true(any(isDosableDrug(candidates, dd)))
})

test_that("a link or grid row naming a candidate is read as an unknown drug", {
  r <- restoredDoseTable(list(Drug = c("etoposideKroghMadsen", "etoposide"), Time = c("0", "0"),
                              Dose = c("100", "100"), Units = c("mg", "mg")), dd)
  expect_true(r$changed)
  expect_false("etoposideKroghMadsen" %in% r$table$Drug)
  expect_true("etoposide" %in% r$table$Drug)
})

test_that("the Simulation API lists no candidate and refuses a dose of one", {
  catalog <- apiDrugCatalog(allowIllicit = TRUE)
  expect_length(intersect(catalog$Drug, candidates), 0)
  expect_true("etoposide" %in% catalog$Drug)
  req <- function(drug) list(
    patient = list(weight = 70, height = 170, age = 50, sex = "male"),
    duration = 60,
    doses = list(drug = drug, time = 0, dose = 100, units = "mg"))
  expect_error(apiSimulate(req("etoposideKroghMadsen")), "published models of etoposide",
               class = "stanpumpr_api_error")
  expect_no_error(apiSimulate(req("etoposide")))
})
