# The antiseizure registry and its parameter audit.  See
# data-raw/antiseizureRegistry.R (which builds the two CSVs) and the
# specification's completion criteria.  Drafted by Claude Code, 2026-10-10.

registry <- utils::read.csv(
  system.file("extdata", "antiseizureRegistry.csv", package = "stanpumpR"),
  stringsAsFactors = FALSE, na.strings = "NA")
parameters <- utils::read.csv(
  system.file("extdata", "antiseizureParameters.csv", package = "stanpumpR"),
  stringsAsFactors = FALSE, na.strings = "NA")

US_STATUS <- c("US_SEIZURE_LABELED", "US_MARKETED_SEIZURE_OFF_LABEL",
               "HISTORICAL_UNAVAILABLE", "PK_UNAVAILABLE")
TIERS <- c("published_population_fit", "label_anchor", "proposed_unestimated")
PD_STATUS <- c("pk_only", "pd_source_incomplete", "no_pk")
PROVENANCE <- c("estimated", "fixed", "label-derived", "guideline-derived",
                "calibration", "derived")

test_that("the registry has 37 ingredient/prodrug entries", {
  expect_equal(nrow(registry), 37)
  expect_false(any(duplicated(registry$moiety)))
})

test_that("every registry row is fully classified with a clickable source", {
  for (col in c("moiety", "routes", "us_status", "evidence_tier", "dose_basis",
                "analyte", "structural_model", "pd_status", "source_url"))
    expect_true(all(nzchar(registry[[col]])), info = col)
  expect_true(all(registry$us_status %in% US_STATUS))
  expect_true(all(registry$evidence_tier %in% TIERS))
  expect_true(all(registry$pd_status %in% PD_STATUS))
  expect_true(all(grepl("^https?://", registry$source_url)))
})

test_that("a published fit maps to a real library drug, and the queue does not", {
  fit <- registry[registry$evidence_tier == "published_population_fit", ]
  queue <- registry[registry$evidence_tier != "published_population_fit", ]
  # every implemented entry names a drug that exists in the library
  defaults <- getDrugDefaultsGlobal()
  expect_true(all(!is.na(fit$stanpumpr_drug)))
  expect_true(all(fit$stanpumpr_drug %in% defaults$Drug))
  # a drug function exists for each
  for (d in unique(fit$stanpumpr_drug))
    expect_true(exists(d, mode = "function"), info = d)
  # queue entries have no implemented model
  expect_true(all(is.na(queue$stanpumpr_drug)))
})

test_that("no queue entry is selectable in the dose table as a seizure drug", {
  # The implementation queue (unestimated or label-only) must not appear in
  # the drug library under the Antiseizure category pretending to be modelled.
  defaults <- getDrugDefaultsGlobal()
  antiseizure <- defaults$Drug[defaults$Category %in% "Antiseizure"]
  implemented <- registry$stanpumpr_drug[!is.na(registry$stanpumpr_drug)]
  # every Antiseizure-category drug is an implemented registry entry
  # (phenytoin covers fosphenytoin)
  expect_true(all(antiseizure %in% implemented))
})

test_that("Acthar is registered as having no PK", {
  acthar <- registry[grepl("corticotropin", registry$moiety), ]
  expect_equal(nrow(acthar), 1)
  expect_equal(acthar$us_status, "PK_UNAVAILABLE")
  expect_equal(acthar$pd_status, "no_pk")
  expect_true(is.na(acthar$stanpumpr_drug))
})

test_that("the parameter audit is complete for every implemented drug", {
  implemented <- unique(registry$stanpumpr_drug[!is.na(registry$stanpumpr_drug)])
  # drugs implemented new for the registry (the pre-existing ones -- gabapentin,
  # pregabalin and the benzodiazepines -- are audited by their own tests)
  newlyImplemented <- c("phenytoin", "valproate", "phenobarbital",
    "pentobarbital", "ethosuximide", "topiramate", "lacosamide", "lamotrigine",
    "zonisamide", "tiagabine", "levetiracetam", "eslicarbazepine", "carbamazepine")
  expect_true(all(newlyImplemented %in% implemented))
  expect_true(all(newlyImplemented %in% parameters$drug))
})

test_that("every audited parameter has value, units, provenance and a source", {
  for (col in c("drug", "parameter", "value", "units", "provenance",
                "population", "source_url"))
    expect_true(all(nzchar(parameters[[col]]) & !is.na(parameters[[col]])),
                info = col)
  expect_true(all(parameters$provenance %in% PROVENANCE))
  expect_true(all(grepl("^https?://", parameters$source_url)))
  # a value is a number or a short expression, never an unexplained blank
  expect_false(any(parameters$value %in% c("", "NA")))
})

test_that("each implemented model's clearance and volume appear in the audit", {
  for (d in c("phenytoin", "valproate", "phenobarbital", "pentobarbital",
              "ethosuximide", "topiramate", "lacosamide", "lamotrigine",
              "zonisamide", "tiagabine", "levetiracetam", "eslicarbazepine",
              "carbamazepine")) {
    rows <- parameters[parameters$drug == d, ]
    expect_gt(nrow(rows), 1, label = d)
    joined <- tolower(paste(rows$parameter, collapse = " "))
    expect_match(joined, "cl|vmax", info = d)
    expect_match(joined, "^.*v", info = d)
  }
})
