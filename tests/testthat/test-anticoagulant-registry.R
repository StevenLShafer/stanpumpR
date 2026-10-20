# The anticoagulant registry and its parameter audit.  See
# data-raw/anticoagulantRegistry.R (which builds the two CSVs) and the
# perioperative-anticoagulant specification.  Drafted by Claude Code,
# 2026-10-17.

registry <- utils::read.csv(
  system.file("extdata", "anticoagulantRegistry.csv", package = "stanpumpR"),
  stringsAsFactors = FALSE, na.strings = "NA")
parameters <- utils::read.csv(
  system.file("extdata", "anticoagulantParameters.csv", package = "stanpumpR"),
  stringsAsFactors = FALSE, na.strings = "NA")

READINESS <- c("research_ready", "provisional", "pending")
PROVENANCE <- c("estimated", "fixed", "label-derived", "derived", "calibration")

# The drugs implemented new for this registry.
IMPLEMENTED <- c("heparin", "bivalirudin", "apixaban", "rivaroxaban", "warfarin",
                 "edoxaban", "dabigatran", "argatroban", "enoxaparin", "fondaparinux",
                 "fluindione", "acenocoumarol", "dabigatranHip")

test_that("every registry row is fully classified with a clickable source", {
  for (col in c("model_id", "drug_class", "context", "analyte", "assay",
                "route", "readiness", "blocking_issue", "source_url"))
    expect_true(all(nzchar(registry[[col]])), info = col)
  expect_false(any(duplicated(registry$model_id)))
  expect_true(all(registry$readiness %in% READINESS))
  expect_true(all(grepl("^https?://", registry$source_url)))
})

test_that("a research-ready model maps to a real library drug, and a pending one does not", {
  defaults <- getDrugDefaultsGlobal()
  ready <- registry[registry$readiness == "research_ready", ]
  # every research-ready entry names a drug that exists in the library
  expect_true(all(!is.na(ready$stanpumpr_drug)))
  expect_true(all(ready$stanpumpr_drug %in% defaults$Drug))
  for (d in unique(ready$stanpumpr_drug))
    expect_true(exists(d, mode = "function"), info = d)
  # a provisional or pending model is NOT implemented as a selectable curve
  notReady <- registry[registry$readiness != "research_ready", ]
  expect_true(all(is.na(notReady$stanpumpr_drug)))
})

test_that("every Anticoagulants-category drug is a research-ready registry entry", {
  defaults <- getDrugDefaultsGlobal()
  anticoag <- defaults$Drug[defaults$Category %in% "Anticoagulants"]
  ready <- registry$stanpumpr_drug[registry$readiness == "research_ready"]
  expect_setequal(anticoag, IMPLEMENTED)
  expect_true(all(anticoag %in% ready))
})

test_that("the common perioperative agents are all catalogued", {
  # The specification's priority agents, by drug class or model id stem.
  for (stem in c("ufh", "enox", "fonda", "biva", "arg",
                 "apix", "riva", "dabi", "edox", "warf", "flu", "aceno"))
    expect_true(any(grepl(paste0("^", stem), registry$model_id)), info = stem)
  # and the pending LMWH/VKA extensions the spec names
  for (m in c("dalteparin_label", "tinzaparin_label",
              "phenprocoumon", "nadroparin", "danaparoid",
              "desirudin"))
    expect_true(m %in% registry$model_id, info = m)
})

test_that("the parameter audit is complete for every implemented drug", {
  expect_true(all(IMPLEMENTED %in% parameters$drug))
  for (col in c("drug", "parameter", "value", "units", "provenance",
                "population", "source_url"))
    expect_true(all(nzchar(parameters[[col]]) & !is.na(parameters[[col]])),
                info = col)
  expect_true(all(parameters$provenance %in% PROVENANCE))
  expect_true(all(grepl("^https?://", parameters$source_url)))
  expect_false(any(parameters$value %in% c("", "NA")))
})

test_that("each implemented model's clearance and volume appear in the audit", {
  for (d in IMPLEMENTED) {
    rows <- parameters[parameters$drug == d, ]
    expect_gt(nrow(rows), 1, label = d)
    joined <- tolower(paste(rows$parameter, collapse = " "))
    expect_match(joined, "cl", info = d)
    expect_match(joined, "v", info = d)
  }
})

test_that("no anticoagulant carries a clinical threshold: plasma/activity/INR only", {
  defaults <- getDrugDefaultsGlobal()
  for (d in IMPLEMENTED) {
    row <- defaults[defaults$Drug == d, ]
    expect_equal(row$MEAC, 0, info = d)
    expect_equal(row$endCe, 0, info = d)
    expect_equal(row$Lower, 0, info = d)
    expect_equal(row$Upper, 0, info = d)
  }
})
