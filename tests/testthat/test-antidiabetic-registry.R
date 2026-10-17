# The antidiabetic registry and its parameter audit (data-raw/
# antidiabeticRegistry.R builds both).  Drafted by Claude Code, 2026-10-17.

registry <- utils::read.csv(
  system.file("extdata", "antidiabeticRegistry.csv", package = "stanpumpR"),
  stringsAsFactors = FALSE, na.strings = "NA")
parameters <- utils::read.csv(
  system.file("extdata", "antidiabeticParameters.csv", package = "stanpumpR"),
  stringsAsFactors = FALSE, na.strings = "NA")

PK_STATUS <- c("PK-FIT", "PK-AUDIT", "PK-CONSTRAINT", "PK-TEMPLATE")
PD_STATUS <- c("PD-FIT", "PD-AUDIT", "PD-CONSTRAINT", "PD-TEMPLATE")
TIERS <- c("core", "international", "optional", "out_of_scope")
PROVENANCE <- c("estimated", "fixed", "derived", "assumed", "library")

test_that("every registry row is one product key, fully classified, with a source", {
  expect_false(any(duplicated(registry$key)))
  for (col in c("key", "ingredient", "product", "route", "registry_tier",
                "spec_pk_status", "pk_status", "spec_pd_status", "pd_status", "source_url"))
    expect_true(all(nzchar(registry[[col]]) & !is.na(registry[[col]])), info = col)
  expect_true(all(registry$pk_status %in% PK_STATUS))
  expect_true(all(registry$spec_pk_status %in% PK_STATUS))
  expect_true(all(registry$pd_status %in% PD_STATUS))
  expect_true(all(registry$spec_pd_status %in% PD_STATUS))
  expect_true(all(registry$registry_tier %in% TIERS))
  expect_true(all(grepl("^https?://", registry$source_url)))
})

test_that("only PK-FIT entries are implemented, and each is a real library drug", {
  implemented <- registry[!is.na(registry$stanpumpr_drug), ]
  expect_true(all(implemented$pk_status == "PK-FIT"))
  defaults <- getDrugDefaultsGlobal()
  expect_true(all(implemented$stanpumpr_drug %in% defaults$Drug))
  for (d in implemented$stanpumpr_drug) expect_true(exists(d, mode = "function"), info = d)
  # every drug in the Antidiabetic category is an implemented registry entry
  expect_setequal(defaults$Drug[defaults$Category %in% "Antidiabetic"],
                  implemented$stanpumpr_drug)
  # one product per drug
  expect_false(any(duplicated(implemented$stanpumpr_drug)))
})

test_that("no pharmacodynamic endpoint is implemented", {
  expect_true(all(!registry$pd_implemented))
  defaults <- getDrugDefaultsGlobal()
  ad <- defaults[defaults$Category %in% "Antidiabetic", ]
  expect_true(all(ad$MEAC == 0 & ad$Lower == 0 & ad$Upper == 0 & ad$Typical == 0 & ad$endCe == 0))
  for (d in ad$Drug) expect_equal(get(d)(70, 170, 40, "male")$tPeak, 0, info = d)
})

test_that("only a product whose label permits a pump offers U/hr SC", {
  defaults <- getDrugDefaultsGlobal()
  pumpDrugs <- defaults$Drug[vapply(defaults$Units, function(u) any(u %in% insulinRateUnits), logical(1))]
  permitted <- registry$stanpumpr_drug[!is.na(registry$stanpumpr_drug) & registry$csii %in% TRUE]
  expect_setequal(pumpDrugs, permitted)
  # the products the specification names as pump-forbidden are marked so
  forbidden <- c("insulin_lispro_humalog_u200", "insulin_lispro_aabc_lyumjev_u200",
                 "insulin_aspart_szjj_merilog", "insulin_glargine_semglee_u100",
                 "insulin_glargine_rezvoglar_u100", "soliqua_100_33", "xultophy_100_3_6")
  expect_true(all(registry$csii[match(forbidden, registry$key)] %in% FALSE))
  expect_true(is.na(registry$csii[registry$key == "insulin_aspart_fsan_garzulys"]))
})

test_that("the specification's product keys are present and kept apart", {
  for (k in c("semaglutide_po_rybelsus", "semaglutide_po_ozempic_tablets",
              "exenatide_ir_sc", "exenatide_er_bydureon", "exenatide_er_bcise",
              "insulin_glargine_lantus_u100", "insulin_glargine_u300_toujeo",
              "insulin_human_inhaled_afrezza", "metformin_er", "soliqua_100_33",
              "xultophy_100_3_6", "dulaglutide_sc", "pioglitazone_kadam2013"))
    expect_true(k %in% registry$key, info = k)
  expect_true(all(is.na(registry$stanpumpr_drug[grepl("^exenatide|toujeo|lantus|semglee|rezvoglar",
                                                       registry$key)])))
})

test_that("every implemented model's numbers are in the audit with provenance and a source", {
  implemented <- registry$stanpumpr_drug[!is.na(registry$stanpumpr_drug)]
  expect_setequal(unique(parameters$drug), implemented)
  for (col in c("drug", "parameter", "value", "units", "provenance", "population",
                "source_url", "locator"))
    expect_true(all(nzchar(parameters[[col]]) & !is.na(parameters[[col]])), info = col)
  expect_true(all(parameters$provenance %in% PROVENANCE))
  expect_true(all(grepl("^https?://", parameters$source_url)))
  for (d in implemented) {
    rows <- parameters[parameters$drug == d, ]
    expect_match(paste(rows$parameter, collapse = " "), "CL", info = d)
    expect_match(paste(rows$parameter, collapse = " "), "V", info = d)
  }
  # the derived values are marked derived
  derived <- parameters[parameters$provenance == "derived", ]
  expect_true(any(derived$drug == "semaglutide" & grepl("weight exponent", derived$parameter)))
  expect_true(any(derived$drug == "tirzepatide" & derived$parameter == "F"))
})

test_that("the audited numbers are the ones the models use", {
  sema <- semaglutide(85, 165, 50, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(sema$cl1 * 60, 0.0478)
  expect_equal(sema$ka_SC * 60, 0.0286)
  expect_equal(SEMAGLUTIDE_WEIGHT_EXPONENT, 0.78)
  expect_equal(TIRZEPATIDE_BIOAVAILABILITY,
               as.numeric(parameters$value[parameters$drug == "tirzepatide" & parameters$parameter == "F"]),
               tolerance = 1e-3)
  for (d in c("insulinRegular", "insulinLispro", "insulinGlargine")) {
    rows <- parameters[parameters$drug == d, ]
    pk <- get(d)(70, 170, 35, "male")$PK$default
    expect_equal(pk$v1, as.numeric(rows$value[rows$parameter == "V/F"]), info = d)
    expect_equal(pk$cl1 * 60, as.numeric(rows$value[rows$parameter == "CL/F"]), info = d)
  }
})
