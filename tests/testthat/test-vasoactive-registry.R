# The vasoactive and resuscitation drug registry and its parameter audit.  See
# data-raw/vasoactiveRegistry.R (which builds the two CSVs) and R/vasoactive.R.
# Drafted by Claude Code, 2026-10-18.

registry   <- vasoactiveRegistry()
parameters <- vasoactiveParameters()

READINESS <- c("implemented", "needs_full_text", "no_suitable_model")
EVIDENCE  <- c("full_text", "abstract", "none")

# The drugs implemented new for this registry, in its own category.
IMPLEMENTED <- c("epinephrine", "magnesiumSulfate", "labetalol", "diltiazem",
                 "nitroglycerin")

# Every drug that was asked for, by its name in the registry
REQUESTED <- c("clevidipine", "sodium nitroprusside", "nitroglycerin",
               "epinephrine", "norepinephrine", "vasopressin", "ephedrine",
               "phenylephrine", "hydralazine", "nicardipine", "dopamine",
               "methylene blue", "atropine", "adenosine", "sodium bicarbonate",
               "magnesium sulfate", "procainamide", "verapamil", "diltiazem",
               "propranolol", "metoprolol", "esmolol", "labetalol")

test_that("every row is fully classified with a clickable source", {
  for (col in c("model_id", "drug", "route", "population", "analyte", "matrix",
                "readiness", "evidence_read", "blocking_issue", "source",
                "source_url"))
    expect_true(all(nzchar(registry[[col]]) & !is.na(registry[[col]])),
                info = col)
  expect_false(any(duplicated(registry$model_id)))
  expect_true(all(registry$readiness %in% READINESS))
  expect_true(all(registry$evidence_read %in% EVIDENCE))
  expect_true(all(grepl("^https?://", registry$source_url)))
  expect_true(all(registry$route == "IV"))
})

test_that("every requested drug is catalogued", {
  expect_setequal(unique(registry$drug), REQUESTED)
})

test_that("an implemented row maps to a library drug, and no other row does", {
  dd <- getDrugDefaultsGlobal()
  done <- registry[registry$readiness == "implemented", ]
  expect_true(all(done$stanpumpr_drug %in% dd$Drug))
  for (d in done$stanpumpr_drug) expect_true(exists(d, mode = "function"), info = d)
  expect_true(all(is.na(registry$stanpumpr_drug[registry$readiness != "implemented"])))
})

test_that("the category holds exactly the implemented drugs, concentration only", {
  dd <- getDrugDefaultsGlobal()
  expect_setequal(dd$Drug[dd$Category %in% VASOACTIVE_DRUG_CATEGORY], IMPLEMENTED)
  expect_true(all(IMPLEMENTED %in% registry$stanpumpr_drug))
  for (d in IMPLEMENTED) {
    row <- dd[dd$Drug == d, ]
    expect_equal(row$MEAC, 0, info = d)
    expect_equal(row$endCe, 0, info = d)
    expect_equal(row$Lower, 0, info = d)
    expect_equal(row$Upper, 0, info = d)
    PK <- getDrugPK(d, 70, 170, 50, "male")
    expect_equal(PK$tPeak, 0, info = d)
  }
  # Clevidipine was already in the library, in the Antihypertensives category,
  # and stays there
  expect_identical(dd$Category[dd$Drug == "clevidipine"], "Antihypertensives")
})

test_that("a model built from an abstract says so, and names the paper to obtain", {
  fromAbstract <- registry[registry$readiness == "implemented" &
                             registry$evidence_read == "abstract" &
                             registry$stanpumpr_drug %in% IMPLEMENTED, ]
  expect_setequal(fromAbstract$stanpumpr_drug,
                  c("labetalol", "diltiazem", "nitroglycerin"))
  expect_true(all(!is.na(fromAbstract$to_obtain) & nzchar(fromAbstract$to_obtain)))
  # and every row still waiting on a paper names it
  waiting <- registry[registry$readiness == "needs_full_text", ]
  expect_true(all(!is.na(waiting$to_obtain) & nzchar(waiting$to_obtain)))
  expect_true(all(grepl("PMID|doi|PMC|NDA", waiting$to_obtain)))
})

test_that("a drug with no suitable model gives the reason", {
  none <- registry[registry$readiness == "no_suitable_model", ]
  expect_setequal(none$drug, c("sodium nitroprusside", "vasopressin", "ephedrine",
                               "hydralazine", "adenosine", "sodium bicarbonate"))
  expect_true(all(nchar(none$blocking_issue) > 60))
})

test_that("cross-references to the antihypertensive registry resolve", {
  ah <- antihypertensiveRegistry()
  ids <- stats::na.omit(registry$antihypertensive_model_id)
  expect_gt(length(ids), 5)
  expect_true(all(ids %in% ah$model_id))
})

test_that("the parameter audit covers every implemented drug", {
  expect_setequal(unique(parameters$drug), IMPLEMENTED)
  for (col in c("drug", "parameter", "value", "units", "provenance",
                "evidence", "source_url"))
    expect_true(all(nzchar(parameters[[col]]) & !is.na(parameters[[col]])),
                info = col)
  expect_true(all(parameters$provenance %in% c("estimated", "fixed", "derived")))
  expect_true(all(parameters$evidence %in% EVIDENCE))
  # The audit's evidence agrees with the registry's for each drug
  for (d in IMPLEMENTED) {
    reg <- registry$evidence_read[registry$stanpumpr_drug %in% d]
    expect_true(all(parameters$evidence[parameters$drug == d] == reg), info = d)
  }
})

test_that("the audit's numbers are the ones the models use", {
  value <- function(d, p) as.numeric(parameters$value[parameters$drug == d &
                                                        parameters$parameter == p])
  expect_equal(value("epinephrine", "CL"), EPINEPHRINE_CL)
  expect_equal(value("epinephrine", "V"), EPINEPHRINE_V)
  expect_equal(value("magnesiumSulfate", "CL"), MAGNESIUM_CL)
  expect_equal(value("magnesiumSulfate", "Vc"), MAGNESIUM_VC)
  expect_equal(value("magnesiumSulfate", "Q"), MAGNESIUM_Q)
  expect_equal(value("magnesiumSulfate", "Vp"), MAGNESIUM_VP)
  expect_equal(value("labetalol", "CL") / 1000, LABETALOL_CL)
  expect_equal(value("labetalol", "V1 (Vdc)"), LABETALOL_V1)
  expect_equal(value("diltiazem", "CL") / 1000, DILTIAZEM_CL)
  expect_equal(value("diltiazem", "V1"), DILTIAZEM_V1)
  expect_equal(value("nitroglycerin", "CL"), NITROGLYCERIN_CL)
  expect_equal(value("nitroglycerin", "time constant"), NITROGLYCERIN_TAU)
  # The derived values, to their printed precision
  lab <- twoCompartmentFromCentralVolume(LABETALOL_CL, LABETALOL_V1,
                                         LABETALOL_THALF_DISTRIBUTION,
                                         LABETALOL_THALF_ELIMINATION)
  expect_equal(round(lab$v2, 3), value("labetalol", "V2"))
  expect_equal(round(lab$cl2, 4), value("labetalol", "Q"))
  dil <- twoCompartmentFromCentralVolume(DILTIAZEM_CL, DILTIAZEM_V1,
                                         DILTIAZEM_THALF_ALPHA,
                                         DILTIAZEM_THALF_BETA)
  expect_equal(round(dil$v2, 1), value("diltiazem", "V2"))
  expect_equal(round(dil$cl2, 3), value("diltiazem", "Q"))
  expect_equal(round(NITROGLYCERIN_CL * NITROGLYCERIN_TAU, 1),
               value("nitroglycerin", "V"))
})
