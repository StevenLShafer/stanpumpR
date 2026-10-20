# The vasoactive and resuscitation drug registry and its parameter audit.  See
# data-raw/vasoactiveRegistry.R (which builds the two CSVs) and R/vasoactive.R.
# Drafted by Claude Code, 2026-10-18.

registry   <- vasoactiveRegistry()
parameters <- vasoactiveParameters()

READINESS <- c("implemented", "needs_full_text", "complete_not_implemented",
               "no_suitable_model")
EVIDENCE  <- c("full_text", "abstract", "none")

# The drugs implemented new for this registry, in its own category.
IMPLEMENTED <- c("epinephrine", "magnesiumSulfate", "labetalol", "diltiazem",
                 "nitroglycerin", "norepinephrine", "procainamide", "esmolol",
                 "dopamine", "atropine", "propranolol", "digoxin",
                 "verapamil", "phenylephrine", "milrinone", "milrinonePediatric",
                 "metoprolol", "nicardipine", "digoxinIV", "digoxinIVPediatric")

# Every drug that was asked for, by its name in the registry
REQUESTED <- c("clevidipine", "sodium nitroprusside", "nitroglycerin",
               "epinephrine", "norepinephrine", "vasopressin", "ephedrine",
               "phenylephrine", "hydralazine", "nicardipine", "dopamine",
               "methylene blue", "atropine", "adenosine", "sodium bicarbonate",
               "magnesium sulfate", "procainamide", "verapamil", "diltiazem",
               "propranolol", "metoprolol", "esmolol", "labetalol",
               # asked for on 2026-10-19
               "digoxin", "milrinone")

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
    # No band, except the published therapeutic ranges of digoxin and
    # milrinone
    if (!d %in% c("digoxin", "digoxinIV", "digoxinIVPediatric", "milrinone",
                   "milrinonePediatric")) {
      expect_equal(row$Lower, 0, info = d)
      expect_equal(row$Upper, 0, info = d)
    }
    # Plasma only, except where a source measured the time of peak effect
    PK <- getDrugPK(d, 70, 170, 50, "male")
    expected <- switch(d, milrinone = MILRINONE_TPEAK, atropine = ATROPINE_TPEAK,
                       digoxinIV = DIGOXIN_IV_TPEAK,
                       norepinephrine = NOREPINEPHRINE_TPEAK,
                       diltiazem = DILTIAZEM_TPEAK,
                       nicardipine = NICARDIPINE_TPEAK, 0)
    expect_equal(PK$tPeak, expected, info = d)
  }
  # Clevidipine was already in the library, in the Antihypertensives category,
  # and stays there
  expect_identical(dd$Category[dd$Drug == "clevidipine"], "Antihypertensives")
})

test_that("a model built from an abstract says so, and names the paper to obtain", {
  fromAbstract <- registry[registry$readiness == "implemented" &
                             registry$evidence_read == "abstract" &
                             registry$stanpumpr_drug %in% IMPLEMENTED, ]
  expect_setequal(fromAbstract$stanpumpr_drug, c("labetalol", "diltiazem"))
  expect_true(all(!is.na(fromAbstract$to_obtain) & nzchar(fromAbstract$to_obtain)))
  # and every row still waiting on a paper names it
  waiting <- registry[registry$readiness == "needs_full_text", ]
  expect_true(all(!is.na(waiting$to_obtain) & nzchar(waiting$to_obtain)))
  expect_true(all(grepl("PMID|doi|PMC|NDA", waiting$to_obtain)))
})

test_that("a complete model that is not offered is a second source for an offered drug", {
  alt <- registry[registry$readiness == "complete_not_implemented", ]
  expect_gt(nrow(alt), 0)
  expect_true(all(alt$evidence_read == "full_text"))
  offered <- unique(registry$drug[registry$readiness == "implemented"])
  expect_true(all(alt$drug %in% offered))
})

test_that("a drug with no suitable model gives the reason", {
  none <- registry[registry$readiness == "no_suitable_model", ]
  expect_true(all(nchar(none$blocking_issue) > 60))
  # The drugs with no usable source at all: nothing implemented, nothing
  # waiting on a paper
  hopeless <- setdiff(unique(registry$drug),
                      registry$drug[registry$readiness %in%
                                      c("implemented", "needs_full_text",
                                        "complete_not_implemented")])
  expect_setequal(hopeless, c("sodium nitroprusside", "vasopressin", "ephedrine",
                              "hydralazine", "adenosine", "sodium bicarbonate"))
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
  expect_equal(value("norepinephrine", "CL"), NOREPINEPHRINE_CL)
  expect_equal(value("norepinephrine", "Vc"), NOREPINEPHRINE_VC)
  expect_equal(value("norepinephrine", "Q"), NOREPINEPHRINE_Q)
  expect_equal(value("norepinephrine", "Vp"), NOREPINEPHRINE_VP)
  expect_equal(value("norepinephrine", "Prod"), NOREPINEPHRINE_PROD)
  expect_equal(value("procainamide", "CL_R"), PROCAINAMIDE_CLR)
  expect_equal(value("procainamide", "Vd"), PROCAINAMIDE_VD)
  expect_equal(value("esmolol", "CL") / 1000, ESMOLOL_CL)
  expect_equal(value("esmolol", "Vc"), ESMOLOL_V1)
  expect_equal(value("dopamine", "k10"), DOPAMINE_K10)
  expect_equal(value("dopamine", "V1"), DOPAMINE_V1)
  expect_equal(value("atropine", "V_P"), ATROPINE_VP)
  expect_equal(value("atropine", "CL") / 1000, ATROPINE_CL)
  esm <- twoCompartmentFromCentralVolume(ESMOLOL_CL, ESMOLOL_V1,
                                         ESMOLOL_THALF_ALPHA, ESMOLOL_THALF_BETA)
  expect_equal(round(esm$v2, 3), value("esmolol", "V2"))
  expect_equal(round(esm$cl2, 4), value("esmolol", "Q"))
  expect_equal(round(ATROPINE_VP * ATROPINE_KPT / ATROPINE_KTP, 1), value("atropine", "V2"))
  prop <- threeCompartmentFromTriexponential(PROPRANOLOL_COEF / 1000 / PROPRANOLOL_DOSE,
                                             PROPRANOLOL_EXP)
  expect_equal(round(prop$v1, 2), value("propranolol", "V1"))
  expect_equal(round(prop$cl1, 3), value("propranolol", "CL"))
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
  expect_equal(value("milrinone", "V1"), MILRINONE_V1)
  expect_equal(value("milrinone", "V3"), MILRINONE_V3)
  expect_equal(value("milrinone", "CL") / 1000, MILRINONE_CL1)
  expect_equal(value("milrinone", "Q2") / 1000, MILRINONE_CL2)
  expect_equal(value("milrinone", "tPeak"), MILRINONE_TPEAK)
  expect_equal(value("milrinonePediatric", "CL"), MILRINONE_PED_CL)
  expect_equal(value("milrinonePediatric", "V"), MILRINONE_PED_V)
  expect_equal(value("milrinonePediatric", "CrCl exponent"),
               MILRINONE_PED_CRCL_EXPONENT)
  expect_equal(value("metoprolol", "CL"), METOPROLOL_CL)
  expect_equal(value("metoprolol", "V1"), METOPROLOL_V1)
  expect_equal(value("metoprolol", "F"), METOPROLOL_F)
  expect_equal(value("metoprolol", "CYP2D6 poor CL factor"),
               METOPROLOL_CYP2D6_CL[["poor"]])
  expect_equal(value("nicardipine", "CL"), NICARDIPINE_CL)
  expect_equal(value("nicardipine", "Q"), NICARDIPINE_Q)
  expect_equal(value("nicardipine", "Vss"), NICARDIPINE_VSS)
  expect_equal(value("digoxinIV", "CL slope"), DIGOXIN_IV_CL_SLOPE)
  expect_equal(value("digoxinIV", "Q"), DIGOXIN_IV_Q)
  expect_equal(value("digoxinIV", "tPeak"), DIGOXIN_IV_TPEAK)
  expect_equal(value("digoxinIVPediatric", "CL"), DIGOXIN_IV_PED_CL)
  expect_equal(value("digoxinIVPediatric", "SCr coefficient"), DIGOXIN_IV_PED_SCR)
})
