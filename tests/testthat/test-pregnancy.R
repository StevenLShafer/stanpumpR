# The Patient Profile's Pregnant field (R/pregnancy.R): which models read it,
# what each applies, that the flag off changes nothing, and that it reaches
# every surface the other covariates reach.  The factors are from Ansari,
# Carvalho, Shafer and Flood, Anesth Analg 2016;122:786-804, as each drug's
# header documents.

pregnancyModels <- c("morphine", "remifentanil", "oxycodone", "methadone",
                     "dexamethasone", "cefazolin", "gentamicin", "vancomycin")
woman <- list(weight = 70, height = 165, age = 30, sex = SEX_FEMALE)
pkOf <- function(drug, pregnant = FALSE, ...) {
  getDrugPK(drug, woman$weight, woman$height, woman$age, woman$sex,
            pregnant = pregnant, ...)
}
setOf <- function(pk) pk$PK[[PK_EVENT_DEFAULT]]
libraryDrugs <- function() {
  drugs <- getDrugDefaultsGlobal()$Drug
  drugs[!isGasDrug(drugs)]
}

test_that("exactly the quantified models read the flag, and the registry agrees", {
  expect_setequal(pregnancyDrugs(), pregnancyModels)
  for (drug in libraryDrugs()) {
    takes <- "pregnant" %in% names(formals(get(drug, mode = "function")))
    expect_identical(takes, drug %in% pregnancyModels, info = drug)
  }
  f <- pregnancyFindings()
  expect_setequal(f$drug[f$group == "applied"], pregnancyModels)
  expect_true(all(f$drug %in% getDrugDefaultsGlobal()$Drug))
  expect_false(any(duplicated(f$drug)))
  expect_true(all(f$group %in% c("applied", "unquantified", "unchanged", "unknown")))
  expect_true(all(nzchar(f$applied[f$group == "applied"])))
  expect_true(all(!nzchar(f$applied[f$group != "applied"])))
  expect_true(all(nzchar(f$finding)))
})

test_that("the flag off is the default, and changes nothing in any model", {
  for (drug in libraryDrugs()) {
    expect_equal(getDrugPK(drug, 70, 165, 30, SEX_FEMALE, pregnant = FALSE),
                 getDrugPK(drug, 70, 165, 30, SEX_FEMALE), info = drug)
  }
  # NULL (a control that has not reported, a caller that predates the field)
  # is off too
  expect_equal(pkOf("morphine", NULL), pkOf("morphine"))
})

test_that("a model that does not read the flag is untouched by it", {
  # (Codeine is not in this list: its own disposition ignores the flag, but
  # the morphine it forms is resolved with it, so its metabolite coefficients
  # move.  That path is checked below through the simulation.)
  for (drug in c("fentanyl", "propofol", "midazolam", "buprenorphine", "bupivacaine", "ondansetron")) {
    expect_equal(pkOf(drug, TRUE)$PK, pkOf(drug, FALSE)$PK, info = drug)
    expect_no_match(pkOf(drug, TRUE)$reference, "Ansari", info = drug)
  }
  on  <- pkOf("codeine", TRUE)
  off <- pkOf("codeine", FALSE)
  for (p in c("v1", "v2", "v3", "cl1", "cl2", "cl3", "ka_PO", "bioavailability_PO")) {
    expect_equal(setOf(on)[[p]], setOf(off)[[p]], info = p)
  }
  expect_equal(on$metabolite, off$metabolite)
})

test_that("the flag is validated, and refused for a man", {
  expect_error(getDrugPK("morphine", 70, 170, 30, SEX_MALE, pregnant = TRUE), "requires sex")
  expect_error(pkOf("morphine", NA), "Invalid pregnant")
  expect_error(pkOf("morphine", "yes"), "Invalid pregnant")
  expect_error(pkOf("morphine", 1), "Invalid pregnant")
  expect_error(pkOf("morphine", c(TRUE, FALSE)), "Invalid pregnant")
  # the rule is enforced before any model runs, so a drug that ignores the
  # flag is refused too
  expect_error(getDrugPK("fentanyl", 70, 170, 30, SEX_MALE, pregnant = TRUE), "requires sex")
  expect_error(simulateDrugsWithCovariates(
    data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg"),
    data.frame(Time = numeric(0), Event = character(0)),
    70, 170, 30, SEX_MALE, 60, FALSE, pregnant = TRUE), "requires sex")
})

test_that("pregnancyPossible() is the rule that shows the control", {
  expect_true(pregnancyPossible(PREGNANCY_MIN_AGE, SEX_FEMALE))
  expect_true(pregnancyPossible(30, SEX_FEMALE))
  expect_true(pregnancyPossible(PREGNANCY_MAX_AGE - 1, SEX_FEMALE))
  expect_false(pregnancyPossible(PREGNANCY_MAX_AGE, SEX_FEMALE))
  expect_false(pregnancyPossible(PREGNANCY_MIN_AGE - 0.5, SEX_FEMALE))
  expect_false(pregnancyPossible(30, SEX_MALE))
  expect_false(pregnancyPossible(NA, SEX_FEMALE))
  expect_false(pregnancyPossible(NULL, SEX_FEMALE))
})

test_that("each model multiplies its elimination clearance by the documented factor", {
  # The factor each header states; the three antibiotics carry the GFR rise
  # through their own renal terms (gentamicin linear, cefazolin a power,
  # vancomycin linear with an intercept, checked below).
  factors <- c(
    morphine      = MORPHINE_PREGNANCY_CL_FACTOR,
    remifentanil  = REMIFENTANIL_PREGNANCY_CL_FACTOR,
    oxycodone     = OXYCODONE_PREGNANCY_CL_FACTOR,
    methadone     = METHADONE_PREGNANCY_CL_FACTOR,
    dexamethasone = DEXAMETHASONE_PREGNANCY_CL_FACTOR,
    cefazolin     = PREGNANCY_GFR_FACTOR^0.586,
    gentamicin    = PREGNANCY_GFR_FACTOR
  )
  expect_equal(unname(factors[c("morphine", "remifentanil", "oxycodone", "methadone", "dexamethasone")]),
               c(1.7, 2, 3.8 / 2.6, 24 / 14, 2))
  expect_equal(PREGNANCY_GFR_FACTOR, 1.5)
  for (drug in names(factors)) {
    on  <- setOf(pkOf(drug, TRUE))
    off <- setOf(pkOf(drug, FALSE))
    expect_equal(on$cl1 / off$cl1, unname(factors[[drug]]), tolerance = 1e-6, info = drug)
  }
  # Vancomycin: Thomson's term at 1.5 x the creatinine clearance it reads
  size <- pkSizeFactors(woman$weight, woman$height, woman$age, woman$sex, TRUE,
                        legacyVolume = woman$weight / 70, legacyClearance = 1)
  scr  <- max(adultEquivalentCreatinine(NULL, woman$age, woman$sex), 60 / 88.42)
  crcl <- creatinineClearanceCG(70 * size$volume, woman$age, woman$sex, scr)
  expect_equal(setOf(pkOf("vancomycin", TRUE))$cl1,
               2.99 * (1 + 0.0154 * (PREGNANCY_GFR_FACTOR * crcl - 66)) / 60)
  # Thomson's intercept is slightly negative (1 - 0.0154 x 66), so the ratio
  # is a little MORE than the GFR factor
  ratio <- setOf(pkOf("vancomycin", TRUE))$cl1 / setOf(pkOf("vancomycin", FALSE))$cl1
  expect_gt(ratio, PREGNANCY_GFR_FACTOR)
  expect_lt(ratio, 1.55)
})

test_that("nothing but the elimination clearance moves", {
  # Methadone is the exception: its racemic reduction is fitted again at the
  # new clearances, so its reduced volumes shift a little (checked next).
  for (drug in setdiff(pregnancyModels, "methadone")) {
    on  <- setOf(pkOf(drug, TRUE))
    off <- setOf(pkOf(drug, FALSE))
    for (p in c("v1", "v2", "v3", "cl2", "cl3")) {
      expect_equal(on[[p]], off[[p]], info = paste(drug, p))
    }
    for (p in grep("^(ka|bioavailability|tlag)_", names(off), value = TRUE)) {
      expect_equal(on[[p]], off[[p]], info = paste(drug, p))
    }
    # and the metabolite, where there is one (oxycodone's oxymorphone)
    expect_equal(pkOf(drug, TRUE)$metabolite, pkOf(drug, FALSE)$metabolite, info = drug)
  }
})

test_that("methadone's reduction keeps the racemic clearance exact and the volumes close", {
  on  <- setOf(pkOf("methadone", TRUE))
  off <- setOf(pkOf("methadone", FALSE))
  expect_equal(on$cl1 / off$cl1, METHADONE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  # the volumes are fitted, not scaled: within a few per cent of the unchanged ones
  expect_equal(on$v1, off$v1, tolerance = 0.05)
  expect_equal(on$v1 + on$v2 + on$v3, off$v1 + off$v2 + off$v3, tolerance = 0.05)
  # the enantiomers themselves are scaled exactly
  e1 <- methadoneEnantiomers(70, METHADONE_PREGNANCY_CL_FACTOR)
  e0 <- methadoneEnantiomers(70)
  for (s in c("R", "S")) {
    expect_equal(sum(e0[[s]]$coef / e0[[s]]$lambda) / sum(e1[[s]]$coef / e1[[s]]$lambda),
                 METHADONE_PREGNANCY_CL_FACTOR, tolerance = 1e-8, info = s)
  }
  # the two states are cached apart
  expect_false(identical(methadoneRacemicPK(70, 1), methadoneRacemicPK(70, METHADONE_PREGNANCY_CL_FACTOR)))
  expect_identical(methadoneRacemicPK(70, 1), methadoneRacemicPK(70))
})

test_that("the References panel names the review only while the flag is on", {
  for (drug in pregnancyModels) {
    expect_match(pkOf(drug, TRUE)$reference, "Ansari J, Carvalho B, Shafer SL, Flood P", fixed = TRUE, info = drug)
    expect_match(pkOf(drug, TRUE)$reference, "pregnancy:", fixed = TRUE, info = drug)
    expect_no_match(pkOf(drug, FALSE)$reference, "Ansari", info = drug)
  }
  expect_match(pregnancyReference("clearance x 2"), "^pregnancy: clearance x 2 \\(Ansari")
})

test_that("the Minto companion doubles its clearance with the library model", {
  row <- getDrugDefaults("remifentanil")
  on  <- alternativeModelEntry("remifentanil", 70, 165, 30, SEX_FEMALE, row, pregnant = TRUE)
  off <- alternativeModelEntry("remifentanil", 70, 165, 30, SEX_FEMALE, row)
  expect_equal(setOf(on)$cl1 / setOf(off)$cl1, REMIFENTANIL_PREGNANCY_CL_FACTOR)
  for (p in c("v1", "v2", "v3", "cl2", "cl3", "ke0")) expect_equal(setOf(on)[[p]], setOf(off)[[p]], info = p)
  expect_match(on$reference, "Ansari", fixed = TRUE)
  expect_no_match(off$reference, "Ansari")
  # Schnider's propofol does not read it
  rowP <- getDrugDefaults("propofol")
  expect_equal(alternativeModelEntry("propofol", 70, 165, 30, SEX_FEMALE, rowP, pregnant = TRUE)$PK,
               alternativeModelEntry("propofol", 70, 165, 30, SEX_FEMALE, rowP)$PK)
  expect_length(alternativeModelProblems(c("propofol", "remifentanil"), 70, 165, 30, SEX_FEMALE, pregnant = TRUE), 0)
})

test_that("recalculatePK() passes the flag to the library model and the companion", {
  drugDefaults <- getDrugDefaultsGlobal()
  DT <- data.frame(Drug = c("remifentanil", "fentanyl"), Time = 0, Dose = 100, Units = "mcg",
                   stringsAsFactors = FALSE)
  on  <- recalculatePK(NULL, drugDefaults, DT, 30, 70, 165, SEX_FEMALE, pregnant = TRUE, alternativeModels = TRUE)
  off <- recalculatePK(NULL, drugDefaults, DT, 30, 70, 165, SEX_FEMALE, alternativeModels = TRUE)
  expect_equal(setOf(on$remifentanil)$cl1 / setOf(off$remifentanil)$cl1, REMIFENTANIL_PREGNANCY_CL_FACTOR)
  minto <- alternativeModelName("remifentanil")
  expect_equal(setOf(on[[minto]])$cl1 / setOf(off[[minto]])$cl1, REMIFENTANIL_PREGNANCY_CL_FACTOR)
  expect_equal(setOf(on$fentanyl), setOf(off$fentanyl))
})

test_that("simulateDrugsWithCovariates() passes the flag, and the parent's flag reaches its metabolite", {
  ET <- data.frame(Time = numeric(0), Event = character(0))
  DT <- data.frame(Drug = "morphine", Time = 0, Dose = 10, Units = "mg", stringsAsFactors = FALSE)
  on  <- simulateDrugsWithCovariates(DT, ET, 70, 165, 30, SEX_FEMALE, 240, FALSE, pregnant = TRUE)
  off <- simulateDrugsWithCovariates(DT, ET, 70, 165, 30, SEX_FEMALE, 240, FALSE)
  # same volume, so the same first point; faster clearance, so lower later
  expect_equal(on$morphine$wide$Plasma[1], off$morphine$wide$Plasma[1])
  expect_lt(tail(on$morphine$wide$Plasma, 1), 0.9 * tail(off$morphine$wide$Plasma, 1))
  # Codeine does not read the flag, but the morphine it forms is morphine's
  # model, resolved inside getDrugPK() with the parent's flag
  DT <- data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg PO", stringsAsFactors = FALSE)
  on  <- simulateDrugsWithCovariates(DT, ET, 70, 165, 30, SEX_FEMALE, 480, FALSE, pregnant = TRUE)
  off <- simulateDrugsWithCovariates(DT, ET, 70, 165, 30, SEX_FEMALE, 480, FALSE)
  expect_equal(on$codeine$wide$Plasma, off$codeine$wide$Plasma)
  expect_lt(tail(on$morphine$wide$Plasma, 1), tail(off$morphine$wide$Plasma, 1))
})

test_that("the help's pregnancy table is built from the models", {
  md <- helpPregnancyTableMarkdown()
  f <- pregnancyFindings()
  for (drug in f$drug) {
    expect_match(md, sprintf("[%s](help:drugs/%s)", helpDrugTitle(drug), drug), fixed = TRUE, info = drug)
  }
  # the applied rows carry the measured ratios: morphine 1.7 and V1 1
  expect_match(md, "[Morphine](help:drugs/morphine) |", fixed = TRUE)
  morphineRow <- grep("help:drugs/morphine", strsplit(md, "\n")[[1]], value = TRUE, fixed = TRUE)
  expect_match(morphineRow, "\\| 1\\.7 \\| 1 \\|$")
  gentamicinRow <- grep("help:drugs/gentamicin", strsplit(md, "\n")[[1]], value = TRUE, fixed = TRUE)
  expect_match(gentamicinRow, "\\| 1\\.5 \\| 1 \\|$")
  expect_match(md, "**Adjusted when Pregnant is ticked**", fixed = TRUE)
  expect_match(md, "**Unknown: not adjusted**", fixed = TRUE)
  # and the page renders through the marker
  html <- helpPageHTML("models/pregnancy")
  expect_match(html, "data-help-page=\"drugs/vancomycin\"", fixed = TRUE)
  expect_match(html, "data-help-scenario=\"remifentanil-labour\"", fixed = TRUE)
  expect_no_match(html, "generated: pregnancy-table", fixed = TRUE)
})

test_that("the slide workbook's covariates say whether the patient was pregnant", {
  # generateEmail() builds the sheet inline; the row it writes is pinned
  # here by name so a reordering of the sheet is caught.
  src <- paste(deparse(generateEmail), collapse = "\n")
  expect_match(src, "\"Pregnant\"", fixed = TRUE)
  expect_match(src, "isTRUE(values$pregnant)", fixed = TRUE)
})
