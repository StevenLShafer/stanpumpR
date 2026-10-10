# Help pages generated from the drug library.  See R/help-drugs.R.

drugDefaults <- getDrugDefaultsGlobal()
ivDrugs <- drugDefaults$Drug[!isGasDrug(drugDefaults$Drug)]
gasDrugs <- drugDefaults$Drug[isGasDrug(drugDefaults$Drug)]

test_that("every drug in the library has a narrative", {
  for (drug in drugDefaults$Drug) {
    expect_true(helpHasMarkdown(paste0("drugs/", drug)),
                info = paste("missing inst/help/drugs/", drug, ".md: every drug needs one (see docs/adding-a-drug.md)"))
  }
})

test_that("every narrative belongs to a drug in the library", {
  files <- sub("^drugs/", "", grep("^drugs/", helpMarkdownIds(), value = TRUE))
  expect_true(all(files %in% drugDefaults$Drug), info = paste("orphan:", setdiff(files, drugDefaults$Drug)))
})

# Drugs with no effect site of their own.  codeine, tramadol and prednisone
# are prodrugs whose effect is the metabolite's; the antibiotics, the other
# steroids, sugammadex and glycopyrrolate are plasma-only by design, because
# there is no equilibration model to attach (see each drug's header);
# mannitol is plotted as serum osmolality and has no published ke0
# (R/drugs_mannitol.R); gabapentin has no estimated human equilibration delay
# yet (GABAPENTIN_TPEAK in R/drugs_gabapentin.R); amiodarone and
# desethylamiodarone are both active, with no published human ke0 for the
# antiarrhythmic effect, and so is amiodaroneIV (2026-10-08).  Called
# `prodrugs` until amiodarone, an active parent, joined it (2026-10-07).
# Clonazepam's, zolpidem's and temazepam's published effects are direct
# functions of plasma concentration (R/drugs_clonazepam.R, R/drugs_zolpidem.R,
# R/drugs_temazepam.R) (2026-10-09).
plasmaOnly <- c(
  "codeine", "tramadol", "prednisone",
  "cefazolin", "clindamycin", "cefalexin", "ceftriaxone", "vancomycin",
  "metronidazole", "gentamicin",
  "hydrocortisone", "methylprednisolone", "dexamethasone", "prednisolone",
  "sugammadex", "glycopyrrolate", "mannitol", "gabapentin",
  "amiodarone", "desethylamiodarone", "amiodaroneIV",
  # The antidepressants: response lags weeks, no ke0 exists
  "escitalopram", "citalopram", "sertraline", "paroxetine", "duloxetine",
  "mirtazapine", "bupropion", "hydroxybupropion", "fluoxetine", "norfluoxetine",
  "clonazepam", "zolpidem", "temazepam",
  "methylphenidate", "lisdexamfetamine", "mixedAmphetamineSalts",
  # no equilibration model for antiemesis (2026-10-10)
  "ondansetron", "aprepitant", "fosaprepitant",
  # systemic plasma concentration, not the nerve block (2026-10-10)
  "bupivacaine", "ropivacaine", "mepivacaine",
  # NSAIDs with no published ke0 (2026-10-10)
  "diclofenac", "meloxicam", "ketorolac",
  # diamorphine (2026-10-10): opt-in illicit research model, parent plasma only
  "diamorphine"
)

test_that("the parameter table evaluates every intravenous model at the reference patients", {
  patients <- helpReferencePatients()
  expect_equal(nrow(patients), 6)
  for (drug in ivDrugs) {
    tab <- helpDrugParameterTable(drug, drugDefaults, patients)
    expect_false(is.null(tab), info = drug)
    expect_equal(nrow(tab), nrow(patients), info = drug)
    expect_true(all(is.finite(tab$V1) & tab$V1 > 0), info = drug)
    expect_true(all(is.finite(tab$CL1) & tab$CL1 > 0), info = drug)
    expect_true(all(is.finite(tab$halfLife1)), info = drug)
    expect_true(all(nzchar(tab$reference)), info = drug)
    if (drug %in% plasmaOnly) {
      expect_true(all(tab$ke0 == 0), info = drug)
      expect_true(all(tab$tPeak == 0), info = drug)
    } else {
      expect_true(all(is.finite(tab$ke0) & tab$ke0 > 0), info = drug)
      # desmetramadol supplies ke0 directly and has no tPeak of its own
      expect_true(all(tab$tPeak > 0 | tab$ke0Supplied), info = drug)
    }
    expect_true(all(tab$tPeakRoute %in% TPEAK_ROUTES), info = drug)
  }
  # one-compartment models report the absent compartments as absent, not as 1 L
  cod <- helpDrugParameterTable("codeine", drugDefaults, patients)
  expect_true(all(is.na(cod$V2) & is.na(cod$V3)))
  expect_true(all(is.na(cod$halfLife2)))
  expect_equal(unique(cod$metabolite), "morphine")
  hyd <- helpDrugParameterTable("hydrocodone", drugDefaults, patients)
  expect_equal(unique(hyd$tPeakRoute), ROUTE_PO)
  des <- helpDrugParameterTable("desmetramadol", drugDefaults, patients)
  expect_true(all(des$ke0Supplied))
})

test_that("a supplied ke0 is explained by the parent only for a drug never dosed", {
  # Desmetramadol has no units and gets its ke0 from its parent's dose;
  # acetaminophen, ibuprofen and alprazolam are dosed directly and supply a
  # published equilibration rate.
  expect_match(helpDrugPageHTML("desmetramadol"), "never dosed directly", fixed = TRUE)
  for (drug in c("acetaminophen", "ibuprofen", "alprazolam")) {
    page <- helpDrugPageHTML(drug)
    expect_match(page, "reports the equilibration rate itself", fixed = TRUE, info = drug)
    expect_false(grepl("never dosed directly", page, fixed = TRUE), info = drug)
  }
})

test_that("the fat-free-mass switch is reported from the code", {
  expect_true(helpDrugRespondsToFFM("morphine"))
  expect_false(helpDrugRespondsToFFM("propofol"))
  expect_false(helpDrugRespondsToFFM("hydrocodone"))
  html <- helpDrugPageHTML("morphine")
  expect_match(html, "Adjust weight to fat-free mass", fixed = TRUE)
  expect_match(html, 'data-help-page="models/fat-free-mass"', fixed = TRUE)
  expect_match(helpDrugPageHTML("propofol"), "does not change this model", fixed = TRUE)
})

test_that("prodrugs, metabolites and oral-only drugs are described from the code", {
  expect_setequal(helpParentDrugs("morphine"), "codeine")
  expect_setequal(helpParentDrugs("hydromorphone"), "hydrocodone")
  expect_setequal(helpParentDrugs("oxymorphone"), "oxycodone")
  expect_setequal(helpParentDrugs("desmetramadol"), "tramadol")
  expect_length(helpParentDrugs("propofol"), 0)

  cod <- helpDrugPageHTML("codeine")
  expect_match(cod, "no effect site of its own", fixed = TRUE)
  expect_match(cod, "Active metabolite", fixed = TRUE)
  expect_match(cod, 'data-help-page="drugs/morphine"', fixed = TRUE)
  expect_match(cod, "Effect of CYP2D6 phenotype", fixed = TRUE)
  expect_match(cod, "Ultrarapid", fixed = TRUE)
  expect_match(cod, "none: no effect site", fixed = TRUE)

  mor <- helpDrugPageHTML("morphine")
  expect_match(mor, "Formed as a metabolite", fixed = TRUE)
  expect_match(mor, 'data-help-page="drugs/codeine"', fixed = TRUE)

  hyd <- helpDrugPageHTML("hydrocodone")
  expect_match(hyd, "Oral only", fixed = TRUE)
  expect_match(hyd, "after an oral dose", fixed = TRUE)

  des <- helpDrugPageHTML("desmetramadol")
  expect_match(des, "cannot be entered in the dose table", fixed = TRUE)
  expect_match(des, "ke0 supplied by the model", fixed = TRUE)
  expect_match(des, "appears only as the active metabolite of Tramadol", fixed = TRUE)

  oxy <- helpDrugPageHTML("oxymorphone")
  expect_match(oxy, 'data-help-page="drugs/oxycodone"', fixed = TRUE)

  prop <- helpDrugPageHTML("propofol")
  expect_match(prop, "Plasma target and Effect site target", fixed = TRUE)
  expect_false(grepl("Active metabolite</h2>", prop, fixed = TRUE))
  expect_false(grepl("Formed as a metabolite", prop, fixed = TRUE))
})

test_that("an active parent with no effect site is not described as a prodrug", {
  # Amiodarone forms an active metabolite and has no effect site, which the
  # page would otherwise read as a prodrug; its model says prodrug = FALSE.
  # (Claude Code, 2026-10-07.)
  expect_setequal(helpParentDrugs("desethylamiodarone"), "amiodarone")
  amio <- helpDrugPageHTML("amiodarone")
  expect_match(amio, "Oral only", fixed = TRUE)
  expect_match(amio, "mg/day PO", fixed = TRUE)
  expect_match(amio, "Active metabolite", fixed = TRUE)
  expect_match(amio, 'data-help-page="drugs/desethylamiodarone"', fixed = TRUE)
  expect_match(amio, "This model has <strong>no effect site</strong>", fixed = TRUE)
  expect_match(amio, "Not an opioid", fixed = TRUE)
  expect_match(amio, "timed on the plasma (no effect site in the model)", fixed = TRUE)
  expect_false(grepl("no effect site of its own", amio, fixed = TRUE))
  expect_false(grepl("the effect is the metabolite", amio, fixed = TRUE))
  # The prodrugs are unchanged
  expect_match(helpDrugPageHTML("prednisone"), "no effect site of its own", fixed = TRUE)

  dea <- helpDrugPageHTML("desethylamiodarone")
  expect_match(dea, "appears only as the active metabolite of Amiodarone", fixed = TRUE)
  expect_match(dea, "cannot be entered in the dose table", fixed = TRUE)
  expect_match(dea, "None: no range applies to this model", fixed = TRUE)
  expect_false(grepl("0 to 0", dea, fixed = TRUE))
  # A drug with no dosing unit has no default unit either (not "NA")
  expect_false(grepl("<td>NA</td>", dea, fixed = TRUE))
  expect_match(helpDrugIndexHTML(drugDefaults), "<td>none</td>", fixed = TRUE)
})

test_that("half-lives of a day or more are also given in days", {
  expect_equal(helpFormatHalfLife(c(10, 1439, 1440, 79717.909, NA)),
               c("10", "1,440", "1,440 (1 d)", "79,700 (55.4 d)", "—"))
  # Amiodarone's terminal half-lives, 55.4 days (parent) and 60.4 days
  # (desethylamiodarone), at the reference adult
  expect_match(helpDrugPageHTML("amiodarone"), "(55.4 d)", fixed = TRUE)
  expect_match(helpDrugPageHTML("desethylamiodarone"), "(60.4 d)", fixed = TRUE)
})

test_that("the table shows the covariate switches", {
  dex <- helpDrugParameterTable("dexmedetomidine")
  expect_equal(length(unique(dex$reference)), 2)
  expect_match(dex$events[dex$Patient == "Infant"], "CPBStart")
  expect_equal(dex$events[dex$Patient == "Reference adult"], PK_EVENT_DEFAULT)

  remi <- helpDrugParameterTable("remifentanil")
  obese <- remi[remi$Patient == "Obese adult", ]
  adult <- remi[remi$Patient == "Reference adult", ]
  expect_false(isTRUE(all.equal(obese$V1 / 120, adult$V1 / 70)))

  # Alfentanil has no covariates of its own: its parameters are identical at
  # every patient with the fat-free-mass switch off. (With it on, they vary by
  # size like every scaled model.)
  alfOff <- helpDrugParameterTable("alfentanil", adjustToFFM = FALSE)
  expect_equal(length(unique(alfOff$V1)), 1)
  alfOn <- helpDrugParameterTable("alfentanil", adjustToFFM = TRUE)
  expect_true(length(unique(alfOn$V1)) > 1)
})

test_that("an unknown drug gives a message, not an error", {
  expect_null(helpDrugParameterTable("notadrug"))
  expect_match(helpDrugPageHTML("notadrug"), "no drug called")
})

test_that("every intravenous drug page carries its citation, units and tables", {
  for (drug in ivDrugs) {
    html <- helpDrugPageHTML(drug, drugDefaults)
    row <- drugDefaults[drugDefaults$Drug == drug, ]
    pk <- helpDrugPK(drug, helpReferencePatients()[1, ], drugDefaults)
    expect_match(html, htmltools::htmlEscape(helpReferenceShort(pk$reference)), fixed = TRUE, info = drug)
    # desmetramadol has no dosing unit at all (it appears only as a metabolite)
    if (!is.na(row$Default.Units) && nzchar(row$Default.Units)) {
      expect_match(html, row$Default.Units, fixed = TRUE, info = drug)
    }
    expect_match(html, "Volumes and clearances", fixed = TRUE, info = drug)
    expect_match(html, "About this model", fixed = TRUE, info = drug)
    expect_match(html, 'data-help-page="drugs/index"', fixed = TRUE, info = drug)
  }
})

test_that("the propofol page reads from the code, not from a hand-written table", {
  html <- helpDrugPageHTML("propofol")
  expect_match(html, "Eleveld DJ et al., Br J Anaesth 2018", fixed = TRUE)
  expect_match(html, "pubmed.ncbi.nlm.nih.gov/29661412", fixed = TRUE)
  expect_match(html, "mcg/mL", fixed = TRUE)
  expect_match(html, "this model has covariates", fixed = TRUE)
  alf <- helpDrugPageHTML("alfentanil")
  expect_match(alf, "no covariates of its own", fixed = TRUE)
})

test_that("extravascular routes and events are described where the model has them", {
  oxy <- helpDrugPageHTML("oxycodone")
  expect_match(oxy, "Extravascular routes", fixed = TRUE)
  expect_match(oxy, "Oral (PO)", fixed = TRUE)
  hydro <- helpDrugPageHTML("hydromorphone")
  expect_match(hydro, "Intranasal (IN)", fixed = TRUE)
  prop <- helpDrugPageHTML("propofol")
  expect_false(grepl("Extravascular routes", prop, fixed = TRUE))

  dex <- helpDrugPageHTML("dexmedetomidine")
  expect_match(dex, "Events that change the kinetics", fixed = TRUE)
  expect_match(dex, "CPBStart", fixed = TRUE)
  expect_false(grepl("Events that change the kinetics", prop, fixed = TRUE))
})

test_that("gas pages carry Gas Man's parameters and the MAC table", {
  for (gas in gasDrugs) {
    html <- helpDrugPageHTML(gas, drugDefaults)
    expect_match(html, "At a glance", fixed = TRUE, info = gas)
    expect_match(html, 'data-help-page="inhaled-agents"', fixed = TRUE, info = gas)
  }
  sevo <- helpDrugPageHTML("sevoflurane")
  expect_match(sevo, "Physical properties", fixed = TRUE)
  expect_match(sevo, "MAC and age", fixed = TRUE)
  expect_match(sevo, helpFormatNumber(macForAge(2.1, 80)), fixed = TRUE)
  air <- helpDrugPageHTML("air")
  expect_false(grepl("MAC and age", air, fixed = TRUE))
  n2o <- helpDrugPageHTML("nitrousOxide")
  expect_match(n2o, ">110<", fixed = TRUE)
})

test_that("the drug index links every drug", {
  html <- helpDrugIndexHTML(drugDefaults)
  for (drug in drugDefaults$Drug) {
    expect_match(html, sprintf('data-help-page="drugs/%s"', drug), fixed = TRUE, info = drug)
  }
  expect_match(html, "Eleveld DJ et al.", fixed = TRUE)
  expect_match(html, "Anesthetic agent", fixed = TRUE)
  expect_match(html, "Ventilator setting", fixed = TRUE)
  expect_match(html, "metabolite only", fixed = TRUE)
  expect_match(html, "IV, TCI", fixed = TRUE)
})

test_that("the bibliography lists every model citation once, with its drugs", {
  html <- helpReferencesHTML(drugDefaults)
  refs <- unique(unlist(lapply(ivDrugs, function(d) helpDrugParameterTable(d, drugDefaults)$reference)))
  for (r in refs) {
    short <- htmltools::htmlEscape(helpReferenceShort(r))
    expect_equal(lengths(regmatches(html, gregexpr(short, html, fixed = TRUE))), 1, info = r)
  }
  expect_match(html, 'data-help-page="drugs/fentanyl"', fixed = TRUE)
  expect_match(html, 'data-help-page="drugs/alfentanil"', fixed = TRUE)
  expect_match(html, "Sheiner LB", fixed = TRUE)
})

test_that("drug pages reflect a library edited in the session", {
  edited <- drugDefaults
  edited$Lower[edited$Drug == "propofol"] <- 1.25
  edited$Upper[edited$Drug == "propofol"] <- 9
  html <- helpDrugPageHTML("propofol", edited)
  expect_match(html, "1.25 to 9 mcg/mL", fixed = TRUE)
})
