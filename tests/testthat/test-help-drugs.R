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

test_that("the parameter table evaluates every intravenous model at the reference patients", {
  patients <- helpReferencePatients()
  expect_equal(nrow(patients), 6)
  for (drug in ivDrugs) {
    tab <- helpDrugParameterTable(drug, drugDefaults, patients)
    expect_false(is.null(tab), info = drug)
    expect_equal(nrow(tab), nrow(patients), info = drug)
    expect_true(all(is.finite(tab$V1) & tab$V1 > 0), info = drug)
    expect_true(all(is.finite(tab$CL1) & tab$CL1 > 0), info = drug)
    expect_true(all(is.finite(tab$ke0) & tab$ke0 > 0), info = drug)
    expect_true(all(is.finite(tab$halfLife1)), info = drug)
    expect_true(all(nzchar(tab$reference)), info = drug)
    expect_true(all(tab$tPeak > 0), info = drug)
  }
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

  alf <- helpDrugParameterTable("alfentanil")
  expect_equal(length(unique(alf$V1)), 1)
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
    expect_match(html, row$Default.Units, fixed = TRUE, info = drug)
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
  expect_match(alf, "no covariates", fixed = TRUE)
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
