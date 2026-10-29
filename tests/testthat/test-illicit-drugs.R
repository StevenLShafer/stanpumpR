# Illicit (drug-of-abuse) opt-in and the red dose-table display
# (R/illicit-drugs.R, R/createHOT.R).  Drafted by Claude Code, 2026-10-10, at
# the request of Steven L. Shafer.

dd <- getDrugDefaultsGlobal()

test_that("the illicit category is known and carries at least diamorphine", {
  expect_true(ILLICIT_DRUG_CATEGORY %in% DRUG_CATEGORIES)
  expect_true("diamorphine" %in% illicitDrugNames(dd))
})

test_that("isIllicitDrug() flags the illicit drugs and nothing else", {
  expect_true(isIllicitDrug("diamorphine", dd))
  expect_false(isIllicitDrug("propofol", dd))
  expect_false(isIllicitDrug("fentanyl", dd))
  # Unknown drug and a library without the column are both all-FALSE
  expect_false(isIllicitDrug("notADrug", dd))
  expect_identical(isIllicitDrug(c("propofol", "diamorphine"), dd), c(FALSE, TRUE))
  expect_equal(isIllicitDrug("diamorphine", dd[, names(dd) != "Category"]), FALSE)
})

test_that("the opt-in gates which drugs are offered", {
  offeredOff <- visibleDrugNames(FALSE, dd)
  offeredOn  <- visibleDrugNames(TRUE, dd)
  expect_false("diamorphine" %in% offeredOff)
  expect_true("diamorphine" %in% offeredOn)
  # Nothing else is hidden; the opt-in adds exactly the illicit drugs
  expect_setequal(setdiff(offeredOn, offeredOff), illicitDrugNames(dd))
  expect_true(all(c("propofol", "fentanyl") %in% offeredOff))
})

test_that("the dose-table autocomplete source honours the opt-in", {
  drugCol <- which(names(doseTableInit) == "Drug")
  off <- createHOT(doseTableInit, dd, drugChoices = visibleDrugNames(FALSE, dd))
  on  <- createHOT(doseTableInit, dd, drugChoices = visibleDrugNames(TRUE, dd))
  expect_false("diamorphine" %in% off$x$columns[[drugCol]]$source)
  expect_true("diamorphine" %in% on$x$columns[[drugCol]]$source)
  # With no drugChoices given, every drug is offered (back-compatible)
  expect_true("diamorphine" %in% createHOT(doseTableInit, dd)$x$columns[[drugCol]]$source)
  # The Drug column renderer colours illicit names red
  expect_true(grepl("_illicitDrugSet", off$x$columns[[drugCol]]$renderer))
})

test_that("a metabolite that is only formed is never offered for dosing", {
  # Adversarial review, 2026-10-27: enalaprilat (no dose units) was offered in
  # the dose-table autocomplete and the Add a dose dialog, which then had no
  # unit to give it.  Every metabolite-only row is the same case.
  formedOnly <- dd$Drug[!isDosableDrug(dd$Drug, dd)]
  expect_true(all(c("enalaprilat", "salicylate", "norfluoxetine") %in% formedOnly))
  expect_true(all(is.na(dd$Category[dd$Drug %in% formedOnly])))
  for (showIllicit in c(FALSE, TRUE)) {
    offered <- visibleDrugNames(showIllicit, dd)
    expect_false(any(formedOnly %in% offered))
    drugCol <- which(names(doseTableInit) == "Drug")
    source <- createHOT(doseTableInit, dd, drugChoices = offered)$x$columns[[drugCol]]$source
    expect_false("enalaprilat" %in% source)
  }
  # The startup menu never listed them (no Category)
  expect_false(any(formedOnly %in% unlist(startupDrugChoices(dd))))
  # The unexpanded library (Units as one string) gives the same answer
  ddRaw <- getDrugDefaultsGlobal(FALSE)
  expect_identical(isDosableDrug(ddRaw$Drug, ddRaw), isDosableDrug(dd$Drug, dd))
  expect_false(isDosableDrug("notADrug", dd))
})

test_that("a click on a formed metabolite's panel goes to the drug forming it", {
  drugs <- list(enalapril = list(metaboliteName = "enalaprilat"),
                propofol  = list())
  expect_identical(formedMetaboliteParent("enalaprilat", drugs, dd), "enalapril")
  # A dosable drug is never redirected, even when something forms it
  expect_identical(formedMetaboliteParent("propofol", drugs, dd), "propofol")
  # Nothing forms it: unchanged
  expect_identical(formedMetaboliteParent("salicylate", drugs, dd), "salicylate")
  # And the real parent names its metabolite
  pk <- getDrugPK("enalapril", 70, 175, 40, "male")
  expect_identical(pk$metaboliteName, "enalaprilat")
})

# -----------------------------------------------------------------------------
# The generated pharmacokinetic table on inst/help/models/illicit-drugs.md.
# These seven models differ more from one another than the rest of the library
# does, so the models page carries one table comparing them, built from the
# models rather than typed, so that it cannot drift from them.
# Claude Code, 2026-10-17, at the request of Steven L. Shafer.
# -----------------------------------------------------------------------------

test_that("the illicit PK table covers every illicit drug, with its real numbers", {
  dd <- getDrugDefaultsGlobal(FALSE)
  md <- helpIllicitPKTableMarkdown(dd)
  for (drug in illicitDrugNames(dd))
    expect_true(grepl(paste0("[", drug, "](help:drugs/", drug, ")"), md, fixed = TRUE),
                info = drug)
  # one header, one rule and one row per drug
  expect_equal(length(strsplit(md, "\n", fixed = TRUE)[[1]]),
               length(illicitDrugNames(dd)) + 2)
  # The saturable drug is described as saturable and not given a fixed
  # half-life or a clearance it does not have.
  ghb <- grep("GHB", strsplit(md, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_true(grepl("saturable", ghb, fixed = TRUE))
  expect_true(grepl("Vmax", ghb, fixed = TRUE))
  expect_true(grepl("Two compartments", ghb, fixed = TRUE))
  expect_false(grepl("CL ", ghb, fixed = TRUE))
  # A linear drug gets a clearance and a half-life instead
  lsd <- grep("LSD", strsplit(md, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_true(grepl("CL ", lsd, fixed = TRUE))
  expect_true(grepl("min", lsd, fixed = TRUE))
  # Routes come from the units, not from a hand-written list: the intranasal
  # drugs must not read as having no absorption (a units-column list parsed
  # as text made every route look intravenous).
  for (drug in c("cocaine", "methamphetamine", "diamorphine")) {
    row <- grep(paste0("\\[", drug, "\\]"), strsplit(md, "\n", fixed = TRUE)[[1]], value = TRUE)
    expect_true(grepl("IN", row, fixed = TRUE), info = drug)
    expect_true(grepl("ka ", row, fixed = TRUE), info = drug)
  }
})

test_that("the models page carries the generated table", {
  html <- helpPageHTML("models/illicit-drugs")
  expect_true(grepl("Vmax", as.character(html), fixed = TRUE))
  expect_false(grepl("generated:", as.character(html), fixed = TRUE))
})

test_that("the PK table knows every extravascular route the library has", {
  # A route missing from the table's absorption lookup reads as no absorption
  # rather than failing, so the lookup is checked against DOSE_ROUTES itself:
  # adding a route to the library (SC arrived with enoxaparin) must not leave
  # a silently blank column behind.
  # SC and RA share the tissue depot, whose rate is ka_RA (routeKaField()).
  set <- getDrugPK("GHB", 70, 170, 40, "male")$PK[[PK_EVENT_DEFAULT]]
  for (r in setdiff(DOSE_ROUTES, ROUTE_IV))
    expect_true(routeKaField(r) %in% names(set),
                info = paste(r, "has no ka_ field in the PK set"))
  expect_equal(routeKaField(ROUTE_SC), "ka_RA")
})
