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
