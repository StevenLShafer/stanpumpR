# The therapeutic reference range registry (R/therapeuticRanges.R): every
# antiseizure, antidepressant and antipsychotic drug, and every metabolite of
# one, says what its range is and whether the band draws it, and the band
# columns of drugDefaults_global.csv agree with it.

dd <- getDrugDefaultsGlobal()
tr <- therapeuticRanges()

# The concentration unit each row's limits must be in
rangeUnits <- c(mcg = "mg/L", ng = "ng/mL")

test_that("every drug in the covered categories, and its metabolites, has a row", {
  covered <- dd$Drug[!is.na(dd$Category) & dd$Category %in% THERAPEUTIC_RANGE_CATEGORIES]
  metabolites <- unlist(lapply(covered, function(d) {
    model <- get(d, envir = asNamespace("stanpumpR"))
    model(weight = 70, height = 170, age = 40, sex = "male")$metabolite$name
  }))
  expect_true(length(metabolites) > 0)
  missing <- setdiff(c(covered, metabolites), tr$drug)
  expect_identical(missing, character(0), info = paste(missing, collapse = ", "))
})

test_that("each row is well formed", {
  expect_false(anyDuplicated(tr$drug) > 0)
  expect_true(all(tr$drug %in% dd$Drug))
  expect_true(all(tr$band_status %in% THERAPEUTIC_RANGE_STATUSES))
  expect_true(all(nzchar(tr$source)))
  expect_true(all(nzchar(tr$analyte)))
  for (i in seq_len(nrow(tr))) {
    r <- tr[i, ]
    unit <- dd$Concentration.Units[dd$Drug == r$drug]
    expect_identical(r$units, unname(rangeUnits[unit]), info = r$drug)
    if (r$band_status %in% c("drawn", "drawn_on_metabolite", "sum_not_drawn")) {
      expect_true(r$lower > 0 && r$upper > r$lower, info = r$drug)
    }
    # An alert level, where the guideline gives one, is at or above the range
    if (!is.na(r$alert_level)) expect_gte(r$alert_level, r$upper, label = r$drug)
    # A range that is not drawn says why
    if (!r$band_status %in% c("drawn", "drawn_on_metabolite"))
      expect_true(nzchar(r$notes) || nzchar(r$source), info = r$drug)
  }
})

test_that("the band columns are the registry's range, or zero when it is not drawn", {
  for (i in seq_len(nrow(tr))) {
    r <- tr[i, ]
    row <- dd[dd$Drug == r$drug, ]
    band <- c(row$Lower, row$Upper)
    if (r$band_status == "drawn") {
      expect_equal(band, c(r$lower, r$upper), info = r$drug)
      expect_equal(row$Typical, r$typical, info = r$drug)
      expect_true(row$Typical >= row$Lower && row$Typical <= row$Upper, info = r$drug)
    } else if (r$band_status == "drawn_on_metabolite") {
      expect_equal(band, c(0, 0), info = r$drug)
      target <- tr[tr$drug == r$band_drug, ]
      expect_identical(target$band_status, "drawn", info = r$drug)
      expect_equal(c(target$lower, target$upper), c(r$lower, r$upper), info = r$drug)
    } else {
      expect_equal(c(band, row$Typical), c(0, 0, 0), info = r$drug)
    }
  }
})

test_that("the drug's help page states the range and its source", {
  page <- helpDrugPageHTML("aripiprazole")
  expect_match(page, "Therapeutic reference range", fixed = TRUE)
  expect_match(page, "120 to 270 ng/mL", fixed = TRUE)
  expect_match(page, "Hart 2026", fixed = TRUE)
  expect_match(page, "laboratory alert level 540 ng/mL", fixed = TRUE)
  expect_match(helpDrugPageHTML("risperidone"), "so not drawn on either row", fixed = TRUE)
  expect_match(helpDrugPageHTML("droperidol"), "None established", fixed = TRUE)
})
