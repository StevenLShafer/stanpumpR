# The startup drug menu (startup-drugs.R).  Drafted by Claude Code,
# 2026-10-07, at the request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

dd <- getDrugDefaultsGlobal()

test_that("every drug that can be dosed has a category the menu knows", {
  # Not offered: the metabolites with no units of their own, and the carrier gases
  # and ventilation, which the gas rules add themselves
  notOffered <- c("desmetramadol", "desethylamiodarone", "air", "oxygen", "ventilation")
  dosed <- setdiff(dd$Drug, notOffered)
  expect_true(all(lengths(dd$Units[match(dosed, dd$Drug)]) > 0))
  expect_true(all(dd$Category[match(dosed, dd$Drug)] %in% DRUG_CATEGORIES))
  expect_true(all(is.na(dd$Category[match(notOffered, dd$Drug)])))
  # and every category has a drug in it
  expect_setequal(stats::na.omit(dd$Category), DRUG_CATEGORIES)
})

test_that("the menu lists the categories in order, each sorted by name", {
  choices <- startupDrugChoices(dd)
  expect_identical(names(choices), DRUG_CATEGORIES)
  for (drugs in choices) {
    titles <- tolower(helpDrugTitle(drugs))
    expect_identical(titles, sort(titles))
  }
  # The non-opioid analgesics given by mouth (acetaminophen, ibuprofen,
  # diclofenac and ketorolac IV too); the oral opioids stay under Opioids
  expect_identical(choices[["Oral analgesics"]],
                   c("acetaminophen", "diclofenac", "gabapentin", "ibuprofen",
                     "ketorolac", "meloxicam", "pregabalin"))
  expect_identical(choices[["Hypnotics and sedatives"]],
                   c("alprazolam", "clonazepam", "dexmedetomidine", "diazepam", "etomidate",
                     "ketamine", "lorazepam", "midazolam", "propofol",
                     "remimazolam", "temazepam", "zolpidem"))
  expect_true(all(STARTUP_DRUGS_DEFAULT %in% unlist(choices)))
  expect_false(any(c("desmetramadol", "desethylamiodarone", "air", "oxygen", "ventilation") %in% unlist(choices)))
  # A library without the column (an old edited copy) offers nothing, quietly
  expect_identical(startupDrugChoices(dd[, names(dd) != "Category"]), list())
})

test_that("Start with the defaults gives the dose table the app used to open with", {
  expect_identical(startupDoseTable(STARTUP_DRUGS_DEFAULT, dd), doseTableInit)
  # in whatever order the boxes were ticked
  expect_identical(startupDoseTable(rev(STARTUP_DRUGS_DEFAULT), dd), doseTableInit)
})

test_that("nothing ticked gives the empty table the session starts with", {
  expect_identical(startupDoseTable(character(0), dd), doseTableBlank)
  expect_identical(startupDoseTable(NULL, dd), doseTableBlank)
  expect_identical(nrow(doseTableBlank), STARTUP_BLANK_ROWS)
  expect_true(all(unlist(doseTableBlank) == ""))
})

test_that("names the menu does not offer are dropped, and duplicates collapse", {
  dt <- startupDoseTable(c("cefazolin", "notADrug", "desmetramadol", "cefazolin",
                           "ventilation", "<script>"), dd)
  expect_identical(dt$Drug[dt$Drug != ""], "cefazolin")
})

test_that("each drug starts with one zero-dose row in its default units", {
  dt <- startupDoseTable(c("vancomycin", "oxycodone", "dexmedetomidine"), dd)
  dt <- dt[dt$Drug != "", ]
  expect_identical(dt$Drug, c("dexmedetomidine", "oxycodone", "vancomycin"))
  expect_identical(dt$Units, c("mcg/kg/hr", "mg PO", "mg"))
  expect_true(all(dt$Time == "0" & dt$Dose == "0"))
})

test_that("every drug on the menu, all ticked at once, is a valid dose table", {
  everything <- unlist(startupDrugChoices(dd), use.names = FALSE)
  dt <- startupDoseTable(everything, dd)
  expect_true(validateDoseTableInput(dt, dd))
  for (drug in everything) {
    offered <- unlist(dd$Units[dd$Drug == drug])
    expect_true(all(dt$Units[dt$Drug == drug] %in% offered), info = drug)
  }
  for (drug in names(STARTUP_UNITS)) {
    expect_true(all(STARTUP_UNITS[[drug]] %in% unlist(dd$Units[dd$Drug == drug])), info = drug)
  }
})

test_that("an inhaled agent brings an oxygen row with its flow left blank", {
  dt <- startupDoseTable(c("sevoflurane", "rocuronium"), dd)
  filled <- dt[dt$Drug != "", ]
  expect_identical(filled$Drug, c("rocuronium", "oxygen", "sevoflurane"))
  expect_identical(filled$Dose[filled$Drug == "oxygen"], "")
  expect_identical(filled$Units[filled$Drug == "oxygen"], "L/min")
  # The gas rules then add the ventilation, as for a gas typed in by hand
  ruled <- applyGasTableRules(dt, 70)
  expect_true("ventilation" %in% ruled$Drug)
  expect_true(validateDoseTableInput(ruled, dd))
})

test_that("with nitrous oxide the blank oxygen flow fills in at 21%", {
  dt <- applyGasTableRules(startupDoseTable("nitrousOxide", dd), 70)
  expect_identical(sum(dt$Drug == "oxygen"), 1L)
  expect_identical(dt$Dose[dt$Drug == "oxygen"], "")
  dt$Dose[dt$Drug == "nitrousOxide"] <- "4"
  dt <- applyGasTableRules(dt, 70)
  # 0.21 / 0.79 * 4 = 1.06, to 0.1 L/min
  expect_identical(dt$Dose[dt$Drug == "oxygen"], "1.1")
})

test_that("only a URL holding a dose table counts as a bookmark", {
  expect_false(isBookmarkRestore(NULL))
  # Shiny restores from any query string; ?debug=1 alone carries nothing
  debugOnly <- shiny:::RestoreContext$new("?debug=1")
  expect_true(debugOnly$active)
  expect_false(isBookmarkRestore(debugOnly$values))
  expect_true(opensOnStartupMenu(debugOnly$values))
  expect_true(opensOnStartupMenu(NULL))

  bookmark <- list(DT = as.list(doseTableInit))
  expect_true(isBookmarkRestore(bookmark))
  expect_false(opensOnStartupMenu(bookmark))

  # The URL is rewritten while the menu is open, with the empty table in it:
  # reloading then must still open on the menu
  empty <- list(DT = as.list(doseTableBlank))
  expect_true(isBookmarkRestore(empty))
  expect_true(opensOnStartupMenu(empty))
})

test_that("a real bookmark URL is recognised, through Shiny's own decoding", {
  qs <- paste0(
    "?_inputs_&age=50&_values_&DT=",
    utils::URLencode(as.character(jsonlite::toJSON(doseTableInit, dataframe = "columns")),
                     reserved = TRUE)
  )
  rc <- shiny:::RestoreContext$new(qs)
  expect_true(isBookmarkRestore(rc$values))
  expect_false(opensOnStartupMenu(rc$values))
})

test_that("the menu's inputs are kept out of the bookmark URL", {
  expect_true(all(startupDrugInputId(DRUG_CATEGORIES) %in% bookmarksToExclude))
  expect_true(all(c("startup_ok", "startup_tour", "show_intro_modal") %in% bookmarksToExclude))
})

test_that("the menu ticks the defaults and has no way out but Start", {
  html <- as.character(startupDrugModal(startupDrugChoices(dd)))
  for (category in DRUG_CATEGORIES) {
    expect_match(html, sprintf('id="%s"', startupDrugInputId(category)), fixed = TRUE)
  }
  ticked <- regmatches(html, gregexpr('value="[^"]+" checked', html))[[1]]
  expect_setequal(sub('value="([^"]+)" checked', "\\1", ticked), STARTUP_DRUGS_DEFAULT)
  expect_match(html, 'id="startup_ok"', fixed = TRUE)
  expect_match(html, 'data-bs-backdrop="static"', fixed = TRUE)
  expect_false(grepl("data-bs-dismiss", html, fixed = TRUE))
})
