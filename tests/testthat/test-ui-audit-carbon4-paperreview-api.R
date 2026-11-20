# The Single Paper Review and Simulation API findings of Carbon's (Gemini)
# fourth adversarial UI audit, of PR #730, 2026-11-15, each reproduced
# before it was fixed: an upload replaced by one that was refused (H5), a
# covariate equation that made ka or F unusable (M4), two regimens of one id
# (M5), a metabolite dosed in no unit through the API (H6), and the CSV
# export's formula guard (L2).  Network-free: no review is sent.  Drafted by
# Claude Code, 2026-11-15, at the request of Steven L. Shafer.

# --- H5: an upload replaced by one that is not kept -------------------------

# A file as Shiny writes an upload, and the input value it sends.
upload4 <- function(bytes, name = "x.pdf") {
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(bytes, path)
  list(name = name, size = length(bytes), type = "application/pdf", datapath = path)
}
pdf4 <- function(text = "paper", extra = 0)
  c(charToRaw(paste0("%PDF-1.7\n", text, "\n")), raw(extra))

# Run the event loop until the review's promise settles and its observers run.
settle4 <- function(session) {
  for (i in 1:200) {
    later::run_now(0.01)
    session$flushReact()
    if (later::loop_empty()) break
  }
}

# The review's call, replaced: it records what it was sent and refuses, so
# nothing leaves the process.
mockSend4 <- function(env) {
  testthat::local_mocked_bindings(
    paperReviewApiKey = function(sessionKey = NULL, ...) sessionKey %or% "operator-key",
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ...) {
      env$calls <- env$calls + 1
      env$pdf <- pdf
      env$supplement <- supplement
      promises::promise_reject(paperReviewError("http_529", "The API is overloaded (529)."))
    },
    paperReviewUsageReport = function(record, ...) invisible(record),
    .env = parent.frame())
}

test_that("H5: a manuscript replaced by a file that is not a PDF is dropped, and Review sends nothing", {
  sent <- new.env(); sent$calls <- 0
  mockSend4(sent)
  shiny::testServer(paperReviewServer, {
    session$setInputs(consent = TRUE)
    session$setInputs(manuscript = upload4(pdf4("paper1")))
    expect_true(hasPdf())
    second <- upload4(charToRaw("not a pdf at all"), "notes.txt")
    session$setInputs(manuscript = second)
    expect_false(file.exists(second$datapath))
    expect_false(hasPdf())
    expect_false(exists("pdf", envir = store))
    expect_match(output$status, "is not a PDF and was discarded, as was the manuscript held before it")
    session$setInputs(review = 1)
    settle4(session)
    expect_equal(sent$calls, 0)
    expect_match(output$status, "Upload a manuscript first")
  })
})

test_that("H5: a supplement replaced by a file that is not a PDF is not sent with the manuscript", {
  sent <- new.env(); sent$calls <- 0
  mockSend4(sent)
  shiny::testServer(paperReviewServer, {
    session$setInputs(consent = TRUE)
    session$setInputs(supplement = upload4(pdf4("supplement1")))
    expect_true(exists("supplement", envir = store))
    session$setInputs(supplement = upload4(charToRaw("not a pdf"), "s.docx"))
    expect_false(exists("supplement", envir = store))
    expect_match(output$status, "as was the supplement held before it")
    session$setInputs(manuscript = upload4(pdf4("paper2")))
    session$setInputs(review = 1)
    settle4(session)
    expect_equal(sent$calls, 1)
    expect_null(sent$supplement)
    expect_true(grepl("paper2", rawToChar(sent$pdf[1:20]), fixed = TRUE))
  })
})

test_that("H5: a manuscript over 20 MB is refused on upload, unread, and a held supplement is kept", {
  sent <- new.env(); sent$calls <- 0
  mockSend4(sent)
  shiny::testServer(paperReviewServer, {
    session$setInputs(consent = TRUE)
    session$setInputs(supplement = upload4(pdf4("supplement", 1000)))
    big <- upload4(pdf4("big", round(20.5 * 1024^2)))
    session$setInputs(manuscript = big)
    expect_false(file.exists(big$datapath))
    expect_false(hasPdf())
    expect_false(exists("pdf", envir = store))
    expect_true(exists("supplement", envir = store))       # not wiped with it
    expect_match(output$status, "The manuscript \\(20.5 MB\\) was discarded")
    expect_match(output$status, "more than the 20 MB a review can send")
    session$setInputs(review = 1)
    settle4(session)
    expect_equal(sent$calls, 0)
    expect_true(exists("supplement", envir = store))
  })
  shiny::testServer(paperReviewServer, {
    session$setInputs(consent = TRUE)
    session$setInputs(manuscript = upload4(pdf4("big", round(20.5 * 1024^2))))
    expect_false(hasPdf())
    expect_match(output$status, "a review can send at most 20 MB")
  })
})

test_that("H5: two files that together exceed 20 MB: the second is refused and the first kept", {
  # Each passed on upload and the pair failed at Review, which discarded both.
  sent <- new.env(); sent$calls <- 0
  mockSend4(sent)
  shiny::testServer(paperReviewServer, {
    session$setInputs(consent = TRUE)
    session$setInputs(supplement = upload4(pdf4("supplement", 12 * 1024^2)))
    session$setInputs(manuscript = upload4(pdf4("paper", 12 * 1024^2)))
    expect_false(hasPdf())
    expect_true(exists("supplement", envir = store))
    expect_match(output$status, "with the supplement held \\(12.0 MB\\) the files come to more than")
    # A manuscript that fits beside it is taken, and both are sent.
    session$setInputs(manuscript = upload4(pdf4("paper", 7 * 1024^2)))
    expect_true(hasPdf())
    session$setInputs(review = 1)
    settle4(session)
    expect_equal(sent$calls, 1)
    expect_false(is.null(sent$supplement))
    expect_lte(length(sent$pdf) + length(sent$supplement), PAPER_REVIEW_MAX_PDF_BYTES)
  })
})

test_that("H5: choosing a new file drops what its field held before the file arrives", {
  # Shiny refuses a file over PAPER_REVIEW_MAX_UPLOAD_BYTES in the browser,
  # and the app never hears of it, so the field's change says it.
  shiny::testServer(paperReviewServer, {
    session$setInputs(consent = TRUE)
    session$setInputs(manuscript = upload4(pdf4("paper1")), supplement = upload4(pdf4("supp1")))
    expect_true(hasPdf())
    session$setInputs(manuscriptChosen = 1)
    expect_false(hasPdf())
    expect_false(exists("pdf", envir = store))
    expect_true(exists("supplement", envir = store))
    expect_match(output$status, "Another manuscript was chosen, so the one held before has been discarded")
    session$setInputs(supplementChosen = 1)
    expect_false(exists("supplement", envir = store))
    # A file that then arrives and is refused says what was dropped,
    session$setInputs(manuscript = upload4(charToRaw("not a pdf")))
    expect_match(output$status, "is not a PDF and was discarded, as was the manuscript held before it")
    # and one that is a PDF is taken as usual.
    session$setInputs(manuscript = upload4(pdf4("paper2")))
    expect_true(hasPdf())
    # After Discard nothing is said to have been held.
    session$setInputs(manuscriptChosen = 2)
    session$setInputs(discard = 1)
    # (a file chosen after Discard: one still arriving from before it is
    # dropped unread, test-paper-review-privacy.R)
    session$setInputs(manuscriptChosen = 3)
    session$setInputs(manuscript = upload4(charToRaw("not a pdf")))
    expect_match(output$status, "^The manuscript is not a PDF and was discarded\\.$")
  })
})

test_that("H5: both upload fields report a chosen file, by its field and never its name", {
  scriptOf <- function(html) regmatches(html, regexpr("jQuery\\(document\\)\\.on\\('change'[^<]*", html))
  check <- function(html, ns) {
    script <- scriptOf(html)
    expect_length(script, 1)
    expect_true(grepl(paste0("#", ns("manuscript"), ", #", ns("supplement")), script, fixed = TRUE))
    expect_true(grepl("this.id + 'Chosen'", script, fixed = TRUE))
    expect_false(grepl("name", script, fixed = TRUE))
  }
  # The review app on its own
  check(as.character(htmltools::renderTags(paperReviewUI())$html), shiny::NS(NULL))
  # and the tab of stanpumpR (app_ui() uses numericInput(updateOn = ), shiny 1.10)
  skip_if_not_installed("shiny", "1.10.0")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  check(as.character(htmltools::renderTags(app_ui()(list()))$html), shiny::NS(PAPER_REVIEW_MODULE_ID))
})

test_that("H5: an upload over the limit is deleted unread", {
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  bytes <- pdf4("x", 5000)
  writeBin(bytes, path)
  read <- paperReviewReadUpload(path, maxBytes = 1000)
  expect_length(read, 0)
  expect_equal(attr(read, "oversize"), length(bytes))
  expect_false(file.exists(path))
  expect_false(dir.exists(dir))
  # within the limit it is read as before
  dir.create(dir)
  writeBin(bytes, path)
  expect_identical(paperReviewReadUpload(path, maxBytes = length(bytes)), bytes)
})

# --- M4: a covariate equation that leaves ka or F unusable ------------------

# The example paper given an oral route, ka and F, and an oral regimen.
oralPaper4 <- function() {
  ex <- paperReviewExample()
  ex$structure$routes <- list("iv_bolus", "oral")
  ex$parameters <- c(ex$parameters, list(
    list(name_in_paper = "ka", role = "ka", route = "oral", value = 1.2, value_text = "1.2",
         units = "1/h", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2"),
    list(name_in_paper = "F", role = "F", route = "oral", value = 50, value_text = "50",
         units = "percent", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2")))
  ex$regimens <- c(ex$regimens, list(list(
    id = "R3", description = "100 mg by mouth",
    doses = list(list(route = "oral", time = 0, amount = 100, mass_unit = "mg", per_kg = "no",
                      per_time = "none", duration = NULL, time_units = "min")),
    location = "Methods")))
  ex
}
effect4 <- function(role, covariate, form, coefficient, reference, printed)
  list(parameter_role = role, covariate = covariate, form = form, coefficient = coefficient,
       reference_value = reference, covariate_units = "", equation_as_printed = printed,
       location = "Equation 3")
patient4 <- function(age = 40, weight = 70) list(weight = weight, height = 170, age = age, sex = "male")
drugAt4 <- function(review, ex, patient)
  paperReviewQuietly(function()
    paperReviewDrug(ex, patient, review$identifiability$closed,
                    absorption = review$absorptions$R3, absorptions = review$absorptions))

test_that("M4: a covariate equation that makes F negative for a patient refuses that patient", {
  ex <- oralPaper4()
  ex$covariate_effects <- list(effect4("F", "age", "linear", -0.03, 40, "1 - 0.03 (AGE - 40)"))
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  expect_true(isTRUE(review$absorptions$R3$ok))
  # At 40 the equation is 1, and the model is built.
  d40 <- drugAt4(review, ex, patient4(40))
  expect_true(d40$ok)
  expect_equal(d40$model$PK$default$bioavailability_PO, 0.5)
  # At 90 it gives -0.5, F -0.25: the curve was negative (-154 ng/mL).
  d90 <- drugAt4(review, ex, patient4(90))
  expect_false(d90$ok)
  expect_true(any(grepl("The absorption is not finite and positive for this patient", d90$problems)))
  expect_true(any(grepl("F ~ age (1 - 0.03 (AGE - 40)) gives -0.5", d90$problems, fixed = TRUE)))
  # and the Simulator says so rather than draw it
  testthat::local_mocked_bindings(paperReviewExample = function() ex)
  shiny::testServer(paperReviewServer, {
    session$setInputs(weight = 70, height = 170, age = 40, sex = "male", maximum = 600)
    session$setInputs(example = 1)
    session$setInputs(regimen = "R3", loadRegimen = 1)
    expect_true(min(simulation()$sim$curve$Cp) >= 0)
    session$setInputs(age = 90)
    expect_error(simulation(), "absorption is not finite and positive")
  })
})

test_that("M4: a negative ka, or a volume of 0, for a patient refuses that patient and names the equation", {
  ex <- oralPaper4()
  ex$covariate_effects <- list(effect4("ka", "age", "linear", -0.03, 40, "1 - 0.03 (AGE - 40)"))
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  d <- drugAt4(review, ex, patient4(90))
  expect_false(d$ok)
  expect_true(any(grepl("ka ~ age", d$problems, fixed = TRUE)))
  # (age / 40)^0.5 on V2 is 0 at age 0, which passed as a volume of 0
  ex <- oralPaper4()
  ex$covariate_effects <- list(effect4("V2", "age", "power", 0.5, 40, "(AGE/40)^0.5"))
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  expect_true(review$drug$ok)
  d0 <- paperReviewQuietly(function() paperReviewDrug(ex, patient4(0), review$identifiability$closed))
  expect_false(d0$ok)
  expect_true(any(grepl("V2 ~ age ((AGE/40)^0.5) gives 0", d0$problems, fixed = TRUE)))
})

test_that("M4: a power with a reference of 0 is not applied, and does not become the reference patient", {
  # (AGE/0)^0.5 is infinite: ka Inf stopped getDrugPK() with "missing value
  # where TRUE/FALSE needed", and the reference patient was 0 years old.
  ex <- oralPaper4()
  ex$covariate_effects <- list(effect4("ka", "age", "power", 0.5, 0, "(AGE/0)^0.5"))
  m <- paperReviewCovariateMultiplier(ex$covariate_effects[[1]], patient4(40), patient4(40))
  expect_false(m$applied)
  expect_equal(m$multiplier, 1)
  expect_match(m$reason, "needs a positive reference value")
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  expect_false(identical(review$reference$age, 0))
  expect_true(review$drug$ok)
  d <- drugAt4(review, ex, patient4(40))
  expect_true(d$ok)
  expect_equal(d$model$PK$default$ka_PO, 1.2 / 60)
  sim <- paperReviewSimulate(d, patient4(40), data.frame(Time = 0, Dose = 100, Units = "mg PO"), 600)
  expect_true(all(is.finite(sim$curve$Cp)) && max(sim$curve$Cp) > 0)
  titles <- vapply(review$findings, `[[`, "", "title")
  expect_true("Not applied: ka ~ age (power)" %in% titles)
  expect_false(any(grepl("Finite and positive for every legal patient: ka ~ age", titles, fixed = TRUE)))

  # A weight of 0 as the reference made the reference patient weigh 0 kg,
  # and no model could be built for anyone.
  ex <- paperReviewExample()
  ex$covariate_effects <- list(effect4("CL", "weight", "power", 0.75, 0, "(WT/0)^0.75"))
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  expect_true(review$reference$weight >= MIN_WEIGHT)
  expect_true(review$drug$ok)
})

# --- M5: two regimens of one id ---------------------------------------------

armPaper4 <- function() {
  arm <- function(amount, fraction)
    list(id = "Arm A", description = paste(amount, "mg by mouth"),
         doses = list(list(route = "oral", time = 0, amount = amount, mass_unit = "mg", per_kg = "no",
                           per_time = "none", duration = NULL, time_units = "min",
                           analyte_fraction = fraction)),
         location = "Methods")
  ex <- oralPaper4()
  ex$regimens[[3]] <- NULL
  ex$regimens <- c(ex$regimens, list(arm(100, 1), arm(200, 0.5)))
  ex
}

test_that("M5: two regimens of one id are told apart, each with its own absorption", {
  ex <- armPaper4()
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  ids <- vapply(review$extraction$regimens, function(r) r$id, "")
  expect_false(anyDuplicated(ids) > 0)
  expect_true(all(c("Arm A", "Arm A (2)") %in% ids))
  expect_match(review$extraction$extraction_limitations,
               "the next \"Arm A\" is called \"Arm A (2)\"", fixed = TRUE)
  # Each keeps its own dose basis; the first was given the second's 0.5.
  expect_equal(review$absorptions[["Arm A"]]$fraction, 1)
  expect_equal(review$absorptions[["Arm A (2)"]]$fraction, 0.5)
  first <- paperReviewRegimen(review$extraction, "Arm A")
  second <- paperReviewRegimen(review$extraction, "Arm A (2)")
  expect_equal(first$doses[[1]]$amount, 100)
  expect_equal(second$doses[[1]]$amount, 200)
  d1 <- paperReviewDrugFor(review$drug, review$extraction, first, review$reference)
  expect_equal(d1$model$PK$default$bioavailability_PO, 0.5)
  # Three of one name, and a name the renaming would take, stay distinct.
  three <- armPaper4()
  three$regimens <- c(three$regimens, list(three$regimens[[3]]))
  three$regimens[[1]]$id <- "Arm A (2)"
  ids <- vapply(paperReviewNameRegimens(three)$regimens, function(r) r$id, "")
  expect_false(anyDuplicated(ids) > 0)
})

test_that("M5: the Simulator loads the regimen chosen, not the first of its name", {
  ex <- armPaper4()
  testthat::local_mocked_bindings(paperReviewExample = function() ex)
  shiny::testServer(paperReviewServer, {
    session$setInputs(example = 1)
    session$setInputs(regimen = "Arm A (2)")
    session$setInputs(loadRegimen = 1)
    expect_equal(doseTable()$Dose, 200)
    expect_equal(simAbsorption()$fraction, 0.5)
    session$setInputs(regimen = "Arm A")
    session$setInputs(loadRegimen = 2)
    expect_equal(doseTable()$Dose, 100)
    expect_equal(simAbsorption()$fraction, 1)
  })
})

# --- H6: a metabolite dosed in no unit through the API ----------------------

apiJson4 <- function(doses)
  paste0('{"patient":{"weight":70,"height":170,"age":50,"sex":"male"},"duration":60,"doses":', doses, '}')
# The 400 a request with these doses (JSON) gets, read as plumber reads it
apiRefusal4 <- function(doses) {
  err <- tryCatch(apiSimulate(apiParseRequest(apiJson4(doses))), stanpumpr_api_error = function(e) e)
  expect_s3_class(err, "stanpumpr_api_error")
  expect_equal(err$status, 400L)
  conditionMessage(err)
}

test_that("H6: a metabolite-only drug cannot be dosed, in no unit or any other", {
  dd <- getDrugDefaultsGlobal()
  formed <- dd$Drug[vapply(dd$Units, function(u) !any(!is.na(u) & nzchar(u)), logical(1))]
  expect_true(all(c("desmetramadol", "enalaprilat", "salicylate") %in% formed))
  for (drug in formed) {
    # "units": null is read as NA, and NA %in% NA let each be simulated (200)
    for (units in c("null", '"mg"', '""')) {
      msg <- apiRefusal4(sprintf('[{"drug":"%s","time":0,"dose":10,"units":%s}]', drug, units))
      expect_match(msg, "is formed as a metabolite of another drug and cannot be dosed itself")
    }
  }
  # Missing from one row of several, and as a column
  expect_match(apiRefusal4(paste0('[{"drug":"enalaprilat","time":0,"dose":10},',
                                  '{"drug":"fentanyl","time":0,"dose":100,"units":"mcg"}]')),
               "row 1: enalaprilat is formed as a metabolite")
  expect_match(apiRefusal4('{"drug":["salicylate"],"time":[0],"dose":[10],"units":[null]}'),
               "salicylate is formed as a metabolite")
})

test_that("H6: a dose with no unit is refused as missing one", {
  for (units in c("null", '""', '"  "'))
    expect_match(apiRefusal4(sprintf('[{"drug":"fentanyl","time":0,"dose":100,"units":%s}]', units)),
                 "row 1: `units` is missing. The units of fentanyl are: mcg")
  expect_match(apiRefusal4(paste0('[{"drug":"fentanyl","time":0,"dose":100,"units":"mcg"},',
                                  '{"drug":"propofol","time":0,"dose":2}]')),
               "row 2: `units` is missing")
  # and an ordinary request still runs
  r <- apiSimulate(apiParseRequest(apiJson4('[{"drug":"fentanyl","time":0,"dose":100,"units":"mcg"}]')))
  expect_equal(r$doses$Units, "mcg")
})

# --- L2: the CSV export's formula guard -------------------------------------

test_that("L2: a text cell that would read as a formula after leading space is quoted", {
  cells <- c(" =1+1", "  +2", " -2", " @x", "\n=cmd", "\t1", "\rabc", "\nplain",
             "=1+1", "-2", "@x", "+1",
             " plain", "plain", "1.5", "a=b", "")
  quoted <- c(rep(TRUE, 12), rep(FALSE, 5))
  # The cell as written: the CSV after its header line
  written <- function(x) sub("^\"Text\"\n", "", apiCsv(data.frame(Text = x, stringsAsFactors = FALSE)))
  for (i in seq_along(cells))
    expect_identical(startsWith(written(cells[i]), "\"'"), quoted[i], label = encodeString(cells[i]))
  # The apostrophe is all that is added
  expect_identical(written(" =1+1"), "\"' =1+1\"\n")
  # Numbers are left alone, -2 included
  expect_identical(sub("^\"Number\"\n", "", apiCsv(data.frame(Number = -2))), "-2\n")
})
