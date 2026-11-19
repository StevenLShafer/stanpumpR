# Carbon's (Gemini's) second repository audit, of 7e9b4f48, 2026-11-19: its
# regression check of PR #767, which addressed the first.  One test per
# finding that reproduced, each failing on the code before its fix.  The IDs
# are the audit's (A2-F1 to A2-F27).

test_that("each of fluoxetine's cohort patients receives the published clearance (A2-F1)", {
  # Han 2025's cohort: 59 kg, 17 years; 171.8 cm men, 159.7 cm women.  CL/F is
  # a women's intercept (2.91 L/h) with a men's factor (1.165), so with the
  # switch on each sex is anchored at its own typical patient.
  clh <- function(height, sex) fluoxetine(59, height, 17, sex, TRUE)$PK$default$cl1 * 60
  expect_equal(clh(159.7, "female"), 2.91, tolerance = 1e-12)
  expect_equal(clh(171.8, "male"), 2.91 * 1.165, tolerance = 1e-12)
  # V/F has no sex term and stays anchored at the pooled patient
  pooled <- cohortSizeFactors(groups = FLUOXETINE_COHORT$groups)
  expect_equal(fluoxetine(59, 159.7, 17, "female", TRUE)$PK$default$v1,
               24.9 * pkSizeFactors(59, 159.7, 17, "female")$volume / pooled$volume)
  # the switch off is the published model, unchanged
  expect_equal(fluoxetine(59, 159.7, 17, "female", FALSE)$PK$default$cl1 * 60, 2.91)
  expect_equal(fluoxetine(59, 171.8, 17, "male", FALSE)$PK$default$cl1 * 60, 2.91 * 1.165)
})

test_that("each of etoposide's cohort patients receives the published central volume (A2-F1)", {
  # Krogh-Madsen 2012: 79.4 kg, 170.1 cm, 52.3 years; V1 9.13 L in men,
  # x (1 - 0.2793) in women
  v1 <- function(sex) etoposideKroghMadsen(79.4, 170.1, 52.3, sex, TRUE)$PK$default$v1
  expect_equal(v1("male"), 9.13, tolerance = 1e-12)
  expect_equal(v1("female"), 9.13 * (1 - 0.2793), tolerance = 1e-12)
})

test_that("cohortSexSizeFactors() takes only the patient's own sex", {
  cohort <- list(weight = 80, height = 170, age = 50, men = 3, women = 7)
  expect_equal(cohortSexSizeFactors(cohort, SEX_MALE),
               cohortSizeFactors(80, 50, 1, 0, height = 170))
  expect_equal(cohortSexSizeFactors(cohort, SEX_FEMALE),
               cohortSizeFactors(80, 50, 0, 1, height = 170))
  # a cohort of men only still anchors a woman, at a woman of its demographics
  expect_equal(cohortSexSizeFactors(modifyList(cohort, list(women = 0)), SEX_FEMALE),
               cohortSizeFactors(80, 50, 0, 1, height = 170))
})

test_that("the local anaesthetics' absorption rate follows the switch (A2-F2)", {
  # Each rate was fitted with the disposition its switch position gives:
  # anchored (on) or published (off).  The anchored rate with the switch off
  # put the calibration peaks 22% and 14% low.
  expect_equal(bupivacaine(70, 170, 40, "male", TRUE)$PK$default$ka_RA, 0.011958)
  expect_equal(bupivacaine(70, 170, 40, "male", FALSE)$PK$default$ka_RA, 0.0187)
  expect_equal(ropivacaine(70, 170, 40, "male", TRUE)$PK$default$ka_RA, 0.004749)
  expect_equal(ropivacaine(70, 170, 40, "male", FALSE)$PK$default$ka_RA, 0.00612)
})

test_that("a Procedure start with am or pm is read as such, and trailing text is refused (A2-F7)", {
  expect_equal(hourMinute(c("2:30 pm", "2:30 PM", "2:30pm", "2:30 p.m.")), rep(14 * 60 + 30, 4))
  expect_equal(hourMinute(c("12:30 am", "12:30 pm", "9:00 am")), c(30, 12 * 60 + 30, 9 * 60))
  expect_true(all(is.na(hourMinute(c("12:30abc", "14:30 pm", "0:30 am", "8:75", "25:00", "")))))
  expect_false(isValidReferenceTime("12:30abc"))
  expect_true(isValidReferenceTime("2:30 pm"))
  # the forms read before are read as before, and seconds are still refused
  expect_equal(hourMinute(c("08:30", "8:30", "0830", "8.30", "08h30", "24:00", "23:59", "8:5")),
               c(510, 510, 510, 510, 510, 0, 1439, 485))
  expect_true(is.na(hourMinute("8:30:15")))
  # an afternoon start: a 15:00 dose is 30 minutes in, not 12 h 30 min
  expect_equal(clockTimeToDelta("2:30 pm", "15:00"), 30)
})

test_that("the clock-time converters take non-finite times and an NA reference (A2-F8)", {
  expect_identical(deltaToClockTime("08:00", c(0, Inf, -Inf, NaN, NA, 59.6)),
                   c("08:00", NA, NA, NA, NA, "09:00"))
  expect_identical(deltaToClockTime(NA_character_, c(10, 20)), c(NA_character_, NA_character_))
  expect_identical(clockTimeToDelta(NA_character_, "09:00"), NA_real_)
})

test_that("an edited comment sent before the browser unticks the box is not sent (A2-F15)", {
  calls <- 0
  local_mocked_bindings(sendSlide = function(...) { calls <<- calls + 1; TRUE })
  local_mocked_bindings(outputComments = function(...) {})
  local_mocked_bindings(shinyalert = function(...) NULL, .package = "shinyalert")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig, add = TRUE)
  dt <- doseTableInit[1, ]
  dt[1, ] <- list("propofol", "0", "2", "mg/kg")
  dt[2, ] <- list("", "", "", "")
  rownames(dt) <- NULL
  shiny::testServer(app_server, {
    session$setInputs(
      timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "60",
      plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
      weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
      sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE)
    session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
    session$flushReact()
    session$setInputs(recipient = "user@example.com", emailComments = "teaching case")
    session$setInputs(commentSafe = TRUE)
    # the edit and Send reach the server together, the box still ticked there
    session$setInputs(emailComments = "John Doe MRN 12345", sendSlide = 1)
    expect_equal(calls, 0)
    # ticked again for the comment as it stands, it is sent
    session$setInputs(commentSafe = FALSE)
    session$setInputs(commentSafe = TRUE)
    session$setInputs(sendSlide = 2)
    expect_equal(calls, 1)
  })
})

test_that("a dose dialog does not write over edits not yet applied (A2-F18)", {
  local_mocked_bindings(outputComments = function(...) {})
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig, add = TRUE)
  dt <- doseTableInit
  dt[1, ] <- list("carbamazepine", "0", "200", "mg PO tid")
  dt[2:6, ] <- list("", "", "", "")
  shiny::testServer(app_server, {
    session$setInputs(
      timeUnits = "minutes", timeMode = "clock", referenceTime = "08:00", maximum = "1440",
      plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
      weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
      sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE)
    startFormatFromInputs(session)
    doseTable(dt)
    session$flushReact()
    # an edit typed into the grid and not applied: a second drug
    draft <- doseTableDraft()
    draft[2, ] <- list("propofol", "0", "100", "mg")
    doseTableHistory()$do(draft)
    session$flushReact()
    committed <- doseTable()

    # Skip next dose from the plot's dialog is refused, and the edit stays
    showAddDrugModal("carbamazepine", 300)
    session$flushReact()
    session$setInputs(skipNextDoseBtn = 1)
    expect_identical(doseTable(), committed)
    expect_equal(doseTableDraft()$Drug[2], "propofol")
    expect_true(doseTableHistory()$can_undo)

    # once applied, the dialog writes
    session$setInputs(dosetable_apply = 1)
    session$setInputs(skipNextDoseBtn = 2)
    expect_true("propofol" %in% doseTable()$Drug)
    expect_true(any(doseTable()$Dose == "0" & doseTable()$Drug == "carbamazepine"))
  })
})

test_that("TCI is refused while an event or an interaction switches the PK set (A2-F9)", {
  dd <- getDrugDefaultsGlobal()
  # An infant's dexmedetomidine (Zuppa) has PK sets for cardiopulmonary bypass
  PK <- getDrugPK("dexmedetomidine", 7, 65, 0.5, "male",
                  drugDefaults = dd[dd$Drug == "dexmedetomidine", ])
  dose <- data.frame(Drug = "dexmedetomidine", Time = 0, Dose = 0.6,
                     Units = TCI_UNIT_PLASMA, stringsAsFactors = FALSE)
  none <- data.frame(Time = numeric(0), Event = character(0))
  cpb <- data.frame(Time = c(60, 180), Event = c("CPB Start", "CPB End"),
                    stringsAsFactors = FALSE)
  # with no event the target is held
  out <- simCpCe(dose, none, PK, 240, plotRecovery = FALSE)
  expect_equal(max(out$wide$Plasma[out$wide$Time >= 30]), 0.6, tolerance = 0.02)
  expect_error(simCpCe(dose, cpb, PK, 240, plotRecovery = FALSE),
               "clinical event or an antiseizure interaction")
  # an event the drug has no PK set for changes nothing, and is not refused
  other <- data.frame(Time = 60, Event = "Incision", stringsAsFactors = FALSE)
  expect_no_error(simCpCe(dose, other, PK, 240, plotRecovery = FALSE))

  # The Drug Library refuses a target unit on an antiseizure interaction victim
  ed <- dd
  ed$Units[ed$Drug == "lamotrigine"] <- paste(ed$Units[ed$Drug == "lamotrigine"], TCI_UNIT_PLASMA, sep = ",")
  expect_match(drugLibraryUnitProblems(ed[ed$Drug == "lamotrigine", ], dd),
               "antiseizure interactions")
})

test_that("a review stopped before its process started is not sent, nor counted (A2-F13)", {
  # The process checks the stop file after writing its ID: told to stop
  # first, it sends nothing (no network is touched here)
  pidFile <- tempfile("paper-review-")
  on.exit(unlink(c(pidFile, paste0(pidFile, ".stop"))), add = TRUE)
  writeLines("stop", paste0(pidFile, ".stop"))
  out <- paperReviewTransport("{}", "sk-ant-test", url = "http://127.0.0.1:9/never",
                              version = "x", beta = "x", timeout = 1, pidFile = pidFile)
  expect_identical(out$failure, "stopped")
  err <- tryCatch(paperReviewInterpret(out), paper_review_error = function(e) e)
  expect_identical(err$code, "stopped")
  expect_true(all(c("stopped", "discarded") %in% PAPER_REVIEW_UNSENT_CODES))

  # The stopper, with no process ID written yet, leaves the stop file
  skip_if_not_installed("mirai")
  job <- mirai::mirai(Sys.sleep(30))
  on.exit(mirai::stop_mirai(job), add = TRUE)
  fresh <- tempfile("paper-review-")
  on.exit(unlink(c(fresh, paste0(fresh, ".stop"))), add = TRUE)
  expect_true(paperReviewStopper(job, fresh)())
  expect_true(file.exists(paste0(fresh, ".stop")))
})

test_that("loading a scenario resets the MAC threshold (A2-F17)", {
  local_mocked_bindings(outputComments = function(...) {})
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig, add = TRUE)
  shiny::testServer(app_server, {
    session$setInputs(timeUnits = "minutes", timeMode = "relative", maximum = "60",
                      weight = 70, weightUnit = "1", height = 170, heightUnit = "1",
                      age = 40, ageUnit = "1", sex = "male")
    macThreshold(1.3)
    helpApi$loadScenario(helpScenarioById("tiva-remifentanil-propofol"))
    expect_equal(macThreshold(), GAS_MAC_THRESHOLD)
  })
})

test_that("the Physiology note counts altitude and endobronchial intubation (A2-F24)", {
  text <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", as.character(x)))
  normal <- physiologyFromInputs(list())
  # only the altitude moved: the agent is changed, and said to be
  note <- text(physiologyNote(normal, c("sevoflurane", "propofol"), altitude = 3000))
  expect_match(note, "Changed: sevoflurane")
  expect_no_match(note, "propofol")
  note <- text(physiologyNote(normal, "sevoflurane", endobronchial = TRUE))
  expect_match(note, "Changed: sevoflurane")
  expect_null(physiologyNote(normal, "sevoflurane"))
  # with an intravenous slider moved and the agents' controls normal, the
  # note names all four of their controls
  liver <- physiologyFromInputs(list(liver = log10(0.5)))
  expect_match(text(physiologyNote(liver, "sevoflurane")),
               "altitude and endobronchial intubation only")
})

test_that("tolcapone reads a missing albumin as the median (A2-F26)", {
  base <- tolcapone(70, 170, 60, "male")$PK$default$v2
  expect_equal(tolcapone(70, 170, 60, "male", albumin = NA)$PK$default$v2, base)
  expect_equal(tolcapone(70, 170, 60, "male", albumin = NaN)$PK$default$v2, base)
  expect_true(is.finite(tolcapone(70, 170, 60, "male", albumin = 3)$PK$default$v2))
})

test_that("getDrugPK() says how many of the rows given are the drug's (A2-F11)", {
  dd <- getDrugDefaultsGlobal()
  expect_error(getDrugPK("propofol", 70, 170, 40, "male",
                         drugDefaults = dd[dd$Drug %in% c("fentanyl", "sufentanil"), ]),
               "0 of the 2 rows given are propofol's")
})

test_that("Single Paper Review reads per-day infusions and never a partial name (A2-F14)", {
  # A 1-min infusion of 2 mg/day is 2/1440 mg, not 2 mg
  ex <- list(regimens = list(list(id = "r", doses = list(list(
    route = "iv_infusion", amount = 2, mass_unit = "mg", per_time = "per_day",
    duration = 1, time_units = "min")))))
  got <- paperReviewBolusAmount(ex, "r", "mcg", list(weight = 70))
  expect_equal(unname(got$amount), 2 / 1440)
  # A field missing from the reply is held as NULL, so $ cannot reach a
  # longer name beside it
  v <- paperReviewValidate(paperReviewNormalize(modifyList(paperReviewExample(),
    list(population = list(n = "24 (12 F)", n_female = 12)))))
  expect_null(v$population$n)
  expect_identical(v$population$n_female, 12)
})

test_that("azilsartan's Q/F takes no weight term with the switch off (A2-F3)", {
  q <- function(w) azilsartan(w, 170, 58, "male", adjustToFFM = FALSE)$PK$default$cl2
  expect_equal(q(45), q(86))
  expect_equal(q(120), q(86))
  # CL/F keeps the source's (weight/86)^0.267
  cl <- function(w) azilsartan(w, 170, 58, "male", adjustToFFM = FALSE)$PK$default$cl1
  expect_equal(cl(120) / cl(86), (120 / 86)^0.267)
})

test_that("the vaporiser check holds the vapour to the ambient pressure (A2-F23)", {
  dt <- data.frame(Drug = c("sevoflurane", "isoflurane"), Time = c("0", "0"),
                   Dose = c("30", "20"), Units = c("%", "%"), stringsAsFactors = FALSE)
  expect_length(gasVaporiserProblems(dt), 0)
  # at 7000 m the ambient pressure is about 43% of sea level's: 50% cannot be given
  expect_length(gasVaporiserProblems(dt, altitude = 7000), 1)
  expect_match(gasVaporiserProblems(dt, altitude = 7000), "7000 m")
  # a realistic setting is still fine there
  ok <- data.frame(Drug = "sevoflurane", Time = "0", Dose = "8", Units = "%", stringsAsFactors = FALSE)
  expect_length(gasVaporiserProblems(ok, altitude = 7000), 0)
})
