# Carbon's (Gemini's) repository audit of fb20428, 2026-11-18: one test per
# finding that reproduced, each failing on the code before its fix.

test_that("a blank time beside one past midnight does not stop the clock-time conversion (F1)", {
  # "07:00" wraps past midnight from an 08:00 start; "" and "25:00" are NA
  expect_equal(clockTimeToDelta("08:00", c("07:00", "", "09:00", "25:00")),
               c(23 * 60, NA, 60, NA))
})

test_that("hourMinute() returns minutes or NA_real_, never a POSIXct NA (F1)", {
  expect_identical(hourMinute(c("01:30", "nonsense")), c(90, NA_real_))
  expect_true(is.numeric(hourMinute("23:59:59")))
  expect_identical(deltaToClockTime("23:59:59", 1), NA_character_)
  expect_identical(deltaToClockTime("08:00", c(60, NA)), c("09:00", NA))
})

test_that("an eigenvalue past 709/dt does not turn the effect-site TCI rate into NaN (F2)", {
  K <- list(lambda3 = c(2000, 0.1, 0.01), lambda4 = c(2000, 0.1, 0.01, 0.5),
            ke0 = 0.5, tPeak = 2,
            pinf = c(1e-4, 0.01, 0.1), einf = c(1e-6, 0.02, 0.15, -0.15))
  x <- tciRate(p = c(0, 0, 0), e = c(0, 0, 0, 0), CT = 1, site = "effect", dt = 0.5, K = K)
  expect_true(x$effectMode)
  expect_true(is.finite(x$rate))
})

test_that("erfcx keeps full precision for large arguments (F5)", {
  x <- c(1e3, 1e8, 1e10, 1e200)
  expect_equal(erfcxReal(x), 1 / (sqrt(pi) * x) * (1 - 1 / (2 * x^2)), tolerance = 1e-12)
  expect_false(anyNA(erfcxReal(c(1e154, 1e300))))
})

test_that("simCpCe() runs a plot of length 0 (F7)", {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("propofol", 70, 170, 40, "male", drugDefaults = dd[dd$Drug == "propofol", ])
  out <- simCpCe(data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg"),
                 data.frame(Time = numeric(0), Event = character(0)), PK, 0,
                 plotRecovery = FALSE)
  expect_false("Time.1" %in% names(out$equiSpace))
})

test_that("a target on a drug whose parameters change with the dose is refused (F7)", {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("propofol", 70, 170, 40, "male", drugDefaults = dd[dd$Drug == "propofol", ])
  PK$doseDependent <- TRUE
  expect_error(
    simCpCe(data.frame(Drug = "propofol", Time = 0, Dose = 3, Units = TCI_UNIT_PLASMA),
            data.frame(Time = numeric(0), Event = character(0)), PK, 60, plotRecovery = FALSE),
    "change with the dose size")
})

test_that("the PK converter says why k21 = k31 is not a three-compartment model (F8)", {
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  js <- system.file("www", "pk-convert.js", package = "stanpumpR")
  script <- tempfile(fileext = ".js")
  on.exit(unlink(script))
  writeLines(c("global.window = {};", sprintf("eval(require('fs').readFileSync(%s, 'utf8') + ';global.P = PKConvert;');",
                                            encodeString(js, quote = "'")),
               "console.log(JSON.stringify(P.fromVolumesClearances(3, [10, 20, 40], [5, 2, 4])));"), script)
  out <- jsonlite::fromJSON(system2("node", script, stdout = TRUE))
  expect_match(out$error, "k21 equals k31")
})

test_that("simulateDrugsWithCovariates() output can be plotted (F10)", {
  dt <- data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg")
  ev <- data.frame(Time = numeric(0), Event = character(0))
  out <- simulateDrugsWithCovariates(dt, ev, 70, 170, 40, "male", maximum = 60,
                                     plotRecovery = FALSE)
  p <- suppressMessages(simulationPlot(drugs = out, events = ev,
                                       drugDefaults = getDrugDefaultsGlobal(),
                                       eventDefaults = getEventDefaults()))
  expect_false(is.null(p))
})

test_that("cube() keeps the slow root of a two-compartment model (F12)", {
  r <- cube(k10 = 1, k12 = 0.5, k13 = 0, k21 = 1e-16, k31 = 0)
  # relative: expect_equal() compares numbers this small absolutely
  # (the old difference of two nearly equal numbers gave 1.11e-16, 67% high)
  expect_lt(abs(r[2] / (1e-16 / 1.5) - 1), 1e-6)
})

test_that("getDrugPK() takes the drug's own row from a whole library table (F13)", {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("propofol", 70, 170, 40, "male", drugDefaults = dd)
  expect_length(PK$Concentration.Units, 1)
  expect_error(getDrugPK("propofol", 70, 170, 40, "male",
                         drugDefaults = dd[dd$Drug %in% c("fentanyl", "sufentanil"), ]),
               "one library row")
})

test_that("the R code is ASCII outside its comments (F15)", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
  if (!dir.exists(file.path(root, "R"))) skip("not a source checkout")
  for (f in list.files(file.path(root, "R"), "\\.R$", full.names = TRUE)) {
    p <- utils::getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"))
    tokens <- p$text[p$token %in% c("STR_CONST", "SYMBOL", "SYMBOL_FUNCTION_CALL")]
    expect_false(any(grepl("[^\\x01-\\x7f]", tokens, perl = TRUE)), info = basename(f))
  }
})

test_that("loading a scenario resets opioid tolerance and the physiology (U1)", {
  session <- shiny::MockShinySession$new()
  sent <- list()
  unlockBinding("sendInputMessage", session)
  session$sendInputMessage <- function(inputId, message) sent[[inputId]] <<- message
  lockBinding("sendInputMessage", session)
  doseTable <- shiny::reactiveVal(doseTableInit)
  eventTable <- shiny::reactiveVal(eventTableInit)
  applyHelpScenario(session, helpScenarioById("tiva-remifentanil-propofol"), doseTable, eventTable)
  expect_equal(sent$opioidTolerance$value, OPIOID_TOLERANCE_DEFAULT)
  for (f in PHYSIOLOGY_FACTORS) expect_equal(as.numeric(sent[[physiologyInputId(f)]]$value), 0)
  expect_equal(as.numeric(sent$physiology_temperature$value), PHYSIOLOGY_TEMPERATURE_DEFAULT)
  expect_equal(as.numeric(sent$physiology_altitude$value), ALTITUDE_DEFAULT)
  expect_false(isTRUE(sent$endobronchialIntubation$value))
})
