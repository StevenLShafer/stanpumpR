# The simulation API (R/api.R, inst/plumber/plumber.R; inst/apidocs/api.md).

apiRequest <- function(...) {
  base <- list(
    patient  = list(weight = 70, height = 170, age = 50, sex = "male"),
    duration = 60,
    doses    = data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  )
  args <- list(...)
  base[names(args)] <- args  # shallow: a data frame or patient replaces, not merges
  base
}

expectApiError <- function(code, pattern) {
  err <- tryCatch(code, stanpumpr_api_error = function(e) e)
  expect_s3_class(err, "stanpumpr_api_error")
  expect_match(conditionMessage(err), pattern)
}


test_that("the result returns the series on the requested grid, in minutes", {
  r <- apiSimulate(apiRequest(output = list(interval = 5)))

  expect_named(r, c("meta", "patient", "drugs", "doses", "events", "timeseries", "warnings"))
  expect_equal(r$meta$timeUnit, "minutes")
  expect_equal(r$meta$grid, "requested")
  expect_equal(unique(r$timeseries$Drug), "fentanyl")
  expect_equal(r$timeseries$Time, seq(0, 60, by = 5))
  expect_named(r$timeseries, c("Drug", "Time", "Plasma", "EffectSite", "ConcentrationUnits"))
  expect_equal(unique(r$timeseries$ConcentrationUnits), "ng/mL")
  expect_equal(r$drugs$MEAC, getDrugPK("fentanyl", 70, 170, 50, "male")$MEAC)
})


test_that("the grid is simulated, not interpolated: a bolus matches the closed form", {
  # Cp(t) after a bolus is dose * sum_i p_coef_bolus_li exp(-lambda_i t).  The
  # engine's own time line has no point at 0.25, 0.5 or 0.75 minutes, so these
  # values would be off if they were interpolated (they were by 1-2%).
  r <- apiSimulate(apiRequest(output = list(times = c(0.25, 0.5, 0.75, 7, 45))))
  pk <- getDrugPK("fentanyl", 70, 170, 50, "male")$PK[[1]]
  t <- r$timeseries$Time
  expected <- 100 * with(pk, p_coef_bolus_l1 * exp(-lambda_1 * t) +
                               p_coef_bolus_l2 * exp(-lambda_2 * t) +
                               p_coef_bolus_l3 * exp(-lambda_3 * t))
  expect_equal(r$timeseries$Plasma, expected, tolerance = 1e-10)
})


test_that("output times leave the engines' own time lines unchanged elsewhere", {
  native <- apiSimulate(apiRequest(output = list(interval = "native")))
  expect_equal(native$meta$grid, "native")
  direct <- simulateDrugsWithCovariates(
    data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg"),
    data.frame(Time = numeric(0), Event = character(0)), 70, 170, 50, "male", 60, FALSE)
  expect_equal(native$timeseries$Time, direct$fentanyl$wide$Time)
  expect_equal(native$timeseries$Plasma, direct$fentanyl$wide$Plasma)
  # and nothing leaks out of withOutputTimes() into the next simulation
  expect_null(stanpumpR:::.sprglobals$gridOutputTimes)
})


test_that("time, weight, height and age units are converted on the way in", {
  inMinutes <- apiSimulate(apiRequest(
    duration = 120,
    doses = data.frame(Drug = "propofol", Time = c(0, 30), Dose = c(2, 0), Units = c("mg/kg", "mg/kg")),
    output = list(interval = 10)))
  inHours <- apiSimulate(apiRequest(
    duration = 2, timeUnit = "hours",
    patient = list(weight = 70 / UNIT_LB, weightUnit = "lb", height = 170 / UNIT_INCH,
                   heightUnit = "in", age = 600, ageUnit = "months", sex = "male"),
    doses = data.frame(Drug = "propofol", Time = c(0, 0.5), Dose = c(2, 0), Units = c("mg/kg", "mg/kg")),
    output = list(interval = 1 / 6)))

  expect_equal(inHours$patient$weight, 70)
  expect_equal(inHours$patient$height, 170)
  expect_equal(inHours$patient$age, 50)
  expect_equal(inHours$meta$duration, 120)
  expect_equal(inHours$doses$Time, c(0, 30))
  expect_equal(inHours$timeseries$Time, inMinutes$timeseries$Time)
  expect_equal(inHours$timeseries$Plasma, inMinutes$timeseries$Plasma)
})


test_that("the default grid is the smallest step that keeps a series to 1000 rows", {
  grid <- function(duration, unit) {
    apiSimulate(apiRequest(duration = duration, timeUnit = unit))$timeseries$Time
  }
  expect_equal(diff(grid(60, "minutes"))[1], 0.1)
  expect_equal(diff(grid(8, "hours"))[1], 0.5)
  expect_equal(diff(grid(7, "days"))[1], 15)
})


test_that("field names are matched ignoring case and separators, rows or columns", {
  r1 <- apiSimulate(list(
    Patient = list(Weight = 70, HEIGHT = 170, age = 50, sex = "Male"),
    duration = 60,
    doses = list(drug = c("fentanyl", "fentanyl"), time = c(0, 10), dose = c(50, 50),
                 units = c("mcg", "mcg"))))
  r2 <- apiSimulate(apiParseRequest('{
    "patient": {"weight": 70, "height": 170, "age": 50, "sex": "male"},
    "duration": 60,
    "doses": [{"Drug": "fentanyl", "Time": 0, "Dose": 50, "Units": "mcg"},
              {"Drug": "fentanyl", "Time": "10", "Dose": "50", "Units": "mcg"}]}'))
  expect_equal(r1$timeseries, r2$timeseries)
})


test_that("metabolites, TCI rates and scheduled repeats are returned", {
  tramadol <- apiSimulate(apiRequest(
    duration = 4, timeUnit = "hours",
    doses = data.frame(Drug = "tramadol", Time = 0, Dose = 100, Units = "mg PO")))
  expect_setequal(tramadol$drugs$Drug, c("tramadol", "desmetramadol"))
  expect_false(tramadol$drugs$Dosed[tramadol$drugs$Drug == "desmetramadol"])
  # A prodrug has no effect site of its own
  expect_true(all(is.na(tramadol$timeseries$EffectSite[tramadol$timeseries$Drug == "tramadol"])))

  tci <- apiSimulate(apiRequest(
    doses = data.frame(Drug = "propofol", Time = 0, Dose = 3, Units = "Effect site target")))
  expect_gt(sum(tci$doses$Units != "Effect site target"), 1)
  ce <- tci$timeseries$EffectSite[tci$timeseries$Time == 30]
  expect_equal(ce, 3, tolerance = 0.01)

  bid <- apiSimulate(apiRequest(
    duration = 2, timeUnit = "days",
    doses = data.frame(Drug = "acetaminophen", Time = 0, Dose = 1000, Units = "mg PO bid")))
  expect_equal(bid$doses$Time, c(0, 720, 1440, 2160))
})


test_that("recovery is returned on request", {
  r <- apiSimulate(apiRequest(output = list(recovery = TRUE)))
  expect_true("Recovery" %in% names(r$timeseries))
  expect_false("Recovery" %in% names(apiSimulate(apiRequest())$timeseries))
})


test_that("invalid requests are refused with a message naming the problem", {
  expectApiError(apiSimulate(apiRequest(duration = NULL)), "`duration` is required")
  expectApiError(apiSimulate(apiRequest(duration = 53, timeUnit = "weeks")), "at most 52.14 weeks \\(365 days\\)")
  expectApiError(apiSimulate(apiRequest(timeUnit = "fortnights")), "timeUnit must be one of")
  expectApiError(apiSimulate(apiRequest(patient = list(weight = 70, height = 170, sex = "male"))),
                 "missing 'age'")
  expectApiError(apiSimulate(apiRequest(patient = list(weight = 900, height = 170, age = 50, sex = "male"))),
                 "patient.weight must be from")
  expectApiError(apiSimulate(apiRequest(patient = list(weight = 70, height = 170, age = 50, sex = "x"))),
                 "patient.sex must be one of")
  expectApiError(apiSimulate(apiRequest(patient = list(weight = 70, height = 170, age = 50, sex = "male",
                                                       pregnant = TRUE))),
                 "patient.pregnant is true, so patient.sex must be 'female'")
  expectApiError(apiSimulate(apiRequest(patient = list(weight = 70, height = 165, age = 30, sex = "female",
                                                       pregnant = "maybe"))),
                 "patient.pregnant must be true or false")
  expectApiError(apiSimulate(apiRequest(patient = list(weight = 70, height = 170, age = 50, sex = "male",
                                                       cyp2d6 = "fast"))),
                 "patient.cyp2d6 must be one of")
  expectApiError(apiSimulate(apiRequest(doses = data.frame(Drug = "aspirin2", Time = 0, Dose = 1, Units = "mg"))),
                 "row 1: unknown drug 'aspirin2'")
  expectApiError(apiSimulate(apiRequest(doses = data.frame(Drug = "propofol", Time = 0, Dose = 1, Units = "mg PO"))),
                 "'mg PO' is not a unit of propofol")
  expectApiError(apiSimulate(apiRequest(doses = data.frame(Drug = "fentanyl", Time = 0, Dose = -1, Units = "mcg"))),
                 "non-negative")
  expectApiError(apiSimulate(apiRequest(doses = data.frame(Drug = "fentanyl", Time = "soon", Dose = 1, Units = "mcg"))),
                 "time 'soon'")
  expectApiError(apiSimulate(apiRequest(doses = data.frame(Drug = "fentanyl", Time = 0, Dose = 1))),
                 "missing the column\\(s\\) 'Units'")
  expectApiError(apiSimulate(apiRequest(events = data.frame(Time = 1, Event = "Lunch"))),
                 "unknown event 'Lunch'")
  expectApiError(apiSimulate(apiRequest(output = list(interval = 0.001))), "too small")
  expectApiError(apiSimulate(apiRequest(output = list(times = c(1, 61)))), "not be later than")
  expectApiError(apiSimulate(apiRequest(
    duration = 2, timeUnit = "weeks",
    doses = data.frame(Drug = "propofol", Time = 0, Dose = 3, Units = "Plasma target"))),
    "only up to a week")
  expectApiError(apiParseRequest("{not json"), "not valid JSON")
  expectApiError(apiParseRequest(""), "empty")
})


test_that("inhaled agents are refused and illicit drugs need the opt-in", {
  gas <- gasDrugNames()[1]
  expectApiError(apiSimulate(apiRequest(doses = data.frame(Drug = gas, Time = 0, Dose = 1, Units = "%"))),
                 "inhaled agent")

  illicit <- illicitDrugNames()[1]
  units <- getDrugDefaults(illicit)$Units[[1]][1]
  doses <- data.frame(Drug = illicit, Time = 0, Dose = 1, Units = units)
  expectApiError(apiSimulate(apiRequest(doses = doses)), "allowIllicitDrugs")
  r <- apiSimulate(apiRequest(doses = doses, options = list(allowIllicitDrugs = TRUE)))
  expect_true(illicit %in% r$drugs$Drug)

  expect_false(illicit %in% apiDrugCatalog()$Drug)
  expect_true(illicit %in% apiDrugCatalog(allowIllicit = TRUE)$Drug)
  expect_false(gas %in% apiDrugCatalog(allowIllicit = TRUE)$Drug)
})


test_that("doses at or after the end of the simulation are reported", {
  r <- apiSimulate(apiRequest(doses = data.frame(Drug = "fentanyl", Time = c(0, 90), Dose = 50, Units = "mcg")))
  expect_match(r$warnings, "ignored \\(fentanyl\\)")
})


test_that("every format can be read back", {
  r <- apiSimulate(apiRequest(
    doses = data.frame(Drug = c("fentanyl", "propofol"), Time = 0, Dose = c(100, 150), Units = c("mcg", "mg")),
    output = list(interval = 10)))

  json <- jsonlite::fromJSON(apiFormatResult(r, "json")$body)
  expect_equal(json$timeseries$Plasma, r$timeseries$Plasma, tolerance = 1e-9)
  expect_equal(json$drugs$Drug, r$drugs$Drug)

  csv <- utils::read.csv(text = apiFormatResult(r, "csv")$body)
  expect_equal(csv$Plasma, r$timeseries$Plasma, tolerance = 1e-12)

  wide <- utils::read.csv(text = apiFormatResult(r, "csv", layout = "wide")$body, check.names = FALSE)
  expect_equal(wide$Time, seq(0, 60, by = 10))
  expect_equal(wide$`propofol EffectSite`, r$timeseries$EffectSite[r$timeseries$Drug == "propofol"])

  out <- apiFormatResult(r, "xlsx")
  expect_type(out$body, "raw")
  path <- tempfile(fileext = ".xlsx")
  writeBin(out$body, path)
  expect_equal(openxlsx::getSheetNames(path), c("Time Series", "Drugs", "Doses", "Patient", "About"))
  expect_equal(openxlsx::read.xlsx(path, "Time Series")$Plasma, r$timeseries$Plasma, tolerance = 1e-12)

  xml <- apiFormatResult(r, "xml")$body
  skip_if_not_installed("xml2")
  doc <- xml2::read_xml(xml)
  points <- xml2::xml_find_all(doc, "//timeseries/point")
  expect_length(points, nrow(r$timeseries))
  expect_equal(as.numeric(xml2::xml_attr(points, "Plasma")), r$timeseries$Plasma, tolerance = 1e-12)
})


test_that("the wide layout needs a common grid", {
  r <- apiSimulate(apiRequest(output = list(interval = "native")))
  expectApiError(apiFormatResult(r, "csv", layout = "wide"), "common time grid")
})


test_that("apiWriteResult picks the format from the extension", {
  r <- apiSimulate(apiRequest())
  path <- apiWriteResult(r, tempfile(fileext = ".csv"))
  expect_equal(nrow(utils::read.csv(path)), nrow(r$timeseries))
})


test_that("the format is negotiated from the query, the body, then Accept", {
  expect_equal(stanpumpR:::apiNegotiateFormat("CSV", list(output = list(format = "xml"))), "csv")
  expect_equal(stanpumpR:::apiNegotiateFormat("", list(output = list(format = "xml"))), "xml")
  expect_equal(stanpumpR:::apiNegotiateFormat("", list(), "text/csv, */*"), "csv")
  expect_equal(stanpumpR:::apiNegotiateFormat(NULL, list(), "*/*"), "json")
})


test_that("the HTTP service answers /health, /drugs and /simulate", {
  skip_if_not_installed("plumber")
  api <- plumber::pr(system.file("plumber", "plumber.R", package = "stanpumpR"))
  call <- function(method, path, query = "", body = "", accept = "*/*",
                   type = "application/json") {
    req <- new.env()
    req$REQUEST_METHOD <- method
    req$PATH_INFO <- path
    req$QUERY_STRING <- query
    req$HTTP_ACCEPT <- accept
    req$CONTENT_TYPE <- type
    req$HTTP_CONTENT_TYPE <- type
    req$HEADERS <- c(accept = accept, "content-type" = type)
    bodyRaw <- charToRaw(body)
    req$rook.input <- list(read = function(...) bodyRaw, rewind = function() invisible(0),
                           read_lines = function() body)
    api$call(req)
  }
  text <- function(res) if (is.raw(res$body)) rawToChar(res$body) else res$body

  health <- call("GET", "/health")
  expect_equal(health$status, 200L)
  expect_equal(jsonlite::fromJSON(text(health))$status, "ok")

  drugs <- call("GET", "/drugs", "?format=csv")
  expect_equal(drugs$status, 200L)
  expect_true("propofol" %in% utils::read.csv(text = text(drugs))$Drug)

  body <- as.character(jsonlite::toJSON(apiRequest(output = list(interval = 10)),
                                        dataframe = "rows", auto_unbox = TRUE))
  ok <- call("POST", "/simulate", "?format=csv", body)
  expect_equal(ok$status, 200L)
  expect_equal(ok$headers[["Content-Type"]], "text/csv")
  expect_equal(utils::read.csv(text = text(ok))$Time, seq(0, 60, by = 10))

  bad <- call("POST", "/simulate", "", sub("fentanyl", "fentanil", body))
  expect_equal(bad$status, 400L)
  expect_match(jsonlite::fromJSON(text(bad))$error, "unknown drug 'fentanil'")

  # Malformed JSON sent as JSON reaches apiParseRequest() (@parser none), so
  # it is a 400 that says why, not plumber's bare 500.
  malformed <- call("POST", "/simulate", "", "{not json")
  expect_equal(malformed$status, 400L)
  expect_match(jsonlite::fromJSON(text(malformed))$error, "not valid JSON")

  # The documentation page is the service's front page.
  page <- call("GET", "/")
  expect_equal(page$status, 200L)
  expect_match(page$headers[["Content-Type"]], "text/html", fixed = TRUE)
  expect_match(text(page), "<title>stanpumpR Simulation API</title>", fixed = TRUE)

  # The layout, like the format, takes the query parameter over the body.
  wideBody <- as.character(jsonlite::toJSON(
    apiRequest(output = list(interval = 10, layout = "wide")),
    dataframe = "rows", auto_unbox = TRUE))
  wide <- utils::read.csv(text = text(call("POST", "/simulate", "?format=csv", wideBody)),
                          check.names = FALSE)
  expect_true("fentanyl Plasma" %in% names(wide))
  long <- utils::read.csv(text = text(call("POST", "/simulate", "?format=csv&layout=long", wideBody)))
  expect_true(all(c("Drug", "Plasma") %in% names(long)))
})

test_that("patient.pregnant reaches the models that read it and is echoed back", {
  doses <- data.frame(Drug = "remifentanil", Time = 0, Dose = 1, Units = "mcg/kg/min")
  patient <- list(weight = 70, height = 165, age = 30, sex = "female")
  off <- apiSimulate(apiRequest(patient = patient, doses = doses, duration = 30))
  on  <- apiSimulate(apiRequest(patient = c(patient, pregnant = TRUE), doses = doses, duration = 30))
  expect_false(off$patient$pregnant)
  expect_true(on$patient$pregnant)
  # doubled clearance: half the plateau
  last <- function(x) tail(x$timeseries$Plasma[x$timeseries$Drug == "remifentanil"], 1)
  expect_equal(last(on) / last(off), 0.5, tolerance = 0.03)
  # the string forms the other flags accept
  expect_true(apiSimulate(apiRequest(patient = c(patient, pregnant = "yes"), doses = doses, duration = 30))$patient$pregnant)
  # a drug that does not read it is unchanged
  fent <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  expect_equal(apiSimulate(apiRequest(patient = c(patient, pregnant = TRUE), doses = fent))$timeseries,
               apiSimulate(apiRequest(patient = patient, doses = fent))$timeseries)
})
