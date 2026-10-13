# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-11, at the request of Steven L. Shafer: an
# API that takes a dose table, the patient's covariates and the length of the
# simulation, and returns the simulated concentrations in a format a script in
# any language can read (JSON, CSV, Excel or XML).
#
# STATUS: run and verified on R 4.3.3 by tests/testthat/test-api.R.
# -----------------------------------------------------------------------------
#
# THE SIMULATION API
# ==================
# Three layers, so that each can be used without the one above it:
#
#   apiSimulate(request)          a request (a list, as parsed from JSON) to a
#                                 result (a list of data frames).  Pure R, no
#                                 Shiny and no HTTP: an R script or RStudio
#                                 session can call it directly.
#   apiFormatResult(result, fmt)  a result to the bytes of one format.
#   inst/plumber/plumber.R        the HTTP service (run_api()): POST /simulate,
#                                 GET /drugs, GET /health.
#
# The request and the result are specified in docs/api.md.  The simulation
# itself is simulateDrugsWithCovariates(), the same Shiny-free entry point the
# tests and vignettes use, so the API returns what the app plots.
#
# Units.  The request may give its times in minutes, hours, days or weeks
# (`timeUnit`), weight in kg or lb, height in cm or in, and age in years or
# months; everything is converted on the way in.  The result is always in
# minutes, kg, cm and years, whatever the request used, so that a client never
# has to know how a request was written to read its answer.
#
# Time grid.  Each drug is simulated on its own adaptive time line, dense
# after every dose (R/simulationTimeGrid.R).  A client usually wants one row
# per time with every drug in it, so the requested output times (a regular
# grid by default) are added to every engine's time line with
# withOutputTimes().  The closed-form engines are exact at any point of their
# line, so the values returned are simulated at those times, not interpolated.
# `output.interval = "native"` returns each drug's own time line instead.

API_VERSION <- "1"
API_FORMATS <- c("json", "csv", "xlsx", "xml")
API_CONTENT_TYPES <- c(
  json = "application/json",
  csv  = "text/csv",
  xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  xml  = "application/xml"
)
API_LAYOUTS <- c("long", "wide")
# The most rows one drug's series may have.  A request for a 1-minute grid
# over a year would otherwise ask for half a million rows per drug.
API_MAX_POINTS <- 20000L
# Default grid steps (minutes): the smallest that keeps a series to
# API_DEFAULT_POINTS rows or fewer: 0.1 minute up to 100 minutes, 1 minute up
# to 1000 minutes, an hour for 6 weeks.
API_DEFAULT_POINTS <- 1000L
API_GRID_STEPS <- c(0.1, 0.25, 0.5, 1, 2, 5, 10, 15, 30, 60, 120, 240, 360,
                    720, 1440, 2880, 10080)
# kg, cm and years per unit.  (Functions: R/constants.R is collated after
# this file.)
apiWeightUnits <- function() c(kg = UNIT_KG, lb = UNIT_LB)
apiHeightUnits <- function() c(cm = UNIT_CM, `in` = UNIT_INCH)
apiAgeUnits    <- function() c(years = 1, months = 1 / 12)

# An error in the request: the HTTP layer answers it with `status` and the
# message, where any other error is a 500.
apiError <- function(..., status = 400L) {
  stop(structure(
    class = c("stanpumpr_api_error", "error", "condition"),
    list(message = paste0(...), call = NULL, status = status)
  ))
}

# The value of `name` in a list, ignoring case and any "_", " " or "." in the
# name, so "effectSite", "effect_site" and "EffectSite" are the same field.
apiField <- function(x, name, default = NULL) {
  if (!is.list(x) || length(x) == 0 || is.null(names(x))) return(default)
  key <- function(s) gsub("[_ .]", "", tolower(s))
  hit <- which(key(names(x)) == key(name))
  if (length(hit) == 0) return(default)
  value <- x[[hit[1]]]
  if (is.null(value)) default else value
}

apiScalar <- function(x, what) {
  if (is.list(x)) x <- unlist(x)
  if (length(x) != 1 || is.na(x)) apiError(what, " must be a single value.")
  x
}

apiNumber <- function(x, what, min = -Inf, max = Inf) {
  x <- apiScalar(x, what)
  v <- suppressWarnings(as.numeric(x))
  if (!is_valid_number(v, min, max)) {
    apiError(what, " must be a number from ", min, " to ", max, "; got '", x, "'.")
  }
  v
}

apiChoice <- function(x, what, choices) {
  x <- as.character(apiScalar(x, what))
  if (!x %in% choices) {
    apiError(what, " must be one of ", paste0("'", choices, "'", collapse = ", "),
             "; got '", x, "'.")
  }
  x
}

apiFlag <- function(x, what) {
  x <- apiScalar(x, what)
  if (is.logical(x)) return(x)
  x <- tolower(as.character(x))
  if (x %in% c("true", "yes", "1")) return(TRUE)
  if (x %in% c("false", "no", "0")) return(FALSE)
  apiError(what, " must be true or false.")
}

#' Parse a simulation request from JSON
#'
#' @param text the request body, a JSON string (or raw vector)
#' @returns the request as a list, ready for [apiSimulate()]
#' @export
apiParseRequest <- function(text) {
  if (is.raw(text)) text <- rawToChar(text)
  if (is.null(text) || length(text) == 0 || !nzchar(paste(text, collapse = ""))) {
    apiError("The request body is empty; send the request as JSON (see docs/api.md).")
  }
  request <- tryCatch(
    jsonlite::fromJSON(paste(text, collapse = "\n"), simplifyVector = TRUE),
    error = function(e) apiError("The request body is not valid JSON: ", conditionMessage(e))
  )
  if (!is.list(request) || is.data.frame(request)) {
    apiError("The request must be a JSON object with `patient`, `doses` and `duration`.")
  }
  request
}

# A table from JSON: an array of objects (a data frame after fromJSON()), an
# object of equal-length columns, or a list of objects (when the rows do not
# all have the same fields).  Column names are matched as apiField() matches
# them and returned as `columns`.
apiTable <- function(x, what, columns, required = columns) {
  if (is.null(x) || length(x) == 0) {
    return(as.data.frame(setNames(rep(list(character(0)), length(columns)), columns)))
  }
  if (!is.data.frame(x)) {
    if (is.list(x) && is.null(names(x))) {
      x <- tryCatch(
        do.call(rbind, lapply(x, function(r) as.data.frame(r, stringsAsFactors = FALSE))),
        error = function(e) apiError(what, " rows must all have the same fields."))
    } else if (is.list(x)) {
      lengths <- vapply(x, length, integer(1))
      if (length(unique(lengths)) != 1) apiError(what, " columns must all have the same length.")
      x <- as.data.frame(x, stringsAsFactors = FALSE)
    } else {
      apiError(what, " must be an array of rows or an object of columns.")
    }
  }
  out <- lapply(columns, function(col) apiField(as.list(x), col))
  names(out) <- columns
  missing <- columns[vapply(out, is.null, logical(1))]
  missing <- intersect(missing, required)
  if (length(missing) > 0) {
    apiError(what, " is missing the column(s) ", paste0("'", missing, "'", collapse = ", "), ".")
  }
  out[vapply(out, is.null, logical(1))] <- list(rep(NA, nrow(x)))
  as.data.frame(lapply(out, unlist), stringsAsFactors = FALSE)
}

# A time (a number in `unit`, or "H:MM" elapsed) to minutes
apiTimes <- function(x, unit, what) {
  minutes <- displayTimeToMinutes(as.character(x), REFERENCE_TIME_NONE, unit)
  bad <- which(is.na(minutes) | minutes < 0)
  if (length(bad) > 0) {
    apiError(what, " row ", bad[1], ": time '", x[bad[1]], "' is not a non-negative number of ",
             unit, " (or elapsed H:MM).")
  }
  minutes
}

# The patient block to the covariates simulateDrugsWithCovariates() takes, in
# kg, cm and years.  Every field but weight, height, age and sex is optional
# and has the app's default.
apiPatient <- function(patient) {
  if (!is.list(patient) || length(patient) == 0) {
    apiError("`patient` is required: weight, height, age and sex.")
  }
  get <- function(name, default = NULL) apiField(patient, name, default)
  required <- c("weight", "height", "age", "sex")
  missing <- required[vapply(required, function(n) is.null(get(n)), logical(1))]
  if (length(missing) > 0) {
    apiError("`patient` is missing ", paste0("'", missing, "'", collapse = ", "), ".")
  }

  weightUnit <- apiChoice(get("weightUnit", "kg"), "patient.weightUnit", names(apiWeightUnits()))
  heightUnit <- apiChoice(get("heightUnit", "cm"), "patient.heightUnit", names(apiHeightUnits()))
  ageUnit    <- apiChoice(get("ageUnit", "years"), "patient.ageUnit", names(apiAgeUnits()))
  weight <- apiNumber(get("weight"), "patient.weight") * apiWeightUnits()[[weightUnit]]
  height <- apiNumber(get("height"), "patient.height") * apiHeightUnits()[[heightUnit]]
  age    <- apiNumber(get("age"), "patient.age") * apiAgeUnits()[[ageUnit]]
  # The limits are the app's, checked after conversion so they mean the same
  # thing whatever unit the request used.
  if (!is_valid_number(weight, MIN_WEIGHT, MAX_WEIGHT)) {
    apiError("patient.weight must be from ", MIN_WEIGHT, " to ", MAX_WEIGHT, " kg.")
  }
  if (!is_valid_number(height, MIN_HEIGHT, MAX_HEIGHT)) {
    apiError("patient.height must be from ", MIN_HEIGHT, " to ", MAX_HEIGHT, " cm.")
  }
  if (!is_valid_number(age, MIN_AGE, MAX_AGE)) {
    apiError("patient.age must be from ", MIN_AGE, " to ", MAX_AGE, " years.")
  }

  creatinine <- get("creatinine")
  if (!is.null(creatinine)) {
    creatinine <- apiNumber(creatinine, "patient.creatinine", MIN_CREATININE, MAX_CREATININE)
  }
  sex <- apiChoice(tolower(apiScalar(get("sex"), "patient.sex")), "patient.sex", SEX_VALUES)
  # Read only by the models the pregnancy review quantifies (R/pregnancy.R);
  # getDrugPK() would refuse it for a man, but the request is told why first.
  pregnant <- apiFlag(get("pregnant", FALSE), "patient.pregnant")
  if (pregnant && sex != SEX_FEMALE) {
    apiError("patient.pregnant is true, so patient.sex must be 'female'.")
  }
  list(
    weight      = weight,
    height      = height,
    age         = age,
    sex         = sex,
    pregnant    = pregnant,
    adjustToFFM = apiFlag(get("adjustToFFM", TRUE), "patient.adjustToFFM"),
    osmolality  = apiNumber(get("osmolality", OSMOLALITY_DEFAULT), "patient.osmolality",
                            MIN_OSMOLALITY, MAX_OSMOLALITY),
    creatinine  = creatinine,
    cyp2d6      = apiChoice(get("cyp2d6", CYP2D6_DEFAULT), "patient.cyp2d6", CYP2D6_VALUES),
    cyp2c19     = apiChoice(get("cyp2c19", CYP2C19_DEFAULT), "patient.cyp2c19", CYP2C19_VALUES),
    cyp2c9      = apiChoice(get("cyp2c9", CYP2C9_DEFAULT), "patient.cyp2c9", CYP2C9_VALUES)
  )
}

# The dose table, checked row by row against the drug library, with Time in
# minutes.  The checks are validateDoseTableInput()'s, plus the one the app
# makes through its units dropdowns: each drug only takes its own units.
apiDoses <- function(doses, unit, maximum, allowIllicit,
                     drugDefaults = getDrugDefaultsGlobal()) {
  DT <- apiTable(doses, "`doses`", c("Drug", "Time", "Dose", "Units"))
  if (nrow(DT) == 0) apiError("`doses` is empty: give at least one dose.")
  if (nrow(DT) > MAX_DOSE_ROWS) apiError("`doses` has more than ", MAX_DOSE_ROWS, " rows.")

  DT$Drug  <- trimws(as.character(DT$Drug))
  DT$Units <- trimws(as.character(DT$Units))
  dose <- suppressWarnings(as.numeric(DT$Dose))
  for (i in seq_len(nrow(DT))) {
    drug <- DT$Drug[i]
    row <- match(drug, drugDefaults$Drug)
    if (is.na(row)) {
      apiError("`doses` row ", i, ": unknown drug '", drug, "'. GET /drugs lists the drugs.")
    }
    if (isGasDrug(drug)) {
      apiError("`doses` row ", i, ": ", drug, " is an inhaled agent; the API does not ",
               "simulate inhaled agents yet.")
    }
    if (!allowIllicit && isIllicitDrug(drug, drugDefaults)) {
      apiError("`doses` row ", i, ": ", drug, " is in the '", ILLICIT_DRUG_CATEGORY,
               "' category, an opt-in research model; set options.allowIllicitDrugs to true.")
    }
    allowed <- drugDefaults$Units[[row]]
    if (!DT$Units[i] %in% allowed) {
      apiError("`doses` row ", i, ": '", DT$Units[i], "' is not a unit of ", drug,
               ". Its units are: ", paste(allowed, collapse = ", "), ".")
    }
    if (!is_valid_number(dose[i], 0, MAX_DOSE_VALUE)) {
      apiError("`doses` row ", i, ": dose '", DT$Dose[i], "' must be a non-negative number.")
    }
  }
  DT$Dose <- dose
  DT$Time <- apiTimes(DT$Time, unit, "`doses`")
  if (any(DT$Units %in% tciUnits) && maximum > ACUTE_MAX_PLOT_MINUTES) {
    apiError("Target-controlled (", paste(tciUnits, collapse = ", "), ") doses are ",
             "simulated only up to a week; shorten `duration`.")
  }
  DT <- DT[order(DT$Drug, DT$Time), ]
  rownames(DT) <- NULL
  DT
}

apiEvents <- function(events, unit, eventDefaults = getEventDefaults()) {
  ET <- apiTable(events, "`events`", c("Time", "Event"))
  if (nrow(ET) > MAX_EVENT_ROWS) apiError("`events` has more than ", MAX_EVENT_ROWS, " rows.")
  ET$Event <- as.character(ET$Event)
  unknown <- which(!ET$Event %in% eventDefaults$Event)
  if (length(unknown) > 0) {
    apiError("`events` row ", unknown[1], ": unknown event '", ET$Event[unknown[1]],
             "'. The events are: ", paste(eventDefaults$Event, collapse = ", "), ".")
  }
  ET$Time <- if (nrow(ET) == 0) numeric(0) else apiTimes(ET$Time, unit, "`events`")
  ET[, c("Time", "Event")]
}

# The output times (minutes): `times` if given, else a regular grid with step
# `interval`, else the default grid.  NULL means each drug's own points.
apiOutputTimes <- function(output, unit, maximum) {
  times <- apiField(output, "times")
  if (!is.null(times)) {
    times <- sort(unique(apiTimes(unlist(times), unit, "`output.times`")))
    if (any(times > maximum)) apiError("`output.times` must not be later than `duration`.")
    if (length(times) > API_MAX_POINTS) {
      apiError("`output.times` has more than ", API_MAX_POINTS, " times.")
    }
    return(times)
  }
  interval <- apiField(output, "interval")
  if (identical(tolower(as.character(interval)), "native")) return(NULL)
  step <- if (is.null(interval)) {
    fits <- API_GRID_STEPS[maximum / API_GRID_STEPS <= API_DEFAULT_POINTS]
    if (length(fits) == 0) max(API_GRID_STEPS) else fits[1]
  } else {
    apiNumber(interval, "output.interval", 0, Inf) * TIME_UNITS[[unit]]
  }
  if (step <= 0) apiError("output.interval must be greater than 0, or \"native\".")
  if (maximum / step + 1 > API_MAX_POINTS) {
    apiError("output.interval is too small: ", API_MAX_POINTS, " rows at most per drug ",
             "(", signif(maximum / (API_MAX_POINTS - 1) / TIME_UNITS[[unit]], 3), " ", unit,
             " or more for this duration).")
  }
  # Built from integers so 0.1-minute steps do not drift.
  times <- seq(0, floor(maximum / step + 1e-9)) * step
  if (maximum - utils::tail(times, 1) > 1e-9) times <- c(times, maximum)
  times
}

# A drug's simulated series on `times` (NULL: its own points).  The engines
# simulated every one of `times` (withOutputTimes()), so approx() only picks
# them out; it interpolates only for a series built on some other line.
# Where the line has two points at one time, the last is reported.
apiSeries <- function(wide, times, recovery) {
  wide <- wide[order(wide$Time), ]
  if (is.null(times)) {
    out <- data.frame(Time = wide$Time, Plasma = wide$Plasma, EffectSite = wide$`Effect Site`)
    if (recovery) out$Recovery <- wide$Recovery
    return(out)
  }
  last <- list("ordered", function(y) y[length(y)])
  at <- function(y, naRm = TRUE) {
    if (sum(!is.na(y)) < 2) return(rep(NA_real_, length(times)))
    stats::approx(wide$Time, y, xout = times, ties = last, rule = 2, na.rm = naRm)$y
  }
  out <- data.frame(Time = times, Plasma = at(wide$Plasma), EffectSite = at(wide$`Effect Site`))
  # A stretch where the time until threshold could not be computed stays
  # missing rather than being interpolated across; see finishDrugSeries().
  if (recovery) out$Recovery <- at(wide$Recovery, naRm = FALSE)
  out
}

apiConcentrationUnits <- function(units) {
  switch(as.character(units),
         "mcg" = "mcg/mL", "ng" = "ng/mL", "mg" = "mg/mL", "mOsm" = "mOsm/kg",
         as.character(units))
}

#' Simulate a dose table for one patient
#'
#' The engine behind the HTTP API (`run_api()`), callable directly from R.
#' The request format is specified in `docs/api.md`; in short:
#'
#' ```
#' list(
#'   patient  = list(weight = 70, height = 170, age = 50, sex = "male"),
#'   duration = 4, timeUnit = "hours",
#'   doses    = data.frame(Drug = "propofol", Time = 0, Dose = 150, Units = "mg")
#' )
#' ```
#'
#' @param request a list, usually from [apiParseRequest()]: `patient`
#'   (weight, height, age, sex and the optional covariates), `doses` (Drug,
#'   Time, Dose, Units), `duration`, and optionally `timeUnit`, `events` and
#'   `output` (`interval`, `times`, `recovery`).
#' @returns a list: `meta` (versions and units), `patient` (the covariates as
#'   simulated, in kg, cm and years), `drugs` (one row per simulated drug:
#'   concentration units and peaks), `doses` (the full dose sequence,
#'   including target-controlled infusion rates and repeated scheduled doses),
#'   `events`, `timeseries` (Drug, Time, Plasma, EffectSite and optionally
#'   Recovery) and `warnings`.  Every time is in minutes.  Errors in the
#'   request are signalled with class `stanpumpr_api_error`.
#' @export
apiSimulate <- function(request) {
  if (!is.list(request)) apiError("The request must be a list (a JSON object).")
  opts    <- apiField(request, "options", list())
  output  <- apiField(request, "output", list())
  unit <- apiChoice(apiField(request, "timeUnit", TIME_UNIT_DEFAULT), "timeUnit", names(TIME_UNITS))

  duration <- apiField(request, "duration")
  if (is.null(duration)) apiError("`duration` is required: the length of the simulation in ", unit, ".")
  maximum <- apiNumber(duration, "duration", 0, Inf) * TIME_UNITS[[unit]]
  longest <- max(MAX_TIME_VALUES)
  if (maximum <= 0 || maximum > longest) {
    apiError("`duration` must be greater than 0 and at most ", signif(longest / TIME_UNITS[[unit]], 4),
             " ", unit, " (", longest / MINS_PER_DAY, " days).")
  }

  patient <- apiPatient(apiField(request, "patient"))
  DT <- apiDoses(apiField(request, "doses"), unit, maximum,
                 allowIllicit = apiFlag(apiField(opts, "allowIllicitDrugs", FALSE),
                                        "options.allowIllicitDrugs"))
  ET <- apiEvents(apiField(request, "events"), unit)
  recovery <- apiFlag(apiField(output, "recovery", FALSE), "output.recovery")
  times <- apiOutputTimes(output, unit, maximum)

  warnings <- character(0)
  late <- unique(DT$Drug[DT$Time >= maximum])
  if (length(late) > 0) {
    warnings <- c(warnings, paste0(
      "Doses at or after the end of the simulation are ignored (",
      paste(late, collapse = ", "), ")."))
  }

  sim <- withCallingHandlers(
    tryCatch(
      withOutputTimes(times, simulateDrugsWithCovariates(
        DT, ET, patient$weight, patient$height, patient$age, patient$sex,
        maximum, recovery,
        cyp2d6 = patient$cyp2d6, cyp2c19 = patient$cyp2c19, cyp2c9 = patient$cyp2c9,
        adjustToFFM = patient$adjustToFFM, osmolality = patient$osmolality,
        creatinine = patient$creatinine, pregnant = patient$pregnant)),
      error = function(e) {
        if (inherits(e, "stanpumpr_api_error")) stop(e)
        apiError("The simulation failed: ", conditionMessage(e), status = 422L)
      }),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  sim <- Filter(function(d) !is.null(d$wide) && nrow(d$wide) > 0, sim)

  series <- lapply(names(sim), function(drug) {
    s <- apiSeries(sim[[drug]]$wide, times, recovery)
    cbind(Drug = drug, s)
  })
  timeseries <- do.call(rbind, series)
  rownames(timeseries) <- NULL

  given <- unique(DT$Drug)
  peak <- function(t, y) {
    if (all(is.na(y))) return(c(NA_real_, NA_real_))
    i <- which.max(y)
    c(y[i], t[i])
  }
  drugs <- do.call(rbind, lapply(names(sim), function(drug) {
    d <- sim[[drug]]
    w <- d$wide
    cp <- peak(w$Time, w$Plasma)
    ce <- peak(w$Time, w$`Effect Site`)
    data.frame(
      Drug               = drug,
      Dosed              = drug %in% given,
      ConcentrationUnits = apiConcentrationUnits(d$Concentration.Units),
      MEAC               = if (is.null(d$MEAC) || d$MEAC == 0) NA_real_ else d$MEAC,
      PeakPlasma         = cp[1],
      TimePeakPlasma     = cp[2],
      PeakEffectSite     = ce[1],
      TimePeakEffectSite = ce[2],
      stringsAsFactors   = FALSE
    )
  }))
  timeseries$ConcentrationUnits <- drugs$ConcentrationUnits[match(timeseries$Drug, drugs$Drug)]

  doses <- exportDoseTable(DT, sim)
  doses <- doses[, c("Drug", "Time", "Dose", "Units")]
  doses$Time <- as.numeric(doses$Time)

  list(
    meta = list(
      apiVersion       = API_VERSION,
      stanpumpRVersion = as.character(utils::packageVersion("stanpumpR")),
      generated        = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      timeUnit         = "minutes",
      duration         = maximum,
      grid             = if (is.null(times)) "native" else "requested",
      reference        = "https://stanpumpr.io"
    ),
    patient    = list(
      weight = patient$weight, weightUnit = "kg",
      height = patient$height, heightUnit = "cm",
      age    = patient$age,    ageUnit    = "years",
      sex    = patient$sex,
      pregnant = patient$pregnant,
      adjustToFFM = patient$adjustToFFM,
      osmolality  = patient$osmolality,
      creatinine  = if (is.null(patient$creatinine)) NA_real_ else patient$creatinine,
      cyp2d6 = patient$cyp2d6, cyp2c19 = patient$cyp2c19, cyp2c9 = patient$cyp2c9
    ),
    drugs      = drugs,
    doses      = doses,
    events     = ET,
    timeseries = timeseries,
    warnings   = unique(warnings)
  )
}

# The time series with one row per time and two columns per drug
# ("propofol Plasma", "propofol EffectSite"); only when every drug shares the
# same times, i.e. not with output.interval = "native".
apiWideSeries <- function(result) {
  ts <- result$timeseries
  if (identical(result$meta$grid, "native")) {
    apiError("layout = \"wide\" needs a common time grid; use an output.interval or output.times, ",
             "not \"native\".")
  }
  value <- intersect(c("Plasma", "EffectSite", "Recovery"), names(ts))
  out <- data.frame(Time = ts$Time[ts$Drug == ts$Drug[1]])
  for (drug in unique(ts$Drug)) {
    s <- ts[ts$Drug == drug, ]
    for (v in value) out[[paste(drug, v)]] <- s[[v]]
  }
  out
}

apiKeyValue <- function(x) {
  data.frame(Field = names(x),
             Value = vapply(x, function(v) if (length(v) == 0 || is.na(v)) "" else as.character(v),
                            character(1)),
             stringsAsFactors = FALSE, row.names = NULL)
}

apiXmlEscape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

apiXmlValue <- function(v) {
  if (is.logical(v)) return(ifelse(is.na(v), "", tolower(as.character(v))))
  if (is.numeric(v)) return(ifelse(is.na(v), "", as.character(v)))
  ifelse(is.na(v), "", as.character(v))
}

# One element per data frame row, the columns as attributes:
#   <drug Drug="propofol" ConcentrationUnits="mcg/mL" ... />
apiXmlRows <- function(df, element) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  cols <- lapply(df, apiXmlValue)
  attrs <- Map(function(name, v) paste0(" ", gsub("[^A-Za-z0-9_]", "_", name), "=\"",
                                        apiXmlEscape(v), "\""), names(cols), cols)
  paste0("    <", element, do.call(paste0, unname(attrs)), "/>")
}

apiXmlList <- function(x, element) {
  vals <- vapply(x, function(v) apiXmlValue(v)[1], character(1))
  paste0("  <", element, paste0(" ", names(x), "=\"", apiXmlEscape(vals), "\"", collapse = ""), "/>")
}

apiXml <- function(result) {
  section <- function(name, rows) c(paste0("  <", name, ">"), rows, paste0("  </", name, ">"))
  paste(c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<stanpumpR>",
    apiXmlList(result$meta, "meta"),
    apiXmlList(result$patient, "patient"),
    section("drugs", apiXmlRows(result$drugs, "drug")),
    section("doses", apiXmlRows(result$doses, "dose")),
    section("events", apiXmlRows(result$events, "event")),
    section("timeseries", apiXmlRows(result$timeseries, "point")),
    section("warnings", if (length(result$warnings)) {
      paste0("    <warning>", apiXmlEscape(result$warnings), "</warning>")
    }),
    "</stanpumpR>"
  ), collapse = "\n")
}

apiCsv <- function(df) {
  con <- textConnection("out", "w", local = TRUE)
  on.exit(close(con))
  utils::write.csv(df, con, row.names = FALSE, na = "")
  paste(c(textConnectionValue(con), ""), collapse = "\n")
}

apiXlsx <- function(result, series) {
  wb <- openxlsx::createWorkbook("stanpumpR")
  sheet <- function(name, df) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, df)
  }
  sheet("Time Series", series)
  sheet("Drugs", result$drugs)
  sheet("Doses", result$doses)
  if (nrow(result$events) > 0) sheet("Events", result$events)
  sheet("Patient", apiKeyValue(result$patient))
  sheet("About", rbind(apiKeyValue(result$meta),
                       if (length(result$warnings)) {
                         data.frame(Field = "warning", Value = result$warnings)
                       }))
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  readBin(path, "raw", file.info(path)$size)
}

#' Serialise a simulation result
#'
#' @param result the value of [apiSimulate()]
#' @param format one of `"json"` (everything, nested), `"csv"` (the time
#'   series only: one table), `"xlsx"` (a workbook: Time Series, Drugs, Doses,
#'   Events, Patient, About) or `"xml"` (everything, one element per row)
#' @param layout for CSV and Excel, `"long"` (one row per drug and time, the
#'   default) or `"wide"` (one row per time, a column per drug and site)
#' @returns a list: `body` (a string, or a raw vector for xlsx), `contentType`
#'   and `filename`
#' @export
apiFormatResult <- function(result, format = "json", layout = "long") {
  format <- apiChoice(tolower(format), "format", API_FORMATS)
  layout <- apiChoice(tolower(layout), "layout", API_LAYOUTS)
  series <- if (layout == "wide") apiWideSeries(result) else result$timeseries
  body <- switch(format,
    json = as.character(jsonlite::toJSON(result, dataframe = "rows", auto_unbox = TRUE,
                                         na = "null", null = "null", digits = I(10),
                                         pretty = FALSE)),
    csv  = apiCsv(series),
    xlsx = apiXlsx(result, series),
    xml  = apiXml(result)
  )
  list(body = body, contentType = API_CONTENT_TYPES[[format]],
       filename = paste0("stanpumpR-simulation.", format))
}

#' Write a simulation result to a file
#'
#' @param result the value of [apiSimulate()]
#' @param path the file to write; its extension (.json, .csv, .xlsx, .xml)
#'   picks the format
#' @inheritParams apiFormatResult
#' @returns `path`, invisibly
#' @export
apiWriteResult <- function(result, path, layout = "long") {
  out <- apiFormatResult(result, tolower(tools::file_ext(path)), layout)
  if (is.raw(out$body)) writeBin(out$body, path) else writeLines(out$body, path, sep = "")
  invisible(path)
}

#' The drugs the API can simulate
#'
#' @param allowIllicit include the opt-in `Illicit drugs` category
#' @returns a data frame: Drug, Category, ConcentrationUnits and Units (the
#'   dose units the drug takes, comma separated)
#' @export
apiDrugCatalog <- function(allowIllicit = FALSE) {
  dd <- getDrugDefaultsGlobal()
  keep <- !isGasDrug(dd$Drug) & (allowIllicit | !isIllicitDrug(dd$Drug, dd))
  dd <- dd[keep, ]
  out <- data.frame(
    Drug               = dd$Drug,
    Category           = dd$Category,
    ConcentrationUnits = vapply(dd$Concentration.Units, apiConcentrationUnits, character(1)),
    Units              = vapply(dd$Units, paste, character(1), collapse = ","),
    stringsAsFactors   = FALSE, row.names = NULL
  )
  out[order(tolower(out$Drug), method = "radix"), , drop = FALSE]
}

# The format a request asked for: `?format=`, then the request's
# output.format, then the Accept header, then JSON.
apiNegotiateFormat <- function(query = NULL, request = NULL, accept = NULL) {
  if (!is.null(query) && nzchar(query)) return(tolower(query))
  fromBody <- apiField(apiField(request, "output", list()), "format")
  if (!is.null(fromBody)) return(tolower(as.character(fromBody)))
  if (!is.null(accept)) {
    for (f in API_FORMATS) if (grepl(API_CONTENT_TYPES[[f]], accept, fixed = TRUE)) return(f)
    if (grepl("text/xml", accept, fixed = TRUE)) return("xml")
  }
  "json"
}

#' Run the simulation API as an HTTP service
#'
#' Serves `POST /simulate`, `GET /drugs` and `GET /health` (see
#' `docs/api.md`), with interactive documentation at `/__docs__/`.  Needs the
#' plumber package.
#'
#' @param host,port where to listen
#' @param docs serve the interactive (Swagger) documentation
#' @returns does not return while the service runs
#' @export
run_api <- function(host = "127.0.0.1", port = 8000, docs = TRUE) {
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("run_api() needs the plumber package: install.packages(\"plumber\")")
  }
  file <- system.file("plumber", "plumber.R", package = "stanpumpR")
  # A request is a dose table of at most MAX_DOSE_ROWS rows: 1 MB is ample.
  plumber::options_plumber(maxRequestSize = 1e6)
  api <- plumber::pr(file)
  api <- plumber::pr_set_docs(api, docs)
  plumber::pr_run(api, host = host, port = port)
}
