# The TCI API: the next infusion rate for one drug in one patient (tci-api.R)
#
# Drafted by Claude Code, 2026-10-12, at the request of Steven L. Shafer.  The
# specification is docs/tci-api.md (version 1.0); the tests are
# tests/testthat/test-tci-api.R.  Pure R: no Shiny, no plumber.  The HTTP
# router that serves these functions is tci-api-server.R.
#
# WHAT IT IS FOR
# ==============
# A target-controlled-infusion controller -- the host computer driving a
# syringe pump, or the pump itself -- calls tciNextRate() once per update
# interval with everything it knows: the patient, the syringe, the pump's
# limits, the target, how long a new command takes to reach the pump, and the
# whole record of what the pump has delivered.  It gets back the rate to run
# for the coming interval, how long to run it, what to run if the next answer
# does not arrive, and the predicted concentrations.
#
# WHY IT IS STATELESS
# ===================
# The server keeps nothing between requests.  The model state -- seven
# exponential amplitudes, three for the plasma and four for the effect site --
# is re-derived from the delivery record on every request.  This is the
# principle of the original STANPUMP: infuse() read the volume infused back
# from the pump every ten seconds and brought the model forward with what had
# actually been delivered, never with what it had asked for.  So a refused
# rate, an occlusion, a dropped request or a server restart cost nothing: the
# next request carries the whole truth and the answer is exact.  Each request
# is also a complete, reproducible record of why the pump did what it did:
# tciNextRate(body) in R replays it.
#
# The state is built by superposition rather than by walking the timeline.
# A constant rate R over [a, b] (both at or before now, taken as 0) leaves the
# amplitude of the exponential with eigenvalue lambda at
#
#     coef * R * (exp(lambda b) - exp(lambda a))
#     = coef * R * exp(lambda b) * (1 - exp(-lambda (b - a)))
#
# and a bolus A at time t leaves coef * A * exp(lambda t).  The system is
# linear, so the state at now is the sum over every piece of the record: one
# vectorised step whatever the length of the history, and the same numbers,
# to rounding, as tciSchedule()'s interval-by-interval recurrence (test group
# 1).  The second form is the one computed: exp(lambda b) is at most one and
# -expm1() is exact for a short piece, where the first form loses digits to
# cancellation and overflows for a piece that is both long and old.
#
# WHY THE DELAY IS MODELLED
# =========================
# A command formed at `now` reaches the pump `latency` seconds later, and the
# pump keeps running the OLD rate until then.  The plan is therefore made from
# the state at now + latency (STANPUMP's "preliminary calculation"), and the
# response says when the new rate starts (effectiveAt), when it must stop
# (holdUntil) and when to ask again (nextRequestAt).  The cost of a wrong
# latency is one-sided -- underestimating it lets a loading rate run longer
# than planned, and the effect site cannot give drug back -- so the response
# also carries the fallback rate the pump should run if the next command is
# late, and the hold is a hard stop.  Holding a loading rate one interval too
# long doubles the loading dose (test group 4).
#
# THE SAME CONTROLLER AS THE APP
# ==============================
# The rate is tciRate() from tci.R, the function behind the app's target rows
# (Shafer & Gregg 1992), and the predictions use the same closed form.  With
# zero latency, a 10 s interval and an unlimited pump, a closed loop of these
# calls reproduces the app's tciSchedule() rate for rate.
#
# UNITS AND TIME
# ==============
# Inside, everything is in minutes and the drug's base mass unit, as in
# simCpCe(): mg for a drug measured in mcg/mL, mcg for one measured in ng/mL.
# A rate is base/min and a concentration base/L, so the target in the drug's
# own concentration units is numerically the target in base/L.  Times arrive
# as RFC 3339 strings on the controller's clock, are parsed to seconds since
# the epoch, and every piece of arithmetic is then done in minutes relative to
# `now`, in double precision, before anything is exponentiated.  The server's
# own clock is only reported.
#
# ERRORS AND WARNINGS
# ===================
# A refused request is a classed condition, tci_api_error, carrying the HTTP
# status, a code, a message and an RFC 6901 JSON pointer to the offending
# field.  Validation stops at the first failure, in dependency order: the
# body's structure (duplicate and unknown keys), then the drug, then each
# block in turn.  What the controller should know but what does not stop the
# plan -- a cross-check that disagrees with the record, a pump that is not
# running, a target above the usual range -- is collected as a warning and
# returned in the body.


# ---------------------------------------------------------------------------
# Errors and warnings

# Signal a TCI API error: a condition of class tci_api_error (also an error)
# with the HTTP status, the code of docs/tci-api.md section 6.3, a message and
# the JSON pointer of the field at fault.
tciApiStop <- function(status, code, message, pointer = NULL)
{
  stop(structure(
    class = c("tci_api_error", "error", "condition"),
    list(message = message, call = NULL, status = as.integer(status),
         code = code, pointer = pointer)))
}

# The warnings of one request accumulate in an environment, so that the
# validation and the planning can add to it as they go.
tciApiWarnings <- function()
{
  w <- new.env(parent = emptyenv())
  w$items <- list()
  w
}

tciApiWarn <- function(w, code, message, pointer = NULL)
{
  w$items[[length(w$items) + 1L]] <-
    list(code = code, message = message, pointer = pointer)
  invisible(w)
}

# The body of an error response (section 6.3), for the router: one error,
# the request id when the request carried a usable one, and the server block.
tciApiErrorBody <- function(e, receivedAt = Sys.time(), respondedAt = Sys.time())
{
  list(
    apiVersion = TCI_API_VERSION,
    requestId  = e$requestId,
    errors     = list(list(code = e$code, message = conditionMessage(e),
                           pointer = e$pointer)),
    server     = tciApiServerBlock(receivedAt, respondedAt, compute = FALSE)
  )
}


# ---------------------------------------------------------------------------
# Time

# Seconds since the epoch.  POSIXct, or already a number of seconds.
tciApiSeconds <- function(t) as.numeric(t)

# Parse RFC 3339 strings to seconds since the epoch, all at once.  `pointerFor`
# names the pointer of element i for the error.  NA strings stay NA (an absent
# optional time); every other string must carry a Z or a numeric offset,
# which a lenient parser would not insist on.  The form is checked by one
# regular expression, with each field held to its RFC 3339 range (a 24th hour
# or a +24:00 offset would otherwise parse and silently shift the clock; the
# 60th second is kept for a leap second), and then parsed with one exact
# format (lubridate::fast_strptime(), which does no format guessing and so
# costs the same per string for one string as for twenty thousand).  The
# common spelling -- upper-case T and a trailing Z -- is parsed as it is; only
# the other spellings are first put into one canonical form, because the
# string substitutions cost more than the parse itself on a long history.
# The pattern is TCI_API_TIME_PATTERN (constants.R), also in the OpenAPI
# document.
tciApiParseTimes <- function(strs, pointerFor)
{
  out <- rep(NA_real_, length(strs))
  has <- !is.na(strs)
  if (!any(has)) return(out)
  bad <- which(has & !grepl(TCI_API_TIME_PATTERN, strs, perl = TRUE))
  if (length(bad))
    tciApiStop(422L, "TIME_INVALID",
               sprintf("%s: '%s' is not an RFC 3339 time with a Z or a numeric offset, such as 2026-10-11T14:32:05.250Z",
                       pointerFor(bad[1]), strs[bad[1]]), pointerFor(bad[1]))
  s <- strs[has]
  secs <- rep(NA_real_, length(s))
  plain <- endsWith(s, "Z") & substr(s, 11L, 11L) == "T"
  if (any(plain))
    secs[plain] <- as.numeric(lubridate::fast_strptime(s[plain], "%Y-%m-%dT%H:%M:%OSZ",
                                                       tz = "UTC", lt = FALSE))
  if (!all(plain)) {
    # One canonical spelling: T, a +hhmm offset (Z is +0000), fractional
    # seconds optional.
    canon <- chartr("tz ", "TZT", s[!plain])
    canon <- sub("Z$", "+00:00", canon)
    canon <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", canon)
    secs[!plain] <- as.numeric(lubridate::fast_strptime(canon, "%Y-%m-%dT%H:%M:%OS%z",
                                                        tz = "UTC", lt = FALSE))
  }
  bad <- which(has)[is.na(secs)]
  if (length(bad))
    tciApiStop(422L, "TIME_INVALID",
               sprintf("%s: '%s' is not a valid time", pointerFor(bad[1]), strs[bad[1]]),
               pointerFor(bad[1]))
  bad <- which(has)[secs < 0]
  if (length(bad))
    tciApiStop(422L, "TIME_INVALID",
               sprintf("%s: '%s' is earlier than 1970", pointerFor(bad[1]), strs[bad[1]]),
               pointerFor(bad[1]))
  out[has] <- secs
  out
}

# A single required time: the string checks, then the parse.
tciApiParseTime <- function(x, pointer)
{
  tciApiParseTimes(tciApiTimeString(x, pointer), function(i) pointer)
}

# A time field as a string, before parsing.  A JSON number is refused: a
# seconds-versus-milliseconds mistake would not be survivable.
tciApiTimeString <- function(x, pointer, required = TRUE)
{
  if (is.null(x)) {
    if (required) tciApiStop(400L, "MISSING_FIELD", sprintf("%s is required", pointer), pointer)
    return(NA_character_)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x))
    tciApiStop(400L, "INVALID_TYPE",
               sprintf("%s must be an RFC 3339 string such as 2026-10-11T14:32:05.250Z; a JSON number is not accepted",
                       pointer), pointer)
  x
}

# RFC 3339 in UTC with milliseconds.  %OSn truncates rather than rounds, so
# the value is rounded to the millisecond first and nudged up by half of one:
# 26.9995 prints as 27.000, not 26.999.
tciApiFormatTime <- function(secs)
{
  if (is.null(secs) || length(secs) != 1L || !is.finite(secs)) return(NULL)
  secs <- round(secs * 1000) / 1000 + 5e-4
  format(as.POSIXct(secs, origin = "1970-01-01", tz = "UTC"),
         "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}


# ---------------------------------------------------------------------------
# JSON shapes and scalar checks
#
# The body arrives as jsonlite::fromJSON(txt, simplifyVector = FALSE) gives it:
# an object is a named list (an empty one has names of length zero), an array
# an unnamed list, a number numeric, true/false logical, and null NULL, which
# the API treats as absent.

tciApiIsObject <- function(x) is.list(x) && !is.null(names(x))
tciApiIsArray  <- function(x) is.list(x) && is.null(names(x))

tciApiMissing <- function(pointer)
  tciApiStop(400L, "MISSING_FIELD", sprintf("%s is required", pointer), pointer)

tciApiObject <- function(x, pointer, required = TRUE)
{
  if (is.null(x)) {
    if (required) tciApiMissing(pointer)
    return(NULL)
  }
  if (!tciApiIsObject(x))
    tciApiStop(400L, "INVALID_TYPE", sprintf("%s must be a JSON object", pointer), pointer)
  x
}

# A finite JSON number within bounds.  `code` is the code of a bounds failure
# (INVALID_VALUE unless the specification names another).
tciApiNumber <- function(x, pointer, required = TRUE, min = -Inf, max = Inf,
                         openMin = FALSE, openMax = FALSE, code = "INVALID_VALUE",
                         maxName = NULL)
{
  if (is.null(x)) {
    if (required) tciApiMissing(pointer)
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x))
    tciApiStop(400L, "INVALID_TYPE", sprintf("%s must be a JSON number", pointer), pointer)
  x <- as.numeric(x)
  if (!is.finite(x))
    tciApiStop(422L, "INVALID_VALUE", sprintf("%s must be finite", pointer), pointer)
  below <- if (openMin) x <= min else x < min
  above <- if (openMax) x >= max else x > max
  if (below || above) {
    lo <- if (is.finite(min)) sprintf("%s %g", if (openMin) ">" else ">=", min)
    hi <- if (is.finite(max)) sprintf("%s %s", if (openMax) "<" else "<=",
                                      if (is.null(maxName)) format(max) else sprintf("%s (%g)", maxName, max))
    tciApiStop(422L, code, sprintf("%s must be %s", pointer, paste(c(lo, hi), collapse = " and ")),
               pointer)
  }
  x
}

tciApiString <- function(x, pointer, required = TRUE, allowed = NULL,
                         maxLength = NULL, code = "INVALID_VALUE")
{
  if (is.null(x)) {
    if (required) tciApiMissing(pointer)
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x))
    tciApiStop(400L, "INVALID_TYPE", sprintf("%s must be a JSON string", pointer), pointer)
  if (!is.null(maxLength) && nchar(x, type = "chars") > maxLength)
    tciApiStop(422L, "INVALID_VALUE",
               sprintf("%s must be at most %d characters", pointer, maxLength), pointer)
  if (!is.null(allowed) && !(x %in% allowed))
    tciApiStop(422L, code,
               sprintf("%s must be one of %s (exactly, case-sensitive)", pointer,
                       paste0("'", allowed, "'", collapse = ", ")), pointer)
  x
}

tciApiLogical <- function(x, pointer, required = TRUE)
{
  if (is.null(x)) {
    if (required) tciApiMissing(pointer)
    return(NULL)
  }
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    tciApiStop(400L, "INVALID_TYPE", sprintf("%s must be true or false", pointer), pointer)
  x
}

# A unit token from one of the closed lists of section 4.4.  Anything else is
# UNIT_UNKNOWN, with the nearest accepted token named: a micro sign (U+00B5)
# or Greek mu (U+03BC) is read as "mc" first, so that a microgram written
# with either is told "mcg" rather than "mg".  The file stays ASCII.
tciApiUnits <- function(x, pointer, tokens, required = TRUE)
{
  x <- tciApiString(x, pointer, required)
  if (is.null(x)) return(NULL)
  if (!(x %in% tokens))
    tciApiStop(422L, "UNIT_UNKNOWN",
               sprintf("%s: '%s' is not an accepted unit token; the nearest accepted token is '%s' (accepted here: %s)",
                       pointer, x, tciApiNearestToken(x, tokens),
                       paste(tokens, collapse = ", ")), pointer)
  x
}

tciApiNearestToken <- function(x, tokens)
{
  y <- gsub("\u00b5|\u03bc", "mc", x)
  tokens[which.min(utils::adist(tolower(y), tolower(tokens)))]
}

# { value >= 0, units } (pump.volumeInfused, pump.syringeVolumeRemaining).
tciApiQuantity <- function(x, pointer, tokens)
{
  x <- tciApiObject(x, pointer, required = FALSE)
  if (is.null(x)) return(NULL)
  list(value = tciApiNumber(x$value, paste0(pointer, "/value"), min = 0),
       units = tciApiUnits(x$units, paste0(pointer, "/units"), tokens))
}

# { concentration >= TCI_API_MIN_INFUSATE, units in mg/mL or mcg/mL }: the
# syringe.  The floor is a thousandth of a unit per mL, far below any syringe
# a pump is loaded with: a concentration of 1e-305 is a finite number that
# would turn every millilitre into a rate the arithmetic cannot carry.
tciApiInfusate <- function(x, pointer, required = FALSE)
{
  x <- tciApiObject(x, pointer, required)
  if (is.null(x)) return(NULL)
  list(concentration = tciApiNumber(x$concentration, paste0(pointer, "/concentration"),
                                    min = TCI_API_MIN_INFUSATE),
       units = tciApiUnits(x$units, paste0(pointer, "/units"), TCI_API_INFUSATE_UNITS))
}


# ---------------------------------------------------------------------------
# The body's keys
#
# Every object in the body has a closed set of keys.  A duplicate key is
# contradictory input and an unknown key is most likely a typo that would
# otherwise fall back silently to a default, so both are refused before any
# value is read.

tciApiSchema <- function()
{
  infusate <- list(keys = c("concentration", "units"))
  quantity <- list(keys = c("value", "units"))
  list(
    keys = c("requestId", "drug", "now", "patient", "infusate", "pump", "target",
             "timing", "history", "lookAhead", "options"),
    children = list(
      patient  = list(keys = c("weight", "height", "age", "sex")),
      infusate = infusate,
      pump     = list(keys = c("rateUnits", "maxRate", "minRate", "rateResolution",
                               "currentRate", "state", "volumeInfused",
                               "syringeVolumeRemaining"),
                      children = list(volumeInfused = quantity,
                                      syringeVolumeRemaining = quantity)),
      target   = list(keys = c("site", "value", "units")),
      timing   = list(keys = c("latency", "interval")),
      history  = list(array = TRUE,
                      item = list(keys = c("time", "rate", "units", "from", "to",
                                           "delivered", "bolus", "infusate", "source"),
                                  children = list(infusate = infusate))),
      lookAhead = list(keys = "concentration"),
      options   = list(keys = "adjustToFFM")
    )
  )
}

tciApiCheckKeys <- function(x, pointer, node)
{
  if (isTRUE(node$array)) {
    tciApiCheckArrayKeys(x, pointer, node$item)
    return(invisible())
  }
  if (!tciApiIsObject(x)) return(invisible())
  where <- if (pointer == "") "the body" else pointer
  nm <- names(x)
  dup <- nm[duplicated(nm)]
  if (length(dup))
    tciApiStop(400L, "DUPLICATE_KEY",
               sprintf("duplicate key '%s' in %s", dup[1], where), paste0(pointer, "/", dup[1]))
  unknown <- setdiff(nm, node$keys)
  if (length(unknown))
    tciApiStop(400L, "UNKNOWN_FIELD",
               sprintf("unknown field '%s' in %s; API version %s accepts %s", unknown[1], where,
                       TCI_API_VERSION, paste(node$keys, collapse = ", ")),
               paste0(pointer, "/", unknown[1]))
  for (key in intersect(nm, names(node$children)))
    tciApiCheckKeys(x[[key]], paste0(pointer, "/", key), node$children[[key]])
  invisible()
}

# The same for every object of an array (the history), in a few vectorised
# steps rather than one recursion per event.
tciApiCheckArrayKeys <- function(x, pointer, item)
{
  if (!tciApiIsArray(x) || length(x) == 0) return(invisible())
  allNames <- lapply(x, names)
  isObj <- vapply(x, is.list, NA, USE.NAMES = FALSE) &
    !vapply(allNames, is.null, NA, USE.NAMES = FALSE)
  idx <- which(isObj)
  nms <- allNames[idx]
  all <- unlist(nms, use.names = FALSE)
  event <- rep(seq_along(nms), lengths(nms))
  # Two keys collide when they are the same name in the same event: one
  # integer per (event, name) pair, not a pasted string per pair.
  distinct <- unique(all)
  dup <- which(duplicated(as.numeric(event) * (length(distinct) + 1) + match(all, distinct)))
  if (length(dup)) {
    at <- paste0(pointer, "/", idx[event[dup[1]]] - 1L)
    tciApiStop(400L, "DUPLICATE_KEY", sprintf("duplicate key '%s' in %s", all[dup[1]], at),
               paste0(at, "/", all[dup[1]]))
  }
  bad <- which(!(all %in% item$keys))
  if (length(bad)) {
    at <- paste0(pointer, "/", idx[event[bad[1]]] - 1L)
    tciApiStop(400L, "UNKNOWN_FIELD",
               sprintf("unknown field '%s' in %s; API version %s accepts %s", all[bad[1]], at,
                       TCI_API_VERSION, paste(item$keys, collapse = ", ")),
               paste0(at, "/", all[bad[1]]))
  }
  for (key in names(item$children)) {
    for (j in unique(event[all == key]))
      tciApiCheckKeys(x[[idx[j]]][[key]], paste0(pointer, "/", idx[j] - 1L, "/", key),
                      item$children[[key]])
  }
  invisible()
}


# ---------------------------------------------------------------------------
# Units
#
# The drug's base mass unit is mg for a drug measured in mcg/mL and mcg for one
# measured in ng/mL (simCpCe()).  A token is parsed into its mass (or mL), a
# per-kilogram flag and a per-hour flag, so one converter serves amounts and
# rates alike: an amount token has no time part and converts to base, a rate
# token converts to base per minute.  mL goes through the syringe's
# concentration, in base per mL, which may differ from piece to piece of the
# history when the syringe was changed.

tciApiBaseUnits <- function(concentrationUnits)
{
  ng <- identical(concentrationUnits, "ng")
  list(mass = if (ng) "mcg" else "mg",
       concentration = if (ng) "ng/mL" else "mcg/mL")
}

# One unit of each mass token, in base units (vectorised over tokens).
tciApiMassFactor <- function(token, base)
{
  ifelse(token == base, 1, ifelse(token == "mg", 1000, 1 / 1000))
}

# The syringe's concentration in base per mL.  `infusate` is a list (or a
# data frame of several) with concentration and units; NA when there is none.
tciApiInfusateBase <- function(infusate, base)
{
  if (is.null(infusate)) return(NA_real_)
  infusate$concentration * tciApiMassFactor(sub("/mL$", "", infusate$units), base)
}

# How many base units (per minute, for a rate token) one unit of each token
# is.  `concBase` is the syringe concentration in force, base per mL, needed
# only for the mL tokens; vectorised over value, units and concBase.  The
# token list is closed and short, so each token is taken apart once and the
# twenty thousand units of a long history are then a single match() against
# it, rather than twenty thousand strsplit()s.  A token that is not on the
# list (the validation has already refused it) gives NA.
tciApiUnitFactor <- function(units, base, weight, concBase)
{
  tokens <- unique(c(TCI_API_RATE_UNITS, TCI_API_BOLUS_UNITS))
  parts <- strsplit(tokens, "/", fixed = TRUE)
  k <- match(units, tokens)
  mass <- vapply(parts, `[`, "", 1L)[k]
  perKg <- vapply(parts, function(p) "kg" %in% p, NA)[k]
  perHour <- vapply(parts, function(p) p[length(p)] == "hr", NA)[k]
  f <- ifelse(mass == "mL", concBase, tciApiMassFactor(mass, base))
  f * ifelse(perKg, weight, 1) / ifelse(perHour, 60, 1)
}

tciApiToBase <- function(value, units, base, weight, concBase)
{
  value * tciApiUnitFactor(units, base, weight, concBase)
}

tciApiFromBase <- function(value, units, base, weight, concBase)
{
  value / tciApiUnitFactor(units, base, weight, concBase)
}


# ---------------------------------------------------------------------------
# The drug

tciApiDrugNames <- function()
{
  g <- getDrugDefaultsGlobal()
  g$Drug[vapply(g$Units, function(u) any(isTciUnit(u)), logical(1))]
}

# The path's drug, which the body's `drug` must match.
tciApiCheckDrug <- function(drug)
{
  if (is.null(drug))
    tciApiStop(400L, "MISSING_FIELD",
               "drug is required: the path's drug, or `drug` in the body", "/drug")
  if (!is.character(drug) || length(drug) != 1L || is.na(drug))
    tciApiStop(400L, "INVALID_TYPE", "drug must be a string", "/drug")
  if (!(drug %in% getDrugDefaultsGlobal()$Drug))
    tciApiStop(404L, "UNKNOWN_DRUG",
               sprintf("'%s' is not a drug in the stanpumpR library", drug), "/drug")
  tci <- tciApiDrugNames()
  if (!(drug %in% tci))
    tciApiStop(422L, "DRUG_NOT_TCI",
               sprintf("%s has no target-controlled infusion in stanpumpR; the TCI drugs are %s",
                       drug, paste(tci, collapse = ", ")), "/drug")
  drug
}

# The PK object for a patient, kept for the next request with the same
# covariates: a controller asks every few seconds for the same patient, and
# getDrugPK() is the expensive step (the ke0 solve).  The cache is one
# environment keyed by the covariates, holding at most TCI_API_PK_CACHE_SIZE
# entries (about 11 KB each): a client that varied its covariates on every
# request would otherwise grow the server without limit.  When it is full the
# entry inserted first goes; a server running a fleet of pumps keeps the PK
# of every current patient, and a miss costs a few milliseconds.
tciApiPKCache <- new.env(parent = emptyenv())

tciApiPKKey <- function(drug, weight, height, age, sex, adjustToFFM)
  paste(drug, weight, height, age, sex, adjustToFFM, sep = "|")

tciApiPK <- function(drug, weight, height, age, sex, adjustToFFM)
{
  key <- tciApiPKKey(drug, weight, height, age, sex, adjustToFFM)
  hit <- tciApiPKCache[[key]]
  if (!is.null(hit)) return(hit$PK)
  PK <- getDrugPK(drug, weight, height, age, sex, getDrugDefaults(drug),
                  adjustToFFM = adjustToFFM)
  keys <- names(tciApiPKCache)
  stamps <- vapply(keys, function(k) tciApiPKCache[[k]]$stamp, 0, USE.NAMES = FALSE)
  if (length(keys) >= TCI_API_PK_CACHE_SIZE)
    rm(list = keys[which.min(stamps)], envir = tciApiPKCache)
  assign(key, list(PK = PK, stamp = if (length(stamps)) max(stamps) + 1 else 1),
         envir = tciApiPKCache)
  PK
}

# The keys the cache holds, oldest first (for the tests), and a way to empty it.
tciApiPKCacheKeys <- function()
{
  keys <- names(tciApiPKCache)
  keys[order(vapply(keys, function(k) tciApiPKCache[[k]]$stamp, 0, USE.NAMES = FALSE))]
}

tciApiPKCacheClear <- function()
  rm(list = names(tciApiPKCache), envir = tciApiPKCache)

# MD5 of the serialised parameters, so two responses from one model compare
# by a single string.  The parameters are rounded to ten significant digits
# first: the Eleveld model's exp() and pow() differ in their last binary digit
# between libm implementations (the first CI run hashed differently on Apple
# silicon than on Linux), and a hash that differs by platform for the same
# model would defeat its purpose.  Ten digits are far below any difference a
# pump could deliver.  Base R has no SHA-256; tools::md5sum reads a file.
tciApiHash <- function(x)
{
  x <- lapply(x, function(v) if (is.numeric(v)) signif(v, 10) else v)
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA), f)
  paste0("md5:", unname(tools::md5sum(f)))
}

# The `model` block of a response (section 6.1): what the pump displays and
# logs about the model it is being driven by.
tciApiModelBlock <- function(PK, adjustToFFM)
{
  k <- PK$PK[[1]]
  units <- tciApiBaseUnits(PK$Concentration.Units)
  parameters <- list(
    v1 = k$v1, v2 = k$v2, v3 = k$v3, cl1 = k$cl1, cl2 = k$cl2, cl3 = k$cl3,
    k10 = k$k10, k12 = k$k12, k13 = k$k13, k21 = k$k21, k31 = k$k31,
    lambda_1 = k$lambda_1, lambda_2 = k$lambda_2, lambda_3 = k$lambda_3,
    ke0 = k$ke0
  )
  list(
    drug               = PK$drug,
    reference          = PK$reference,
    concentrationUnits = units$concentration,
    massUnits          = units$mass,
    tPeak              = PK$tPeak,
    adjustToFFM        = adjustToFFM,
    parameters         = parameters,
    hash               = tciApiHash(parameters)
  )
}

# A number that may have arrived as a query-string parameter ("70") rather
# than a JSON number.  Anything that is not plainly numeric is left alone, for
# tciApiNumber() to refuse as INVALID_TYPE.
tciApiCoerceNumber <- function(x)
{
  if (is.character(x) && length(x) == 1L && !is.na(x) &&
      grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", x))
    return(as.numeric(x))
  x
}

# The JSON spellings only: strings are compared exactly (section 4.3), so a
# query string's TRUE is refused as INVALID_TYPE like any other string.
tciApiCoerceLogical <- function(x)
{
  if (is.character(x) && length(x) == 1L && x %in% c("true", "false"))
    return(x == "true")
  x
}


# ---------------------------------------------------------------------------
# The delivery history (section 4.2)
#
# Four kinds of event.  A rate change holds from its time to the next rate
# change, or to now.  A delivered segment is what the pump's volume counter
# reports between two times, modelled as a constant rate, and overrides the
# rate changes over its span.  A bolus is drug given outside the pump.  An
# infusate change is a syringe of a different concentration, in force from
# its time.
#
# Nothing here loops over the events.  The history is flattened to one row
# per field -- unlist(recursive = FALSE) keeps every field as exactly one
# element, a null and an empty object included, so the rows line up with the
# events' names -- and every check is then a vectorised comparison over those
# rows; the per-field helpers are called only on the offending row, to raise
# its error.  A history of twenty thousand events therefore costs a few tens
# of milliseconds (the budget of docs/tci-api.md section 7.1), where a loop
# with the same checks inline cost ten times that.

tciApiValidateHistory <- function(history, now, w)
{
  n <- length(history)
  kindKeys <- c("rate", "delivered", "bolus", "infusate")
  kindName <- c(rate = "rate change", delivered = "delivered segment",
                bolus = "bolus", infusate = "infusate change")
  itemKeys <- tciApiSchema()$children$history$item$keys
  allowed <- list(rate      = c("time", "rate", "units", "source"),
                  delivered = c("from", "to", "delivered", "units", "source"),
                  bolus     = c("time", "bolus", "units", "source"),
                  infusate  = c("time", "infusate", "source"))
  allowedMat <- t(vapply(allowed, function(a) itemKeys %in% a, logical(length(itemKeys))))
  unitsFor <- list(rate = TCI_API_RATE_UNITS, delivered = TCI_API_AMOUNT_UNITS,
                   bolus = TCI_API_BOLUS_UNITS)
  ptr <- function(i, key = NULL)
    paste0("/history/", i - 1L, if (!is.null(key)) paste0("/", key))

  # Every event is an object.  Anything that is not has no names, so only the
  # nameless events (an empty object among them) are looked at one by one.
  nms <- lapply(history, names)
  nameless <- which(lengths(nms) == 0L)
  isObj <- vapply(history[nameless], function(e) is.list(e) && !is.null(names(e)), NA,
                  USE.NAMES = FALSE)
  if (!all(isObj)) {
    i <- nameless[!isObj][1]
    tciApiStop(400L, "INVALID_TYPE", sprintf("%s must be a JSON object", ptr(i)), ptr(i))
  }

  # One row per field: its event, its key, its value.  A null value is an
  # absent key; an empty object or array is a present value of the wrong
  # type, which the field's own check refuses.
  ev <- rep(seq_len(n), lengths(nms))
  key <- unlist(nms, use.names = FALSE)
  vals <- unlist(history, recursive = FALSE, use.names = FALSE)
  if (is.null(vals)) vals <- list()
  if (length(vals) != length(key))      # not from JSON (a named atomic vector): take the slow, exact way
    vals <- lapply(seq_along(key), function(j) history[[ev[j]]][[key[j]]])
  present <- lengths(vals) > 0L
  empty <- which(!present)
  if (length(empty))
    present[empty] <- !vapply(vals[empty], is.null, NA, USE.NAMES = FALSE)

  # Exactly one of the four kind keys, and only the keys of that kind.
  isKind <- present & key %in% kindKeys
  bad <- which(tabulate(ev[isKind], nbins = n) != 1L)
  if (length(bad))
    tciApiStop(422L, "HISTORY_INVALID",
               sprintf("%s must be exactly one of a rate change (rate), a delivered segment (delivered), a bolus (bolus) or an infusate change (infusate)",
                       ptr(bad[1])), ptr(bad[1]))
  kind <- character(n)
  kind[ev[isKind]] <- key[isKind]
  fieldCode <- match(key, itemKeys)
  ok <- !present | (!is.na(fieldCode) & allowedMat[cbind(match(kind[ev], kindKeys), fieldCode)])
  bad <- which(!ok)
  if (length(bad)) {
    j <- bad[1]
    tciApiStop(422L, "HISTORY_INVALID",
               sprintf("%s: a %s does not take '%s'", ptr(ev[j]), kindName[[kind[ev[j]]]], key[j]),
               ptr(ev[j], key[j]))
  }

  isString <- function(idx)
    vapply(vals[idx], is.character, NA, USE.NAMES = FALSE) & lengths(vals[idx]) == 1L
  isNumber <- function(idx)
    vapply(vals[idx], is.numeric, NA, USE.NAMES = FALSE) & lengths(vals[idx]) == 1L

  # source: any string of at most 32 characters.
  idx <- which(present & key == "source")
  bad <- idx[!isString(idx)]
  if (length(bad)) tciApiString(vals[[bad[1]]], ptr(ev[bad[1]], "source"))
  bad <- idx[nchar(unlist(vals[idx], use.names = FALSE), type = "chars") > 32L]
  if (length(bad)) tciApiString(vals[[bad[1]]], ptr(ev[bad[1]], "source"), maxLength = 32L)

  # Times: a string each (the parse comes below); `time`, or a segment's
  # `from` and `to`, is required.
  idx <- which(present & key %in% c("time", "from", "to"))
  bad <- idx[!isString(idx)]
  if (length(bad)) tciApiTimeString(vals[[bad[1]]], ptr(ev[bad[1]], key[bad[1]]))
  times <- as.character(unlist(vals[idx], use.names = FALSE))
  if (anyNA(times)) {
    j <- idx[is.na(times)][1]
    tciApiTimeString(NA_character_, ptr(ev[j], key[j]))
  }
  tA <- rep(NA_character_, n)      # time, or a segment's from
  tB <- rep(NA_character_, n)      # a segment's to
  isTo <- key[idx] == "to"
  tA[ev[idx[!isTo]]] <- times[!isTo]
  tB[ev[idx[isTo]]] <- times[isTo]
  keyA <- rep("time", n)
  keyA[kind == "delivered"] <- "from"
  bad <- which(is.na(tA))
  if (length(bad)) tciApiTimeString(NULL, ptr(bad[1], keyA[bad[1]]))
  bad <- which(kind == "delivered" & is.na(tB))
  if (length(bad)) tciApiTimeString(NULL, ptr(bad[1], "to"))

  # The amount: rate, delivered or bolus, a number; and its units, a string.
  val <- rep(NA_real_, n)
  idx <- which(present & key %in% c("rate", "delivered", "bolus"))
  bad <- idx[!isNumber(idx)]
  if (length(bad))
    tciApiStop(400L, "INVALID_TYPE", sprintf("%s must be a JSON number", ptr(ev[bad[1]], key[bad[1]])),
               ptr(ev[bad[1]], key[bad[1]]))
  val[ev[idx]] <- as.numeric(unlist(vals[idx], use.names = FALSE))
  bad <- which(kind != "infusate" & is.na(val))
  if (length(bad))
    tciApiStop(400L, "INVALID_TYPE", sprintf("%s must be a JSON number", ptr(bad[1], kind[bad[1]])),
               ptr(bad[1], kind[bad[1]]))
  unit <- rep(NA_character_, n)
  idx <- which(present & key == "units")
  bad <- idx[!isString(idx)]
  if (length(bad)) tciApiString(vals[[bad[1]]], ptr(ev[bad[1]], "units"))
  unit[ev[idx]] <- as.character(unlist(vals[idx], use.names = FALSE))
  bad <- which(kind != "infusate" & is.na(unit))
  if (length(bad)) tciApiString(NULL, ptr(bad[1], "units"))

  # The syringe of an infusate change: the few such events are checked one by
  # one with the request's own infusate check.
  infConc <- rep(NA_real_, n)
  infUnit <- rep(NA_character_, n)
  for (j in which(present & key == "infusate")) {
    x <- tciApiInfusate(vals[[j]], ptr(ev[j], "infusate"), required = TRUE)
    infConc[ev[j]] <- x$concentration
    infUnit[ev[j]] <- x$units
  }

  bad <- which(!is.na(val) & !is.finite(val))
  if (length(bad))
    tciApiStop(422L, "INVALID_VALUE", sprintf("%s must be finite", ptr(bad[1], kind[bad[1]])),
               ptr(bad[1], kind[bad[1]]))
  for (k in names(unitsFor)) {
    bad <- which(kind == k & !(unit %in% unitsFor[[k]]))
    if (length(bad)) tciApiUnits(unit[bad[1]], ptr(bad[1], "units"), unitsFor[[k]])
  }

  bad <- which(kind %in% c("rate", "delivered") & val < 0)
  if (length(bad))
    tciApiStop(422L, "INVALID_VALUE", sprintf("%s must be >= 0", ptr(bad[1], kind[bad[1]])),
               ptr(bad[1], kind[bad[1]]))
  bad <- which(kind == "bolus" & val <= 0)
  if (length(bad))
    tciApiStop(422L, "INVALID_VALUE", sprintf("%s must be > 0", ptr(bad[1], "bolus")),
               ptr(bad[1], "bolus"))

  keyA <- ifelse(kind == "delivered", "from", "time")
  secA <- tciApiParseTimes(tA, function(i) ptr(i, keyA[i]))
  secB <- tciApiParseTimes(tB, function(i) ptr(i, "to"))
  bad <- which(kind == "delivered" & secB <= secA)
  if (length(bad))
    tciApiStop(422L, "INVALID_VALUE", sprintf("%s must be later than %s", ptr(bad[1], "to"),
                                              ptr(bad[1], "from")), ptr(bad[1], "to"))

  # The history is the past.  A moment of clock skew is forgiven and clamped
  # to now; more than that is a plan, not a record.
  relA <- tciApiPastTimes(secA, now, w, function(i) ptr(i, keyA[i]))
  relB <- tciApiPastTimes(secB, now, w, function(i) ptr(i, "to"))

  frame <- function(which, ...) {
    out <- data.frame(..., index = which, stringsAsFactors = FALSE)
    out[order(out$time, out$index), , drop = FALSE]
  }
  isRate <- kind == "rate"; isSeg <- kind == "delivered"
  isBolus <- kind == "bolus"; isInf <- kind == "infusate"
  rates <- frame(which(isRate), time = relA[isRate], rate = val[isRate], units = unit[isRate])
  boluses <- frame(which(isBolus), time = relA[isBolus], amount = val[isBolus], units = unit[isBolus])
  infusates <- frame(which(isInf), time = relA[isInf], concentration = infConc[isInf],
                     units = infUnit[isInf])
  segments <- data.frame(from = relA[isSeg], to = relB[isSeg], delivered = val[isSeg],
                         units = unit[isSeg], index = which(isSeg), stringsAsFactors = FALSE)
  segments <- segments[order(segments$from, segments$index), , drop = FALSE]

  # Two rate changes (or two syringes) at one instant are contradictory: the
  # order of two objects in an array is not evidence of which came later.
  sameInstant <- function(df, what) {
    d <- which(duplicated(df$time))
    if (length(d))
      tciApiStop(422L, "HISTORY_INVALID",
                 sprintf("two %s at the same instant (%s and %s)", what,
                         ptr(df$index[d[1] - 1L]), ptr(df$index[d[1]])), ptr(df$index[d[1]]))
  }
  sameInstant(rates, "rate changes")
  sameInstant(infusates, "infusate changes")
  if (nrow(segments) > 1) {
    o <- which(segments$from[-1] < segments$to[-nrow(segments)])
    if (length(o))
      tciApiStop(422L, "HISTORY_OVERLAP",
                 sprintf("delivered segments %s and %s overlap", ptr(segments$index[o[1]]),
                         ptr(segments$index[o[1] + 1L])), ptr(segments$index[o[1] + 1L]))
  }

  # A millilitre quantity converts with the syringe in force at its time.
  # Once the history records any syringe, the only syringes known are the
  # recorded ones: a millilitre quantity before the first recorded syringe
  # (a rate, a bolus, or a segment that starts before it) has no known
  # concentration, and pricing it with the request's syringe -- the one on
  # the pump NOW, which must equal the last recorded one -- would halve or
  # double the modelled dose across a mid-case change.  Such a history is
  # refused: the controller records the initial syringe as an infusate event
  # too.  Without any infusate event the request's syringe is the only one
  # there ever was, and applies throughout.
  ml <- which(unit %in% c("mL", "mL/hr"))
  mlBeforeInfusate <- integer(0)
  if (nrow(infusates))
    mlBeforeInfusate <- ml[relA[ml] < infusates$time[1]]
  list(rates = rates, segments = segments, boluses = boluses, infusates = infusates,
       mlPointers = if (length(ml)) ptr(ml, "units") else character(0),
       mlBeforeInfusate = mlBeforeInfusate)
}

# Seconds since the epoch to minutes relative to now, which must not lie in
# the future: within TCI_API_CLOCK_SKEW of now is clamped with a warning,
# beyond it refused.  NA passes through.
tciApiPastTimes <- function(secs, now, w, pointerFor)
{
  late <- secs - now
  bad <- which(late > TCI_API_CLOCK_SKEW)
  if (length(bad))
    tciApiStop(422L, "HISTORY_IN_FUTURE",
               sprintf("%s is %.3f s after now; the history is the past, not a plan",
                       pointerFor(bad[1]), late[bad[1]]), pointerFor(bad[1]))
  skew <- which(late > 0)
  for (i in skew)
    tciApiWarn(w, "HISTORY_CLOCK_SKEW",
               sprintf("%s is %.3f s after now and was clamped to now", pointerFor(i), late[i]),
               pointerFor(i))
  late[skew] <- 0
  late / 60
}

# The record in base units: the pieces of the piecewise-constant pump rate
# (start, end in minutes relative to now, rate in base/min), the boluses, the
# rate running at now and the pump's total.  The syringe in force at any
# instant is the last infusate change at or before it; with no infusate
# change in the history it is the request's throughout (the validation has
# refused a millilitre quantity that lies before the first recorded syringe,
# so the request's syringe is never applied across a change).  A piece is
# split at every infusate change, so a mL quantity converts with the right
# syringe even when a segment or a rate spans a syringe change.
tciApiHistory <- function(H, base, weight, infusate)
{
  conc0 <- tciApiInfusateBase(infusate, base)
  inf <- H$infusates
  if (nrow(inf)) inf$concBase <- tciApiInfusateBase(inf, base)
  concAt <- function(t) {
    out <- rep(conc0, length(t))
    if (nrow(inf)) {
      k <- findInterval(t, inf$time)
      has <- k > 0
      out[has] <- inf$concBase[k[has]]
    }
    out
  }

  rc <- H$rates
  sg <- H$segments
  # A segment clamped to nothing (both ends at now) keeps its amount as an
  # instantaneous pump delivery: the limit of a constant rate over a span
  # shrinking to zero.
  flat <- sg[sg$to <= sg$from, , drop = FALSE]
  sg <- sg[sg$to > sg$from, , drop = FALSE]

  bounds <- sort(unique(c(rc$time, sg$from, sg$to, inf$time, 0)))
  bounds <- bounds[bounds <= 0]
  start <- bounds[-length(bounds)]
  end <- bounds[-1]
  rate <- numeric(length(start))
  if (length(start)) {
    value <- numeric(length(start))
    units <- rep(NA_character_, length(start))
    if (nrow(rc)) {
      k <- findInterval(start, rc$time)
      has <- k > 0
      value[has] <- rc$rate[k[has]]
      units[has] <- rc$units[k[has]]
    }
    if (nrow(sg)) {
      k <- findInterval(start, sg$from)
      inside <- k > 0
      inside[inside] <- start[inside] < sg$to[k[inside]]
      value[inside] <- sg$delivered[k[inside]] / (sg$to[k[inside]] - sg$from[k[inside]])
      units[inside] <- sg$units[k[inside]]
    }
    use <- !is.na(units)
    rate[use] <- tciApiToBase(value[use], units[use], base, weight, concAt(start[use]))
  }
  pieces <- data.frame(start = start, end = end, rate = rate)

  hand <- H$boluses
  handAmount <- if (nrow(hand)) tciApiToBase(hand$amount, hand$units, base, weight,
                                             concAt(hand$time)) else numeric(0)
  flatAmount <- if (nrow(flat)) tciApiToBase(flat$delivered, flat$units, base, weight,
                                             concAt(flat$from)) else numeric(0)
  boluses <- data.frame(time = c(hand$time, flat$from), amount = c(handAmount, flatAmount))

  running <- if (nrow(rc)) tciApiToBase(rc$rate[nrow(rc)], rc$units[nrow(rc)], base,
                                        weight, conc0) else 0

  list(pieces = pieces, boluses = boluses, runningRate = running,
       # What passed through the pump: the integral of its rate.  A hand bolus
       # did not, so it is not here; the pump's totaliser cannot know of it.
       delivered = sum(pieces$rate * (pieces$end - pieces$start)) + sum(flatAmount),
       concBase = conc0)
}


# ---------------------------------------------------------------------------
# The closed form

# The seven amplitudes at now from the record, by superposition (see the
# header): each piece and each bolus contributes independently.
tciStatesFromHistory <- function(H, K)
{
  p <- numeric(3)
  e <- numeric(4)
  pc <- H$pieces[H$pieces$rate != 0 & H$pieces$end > H$pieces$start, , drop = FALSE]
  if (nrow(pc)) {
    gain <- function(lambda)
      exp(outer(lambda, pc$end)) * (-expm1(-outer(lambda, pc$end - pc$start)))
    p <- K$pinf * as.vector(gain(K$lambda3) %*% pc$rate)
    e <- K$einf * as.vector(gain(K$lambda4) %*% pc$rate)
  }
  b <- H$boluses
  if (nrow(b)) {
    p <- p + K$pbolus * as.vector(exp(outer(K$lambda3, b$time)) %*% b$amount)
    e <- e + K$ebolus * as.vector(exp(outer(K$lambda4, b$time)) %*% b$amount)
  }
  list(p = p, e = e)
}

# The state dt minutes later at a constant rate (base/min): tciSchedule()'s
# advance().
tciAdvance <- function(S, dt, rate, K)
{
  if (dt <= 0) return(S)
  d3 <- exp(-K$lambda3 * dt)
  d4 <- exp(-K$lambda4 * dt)
  list(p = S$p * d3 + K$pinf * rate * (1 - d3),
       e = S$e * d4 + K$einf * rate * (1 - d4))
}

# The plasma or effect-site concentration tau minutes after a state with the
# pump off (vectorised over tau).
tciConc <- function(S, K, site, tau = 0)
{
  if (site == "effect") as.vector(S$e %*% exp(-outer(K$lambda4, tau)))
  else                  as.vector(S$p %*% exp(-outer(K$lambda3, tau)))
}

# The time for the controlled concentration to fall to `level` with the pump
# off from the state: STANPUMP's look_ahead().  Zero if already there; NULL
# with beyond = TRUE if not within the horizon.  The crossing is bracketed on
# a coarse grid and solved by uniroot on the closed form.
tciApiDecrement <- function(S, K, site, level, horizonSeconds = TCI_API_LOOKAHEAD_MAX)
{
  if (tciConc(S, K, site) <= level) return(list(seconds = 0, beyond = FALSE))
  horizon <- horizonSeconds / 60
  grid <- seq(0, horizon, length.out = max(3L, min(3001L, ceiling(horizonSeconds / 10) + 1L)))
  below <- which(tciConc(S, K, site, grid) <= level)
  if (!length(below)) return(list(seconds = NULL, beyond = TRUE))
  k <- below[1]
  root <- stats::uniroot(function(tau) tciConc(S, K, site, tau) - level,
                         c(grid[k - 1L], grid[k]), tol = 1e-10)$root
  list(seconds = root * 60, beyond = FALSE)
}

# The effect-site peak with the pump off from the state: the maximum of Ce
# over the next tPeak + 10 s on a 1 s grid, the grid tciRate() searched.
tciApiPeak <- function(S, K)
{
  if (K$ke0 <= 0) return(NULL)
  tau <- seq(0, K$tPeak + 10 / 60, by = 1 / 60)
  ce <- tciConc(S, K, "effect", tau)
  i <- which.max(ce)
  list(ce = ce[i], tau = tau[i])
}


# ---------------------------------------------------------------------------
# The plan (sections 5.3 and 5.4)

# The pump's limits, in order: clamp to maxRate, send 0 below minRate, round
# DOWN to a multiple of rateResolution.  A deficit is recovered by the next
# plan; an excess is not.  The rounding is done in decimal, not in binary:
# 24.6 / 0.1 is 245.99999999999997 and 9999 * 0.1 is 999.90000000000009, so
# the multiple is taken with a hair of tolerance and, when it lands a few
# ulps above the rate, the rate itself is kept -- it IS that multiple as the
# pump typed it.  Pulling it back a whole step instead (the first version of
# this function) sent 999.8 for a pump whose ceiling was 999.9 mL/hr, and
# named rateResolution as a limit on a value that already was a multiple: a
# third of all one-decimal ceilings lost a step that way.  The result is never
# above the rate that came in.
tciApiLimit <- function(ideal, pump)
{
  value <- ideal
  limited <- character(0)
  if (value > pump$maxRate) {
    value <- pump$maxRate
    limited <- c(limited, "maxRate")
  }
  if (!is.null(pump$minRate) && value > 0 && value < pump$minRate) {
    value <- 0
    limited <- c(limited, "minRate")
  }
  if (!is.null(pump$rateResolution) && value > 0) {
    res <- pump$rateResolution
    k <- floor(value / res + 1e-9)
    rounded <- k * res
    if (rounded > value) {
      rounded <- if (rounded - value <= 1e-9 * max(value, res)) value else (k - 1) * res
    }
    if (abs(rounded - value) > 1e-12 * max(value, res)) limited <- c(limited, "rateResolution")
    value <- rounded
  }
  list(value = value, limitedBy = limited)
}

# tciRate() on a state, then the pump's limits.  `conv` carries base, weight,
# concBase and rateFactor (how many base/min one unit of the pump's rate is,
# computed once per request: the look-ahead calls this thousands of times).
tciApiPlan <- function(S, CT, site, interval, K, pump, conv)
{
  x <- tciRate(S$p, S$e, CT, site, interval / 60, K)
  # With the pump meant to be off, the effect-site search leaves a rounding
  # residual rather than an exact zero: of order 1e-15 for an adult, and up
  # to 1e-13 mcg/min for remifentanil at MIN_WEIGHT, where the coefficients
  # are large.  A rate whose steady-state plasma would be a millionth of the
  # target is zero (a maintenance rate holds the target itself, so this is a
  # million times below any rate the controller means): the phase and the
  # nextRequestAt rule read "pump off" from it, and a pump could not run it
  # anyway.
  if (x$rate * sum(K$pinf) <= 1e-6 * CT) x$rate <- 0
  ideal <- x$rate / conv$rateFactor
  lim <- tciApiLimit(ideal, pump)
  list(
    plan      = if (CT <= 0) "off" else if (x$effectMode) "effectSite" else "plasma",
    idealBase = x$rate,
    ideal     = ideal,
    value     = lim$value,
    limitedBy = lim$limitedBy,
    base      = lim$value * conv$rateFactor
  )
}

# The concentration at the controlled site that a rate of one base unit per
# minute, run from now on from an empty body, has built tau minutes later
# (vectorised over tau): the unit step response.
tciStepConc <- function(K, site, tau)
{
  if (site == "effect") as.vector(K$einf %*% (1 - exp(-outer(K$lambda4, tau))))
  else                  as.vector(K$pinf %*% (1 - exp(-outer(K$lambda3, tau))))
}

# When the controlled concentration first comes within TCI_API_ON_TARGET of
# the target, running the same controller forward from the state in closed
# loop (same interval and limits, no latency), as minutes from the state, or
# NULL if not within TCI_API_LOOKAHEAD_MAX.
#
# Three things keep this to a few plans rather than one per interval of the
# horizon, which at a 1 s interval would be thirty thousand tciRate() calls
# and a second of compute, longer than the controller's own cycle:
#
#   * The pump at its ceiling from now on bounds every closed loop from above:
#     no plan exceeds the ceiling, and the system is linear with a
#     non-negative response.  When even that trajectory never enters the band
#     the answer is NULL at once.  This is the common expensive case, a
#     ceiling below the maintenance requirement or a target the pump cannot
#     lift the concentration to; without the exit it cost 1 s at a 1 s
#     interval and 0.1 s at 10 s.
#   * While the controller ITSELF chooses rate 0 -- the effect site rising to
#     its peak after a loading infusion, a plasma decaying from above the
#     target -- its plan stays 0 as long as nothing it reads changes: the
#     pump-off trajectory still reaches the target beyond the next step, the
#     controlled concentration is on the same side of the target, the
#     Shafer-Gregg mode (effect site, or plasma within TCI_PLASMA_SWITCH of
#     the target) is the same.  So that stretch is one closed-form evaluation
#     of the pump-off trajectory on the step grid, resuming one step before
#     anything changes, rather than one plan per step; the result is the same
#     to the last bit.  A pump limit that zeroes a positive plan is not the
#     controller's choice and is stepped through.
#   * After TCI_API_LOOKAHEAD_EXACT_STEPS plans the step grows by a tenth per
#     plan, up to TCI_API_LOOKAHEAD_MAX_STEP seconds, as the app's own
#     tciSchedule() lengthens its step once a maintenance infusion has
#     settled.  This bounds the plans at a few hundred whatever the interval;
#     a ceiling just above the maintenance requirement, where the approach to
#     the band is asymptotic, is the case that reaches it, and the reported
#     time is then on the coarser grid.
tciApiTargetReached <- function(S, CT, site, interval, K, pump, conv)
{
  if (CT <= 0) return(NULL)
  band <- TCI_API_ON_TARGET * CT
  within <- function(x) abs(x - CT) <= band
  controlled <- function(S) if (site == "effect") sum(S$e) else sum(S$p)
  horizon <- TCI_API_LOOKAHEAD_MAX / 60
  c0 <- controlled(S)
  if (within(c0)) return(0)
  if (c0 < CT - band) {
    capBase <- tciApiLimit(pump$maxRate, pump)$value * conv$rateFactor
    grid <- seq(0, horizon, by = 10 / 60)
    if (all(tciConc(S, K, site, grid) + capBase * tciStepConc(K, site, grid) < CT - band))
      return(NULL)
  }
  effectMode <- function(ce) site == "effect" & abs(ce - CT) >= TCI_PLASMA_SWITCH * CT
  dt <- interval / 60
  t <- 0
  plans <- 0L
  while (t < horizon - 1e-9) {
    c0 <- controlled(S)
    if (within(c0)) return(t)
    plan <- tciApiPlan(S, CT, site, dt * 60, K, pump, conv)
    plans <- plans + 1L
    if (plan$idealBase == 0) {
      # Off by choice: follow the pump-off trajectory to the first step at
      # which the plan could change, at most 3000 steps at a time.
      m <- min(ceiling((horizon - t) / dt - 1e-9), 3000L)
      tau <- dt * seq_len(m)
      cp <- tciConc(S, K, "plasma", tau)
      ce <- tciConc(S, K, "effect", tau)
      conc <- if (site == "effect") ce else cp
      mode0 <- effectMode(sum(S$e))
      change <- within(conc) | sign(conc - CT) != sign(c0 - CT) |
        effectMode(ce) != mode0 | (!mode0 & cp < CT)
      k <- if (any(change)) which(change)[1] else NA_integer_
      if (is.na(k) && t + tau[m] >= horizon - 1e-9) return(NULL)
      if (!is.na(k) && within(conc[k])) return(t + tau[k])
      jump <- if (is.na(k)) m else k - 1L
      if (jump >= 1L) {
        S <- list(p = S$p * exp(-K$lambda3 * tau[jump]), e = S$e * exp(-K$lambda4 * tau[jump]))
        t <- t + tau[jump]
        next
      }
    }
    S <- tciAdvance(S, dt, plan$base, K)
    t <- t + dt
    if (plans >= TCI_API_LOOKAHEAD_EXACT_STEPS)
      dt <- min(dt * 1.1, TCI_API_LOOKAHEAD_MAX_STEP / 60)
  }
  if (within(controlled(S))) return(t)
  NULL
}


# ---------------------------------------------------------------------------
# The request (section 4)

# Validate the parsed body against the path's drug.  Returns the request in
# normalised form: times in seconds since the epoch, numbers as doubles,
# defaults filled in, the history parsed.  Signals tci_api_error at the first
# failure; adds warnings to w.
tciApiValidate <- function(request, drug, w)
{
  if (!tciApiIsObject(request))
    tciApiStop(400L, "INVALID_TYPE", "the request body must be a JSON object", "")
  tciApiCheckKeys(request, "", tciApiSchema())

  requestId <- tciApiString(request$requestId, "/requestId", required = FALSE, maxLength = 128L)
  if (!is.null(requestId) && !grepl("^[ -~]*$", requestId))
    tciApiStop(422L, "INVALID_VALUE", "/requestId must be printable ASCII", "/requestId")

  drug <- tciApiCheckDrug(drug)
  if (!is.null(request[["drug"]])) {
    bodyDrug <- tciApiString(request[["drug"]], "/drug")
    if (!identical(bodyDrug, drug))
      tciApiStop(400L, "DRUG_MISMATCH",
                 sprintf("the body's drug '%s' is not the path's drug '%s'", bodyDrug, drug),
                 "/drug")
  }
  defaults <- getDrugDefaults(drug)
  units <- tciApiBaseUnits(defaults$Concentration.Units)

  nowText <- tciApiTimeString(request$now, "/now")
  now <- tciApiParseTimes(nowText, function(i) "/now")

  patient <- tciApiObject(request$patient, "/patient")
  weight <- tciApiNumber(patient$weight, "/patient/weight", min = MIN_WEIGHT, max = MAX_WEIGHT)
  height <- tciApiNumber(patient$height, "/patient/height", min = MIN_HEIGHT, max = MAX_HEIGHT)
  age    <- tciApiNumber(patient$age, "/patient/age", min = MIN_AGE, max = MAX_AGE)
  sex    <- tciApiString(patient$sex, "/patient/sex", allowed = SEX_VALUES)

  target <- tciApiObject(request$target, "/target")
  site   <- tciApiString(target$site, "/target/site", allowed = c("effect", "plasma"))
  value  <- tciApiNumber(target$value, "/target/value", min = 0)
  tunits <- tciApiUnits(target$units, "/target/units", TCI_API_CONCENTRATION_UNITS)
  if (tunits != units$concentration)
    tciApiStop(422L, "UNIT_MISMATCH",
               sprintf("target.units must be %s for %s", units$concentration, drug), "/target/units")
  upper <- defaults$Upper
  if (value > TCI_API_TARGET_CEILING * upper)
    tciApiStop(422L, "TARGET_OUT_OF_RANGE",
               sprintf("target.value %g %s is above %g x the upper typical target of %s (%g %s)",
                       value, tunits, TCI_API_TARGET_CEILING, drug, upper, tunits), "/target/value")
  if (value > upper)
    tciApiWarn(w, "TARGET_HIGH",
               sprintf("target.value %g %s is above the upper typical target of %s (%g %s)",
                       value, tunits, drug, upper, tunits), "/target/value")

  timing   <- tciApiObject(request$timing, "/timing")
  interval <- tciApiNumber(timing$interval, "/timing/interval",
                           min = TCI_API_MIN_INTERVAL, max = TCI_API_MAX_INTERVAL)
  latency  <- tciApiNumber(timing$latency, "/timing/latency", min = 0)
  if (latency >= interval)
    tciApiStop(422L, "LATENCY_INVALID",
               sprintf("timing.latency (%g s) must be less than timing.interval (%g s)",
                       latency, interval), "/timing/latency")
  if (latency > interval / 2)
    tciApiWarn(w, "LATENCY_LONG",
               sprintf("timing.latency (%g s) is more than half the interval (%g s)",
                       latency, interval), "/timing/latency")

  pump <- tciApiObject(request$pump, "/pump")
  rateUnits <- tciApiUnits(pump$rateUnits, "/pump/rateUnits", TCI_API_RATE_UNITS)
  maxRate <- tciApiNumber(pump$maxRate, "/pump/maxRate", min = 0, openMin = TRUE,
                          max = TCI_API_MAX_RATE)
  minRate <- tciApiNumber(pump$minRate, "/pump/minRate", required = FALSE, min = 0,
                          max = maxRate, openMax = TRUE, maxName = "maxRate")
  rateResolution <- tciApiNumber(pump$rateResolution, "/pump/rateResolution", required = FALSE,
                                 min = 0, openMin = TRUE, max = maxRate / 10,
                                 maxName = "maxRate / 10")
  currentRate <- tciApiNumber(pump$currentRate, "/pump/currentRate", required = FALSE, min = 0)
  state <- tciApiString(pump$state, "/pump/state", required = FALSE, allowed = TCI_API_PUMP_STATES)
  if (is.null(state)) state <- "running"
  volumeInfused <- tciApiQuantity(pump$volumeInfused, "/pump/volumeInfused", TCI_API_AMOUNT_UNITS)
  syringe <- tciApiQuantity(pump$syringeVolumeRemaining, "/pump/syringeVolumeRemaining",
                            TCI_API_VOLUME_UNITS)

  infusate <- tciApiInfusate(request$infusate, "/infusate")

  history <- request$history
  if (is.null(history)) tciApiMissing("/history")
  if (!tciApiIsArray(history))
    tciApiStop(400L, "INVALID_TYPE", "/history must be a JSON array (it may be empty)", "/history")
  if (length(history) > TCI_API_MAX_HISTORY)
    tciApiStop(413L, "HISTORY_TOO_LONG",
               sprintf("/history has %d events; at most %d are accepted (merge old segments)",
                       length(history), TCI_API_MAX_HISTORY), "/history")
  H <- tciApiValidateHistory(history, now, w)

  lookAhead <- tciApiObject(request$lookAhead, "/lookAhead", required = FALSE)
  lookAheadConcentration <- if (is.null(lookAhead)) NULL else
    tciApiNumber(lookAhead$concentration, "/lookAhead/concentration", min = 0, openMin = TRUE)

  options <- tciApiObject(request$options, "/options", required = FALSE)
  adjustToFFM <- if (is.null(options)) NULL else
    tciApiLogical(options$adjustToFFM, "/options/adjustToFFM", required = FALSE)
  if (is.null(adjustToFFM)) adjustToFFM <- TRUE

  # Anything in millilitres needs the syringe, which must be the syringe the
  # history last reported.
  ml <- c(if (rateUnits == "mL/hr") "/pump/rateUnits",
          if (!is.null(volumeInfused) && volumeInfused$units == "mL") "/pump/volumeInfused/units",
          if (!is.null(syringe)) "/pump/syringeVolumeRemaining/units",
          H$mlPointers)
  if (length(ml) && is.null(infusate))
    tciApiStop(422L, "INFUSATE_REQUIRED",
               sprintf("infusate is required because %s is in millilitres", ml[1]), "/infusate")
  if (length(H$mlBeforeInfusate)) {
    i <- H$mlBeforeInfusate[1]
    tciApiStop(422L, "INFUSATE_REQUIRED",
               sprintf("the syringe in force before /history/%d is unknown: record the initial syringe as an infusate event (the request's infusate is the syringe on the pump now, which must equal the last infusate event)",
                       i - 1L), paste0("/history/", i - 1L, "/units"))
  }
  if (!is.null(infusate) && nrow(H$infusates)) {
    last <- H$infusates[nrow(H$infusates), ]
    if (last$concentration != infusate$concentration || last$units != infusate$units)
      tciApiStop(422L, "INFUSATE_MISMATCH",
                 sprintf("infusate (%g %s) must equal the last infusate change in the history (%g %s at /history/%d)",
                         infusate$concentration, infusate$units, last$concentration, last$units,
                         last$index - 1L), "/infusate")
  }

  list(
    requestId = requestId, drug = drug, now = now, nowText = nowText,
    patient = list(weight = weight, height = height, age = age, sex = sex),
    infusate = infusate,
    pump = list(rateUnits = rateUnits, maxRate = maxRate, minRate = minRate,
                rateResolution = rateResolution, currentRate = currentRate, state = state,
                volumeInfused = volumeInfused, syringeVolumeRemaining = syringe),
    target = list(site = site, value = value, units = tunits),
    timing = list(latency = latency, interval = interval),
    history = H,
    lookAhead = lookAheadConcentration,
    adjustToFFM = adjustToFFM,
    units = units,
    defaults = defaults
  )
}


# ---------------------------------------------------------------------------
# The response (sections 5 and 6)

# The installed package's DESCRIPTION, read once: it does not change while a
# server runs, and reading it is a measurable part of a request.
tciApiDescription <- memoise::memoise(function() utils::packageDescription("stanpumpR"))

tciApiServerBlock <- function(receivedAt, respondedAt, compute = TRUE)
{
  desc <- tciApiDescription()
  received <- tciApiSeconds(receivedAt)
  responded <- tciApiSeconds(respondedAt)
  out <- list(
    package     = "stanpumpR",
    version     = as.character(desc$Version),
    commit      = if (is.null(desc$RemoteSha)) NULL else as.character(desc$RemoteSha),
    receivedAt  = tciApiFormatTime(received),
    respondedAt = tciApiFormatTime(responded)
  )
  if (compute) out$computeSeconds <- responded - received
  out
}

# The request id for an error body, read leniently before validation so that
# a refused request is still answered by name.
tciApiLenientRequestId <- function(request)
{
  id <- if (tciApiIsObject(request)) request[["requestId"]] else NULL
  if (is.character(id) && length(id) == 1L && !is.na(id)) id else NULL
}

#' The next infusion rate for a TCI pump
#'
#' The controller of the stanpumpR TCI API (version 1): from a pump
#' controller's request -- the patient, the syringe, the pump's limits, the
#' target, the command latency and update interval, and the record of what
#' the pump has delivered -- computes the rate to run for the coming interval
#' and the predictions that go with it.  Stateless: the model state is
#' rebuilt from the delivery record on every call, so the answer depends on
#' the arguments alone.  The rate is \code{tciRate()}, the controller behind
#' the app's target rows (Shafer & Gregg 1992); see \code{docs/tci-api.md}
#' for the request and response in full.
#'
#' @param request the request body as a list, as
#'   \code{jsonlite::fromJSON(txt, simplifyVector = FALSE)} returns it: see
#'   section 4 of \code{docs/tci-api.md}.
#' @param drug the drug of the request path, which the body's \code{drug}
#'   must equal when present.  Defaults to the body's.
#' @param receivedAt when the request arrived (POSIXct, or seconds since the
#'   epoch), reported in \code{server} and compared with the request's
#'   \code{now} for the \code{CLOCK_SKEW} warning.  The computation itself
#'   uses only the controller's clock.
#'
#' @returns the response body as a list, ready for
#'   \code{jsonlite::toJSON(auto_unbox = TRUE, digits = NA, null = "null")}:
#'   the commanded rate in the pump's units, the bounded segment to program,
#'   the fallback rate, the timing, the model state and the predictions
#'   (section 6 of \code{docs/tci-api.md}).  A refused request signals a
#'   condition of class \code{tci_api_error} with the HTTP \code{status}, the
#'   error \code{code}, a \code{message}, the JSON \code{pointer} of the field
#'   at fault and the \code{requestId} when the body carried one.
#'
#' @examples
#' body <- list(
#'   drug = "propofol", now = "2026-10-11T14:32:05.250Z",
#'   patient = list(weight = 70, height = 170, age = 50, sex = "male"),
#'   pump = list(rateUnits = "mg/min", maxRate = 1000),
#'   target = list(site = "effect", value = 3, units = "mcg/mL"),
#'   timing = list(latency = 1.5, interval = 10),
#'   history = list()
#' )
#' response <- tciNextRate(body)
#' response$rate$value
#' response$timing$effectiveAt
#'
#' @export
tciNextRate <- function(request, drug = request[["drug"]], receivedAt = Sys.time())
{
  rid <- tciApiLenientRequestId(request)
  tryCatch(
    tciApiCompute(request, drug, receivedAt),
    tci_api_error = function(e) {
      e$requestId <- rid
      stop(e)
    }
  )
}

tciApiCompute <- function(request, drug, receivedAt)
{
  w <- tciApiWarnings()
  if (!tciApiIsObject(request))
    tciApiStop(400L, "INVALID_TYPE", "the request body must be a JSON object", "")
  req <- tciApiValidate(request, drug, w)

  PK <- tciApiPK(req$drug, req$patient$weight, req$patient$height, req$patient$age,
                 req$patient$sex, req$adjustToFFM)
  K <- tciKinetics(PK)
  base <- req$units$mass
  tunits <- req$target$units
  CT <- req$target$value
  site <- req$target$site
  L <- req$timing$latency
  I <- req$timing$interval
  dt <- I / 60
  pump <- req$pump

  if (K$tPeak > 0 && I > K$tPeak * 60 / 3)
    tciApiWarn(w, "INTERVAL_LONG",
               sprintf("timing.interval (%g s) is more than a third of %s's time to peak effect (%.1f s)",
                       I, req$drug, K$tPeak * 60), "/timing/interval")
  received <- tciApiSeconds(receivedAt)
  if (abs(received - req$now) > TCI_API_SERVER_SKEW)
    tciApiWarn(w, "CLOCK_SKEW",
               sprintf("now is %.1f s from the server's clock; the computation uses the controller's clock",
                       req$now - received), "/now")

  # 5.1  The state at now from the record
  H <- tciApiHistory(req$history, base, req$patient$weight, req$infusate)
  conv <- list(base = base, weight = req$patient$weight, concBase = H$concBase,
               rateFactor = tciApiUnitFactor(pump$rateUnits, base, req$patient$weight, H$concBase))
  S0 <- tciStatesFromHistory(H, K)

  if (pump$state != "running")
    tciApiWarn(w, "PUMP_NOT_RUNNING",
               sprintf("pump.state is '%s': the latency projection runs at rate 0 and the plan assumes the pump will run it",
                       pump$state), "/pump/state")
  if (pump$state == "running" && !is.null(pump$currentRate)) {
    runningPump <- H$runningRate / conv$rateFactor
    tol <- max(if (is.null(pump$rateResolution)) 0 else pump$rateResolution, 1e-6 * runningPump)
    if (abs(pump$currentRate - runningPump) > tol)
      tciApiWarn(w, "RATE_MISMATCH",
                 sprintf("pump.currentRate (%g %s) differs from the rate the history says is running (%g %s); the history is authoritative",
                         pump$currentRate, pump$rateUnits, runningPump, pump$rateUnits),
                 "/pump/currentRate")
  }

  # 5.2  The latency projection: the old rate runs until the command lands
  running <- if (pump$state == "running") H$runningRate else 0
  S1 <- tciAdvance(S0, L / 60, running, K)

  # 5.3, 5.4  The plan and the pump's limits
  plan <- tciApiPlan(S1, CT, site, I, K, pump, conv)

  # 5.7  The hold and the fallback
  S2 <- tciAdvance(S1, dt, plan$base, K)
  fallback <- tciApiPlan(S2, CT, site, I, K, pump, conv)

  # 5.5  Timing
  t0 <- req$now
  t1 <- t0 + L
  t2 <- t1 + I
  controlled <- function(S) if (site == "effect") sum(S$e) else sum(S$p)
  C1 <- controlled(S1)
  nextAt <- t2 - L
  if (plan$value == 0 && CT > 0) {
    # STANPUMP's rule: with the pump off, ask again when the concentration
    # reaches the target, so the maintenance starts at the peak.  A peak that
    # stays below the band does not reach the target (a pump floor that
    # zeroed a positive plan, say), and a peak at effectiveAt itself is
    # nothing to wait for; both keep the usual time.
    reach <- NULL
    if (site == "effect" && C1 < CT) {
      pk <- tciApiPeak(S1, K)
      if (!is.null(pk) && pk$ce >= (1 - TCI_API_ON_TARGET) * CT && pk$tau > 0)
        reach <- t1 + pk$tau * 60
    } else if (C1 > CT) {
      d <- tciApiDecrement(S1, K, site, CT, horizonSeconds = 2 * I)
      if (!d$beyond && d$seconds > 0) reach <- t1 + d$seconds
    }
    if (!is.null(reach) && reach < t2 + I) nextAt <- max(t0, reach - L)
  }

  phase <- if (CT <= 0) "stopped"
           else if (plan$value == 0) { if (C1 > CT) "aboveTarget" else "waitingForPeak" }
           else if (C1 < (1 - TCI_PLASMA_SWITCH) * CT) "loading"
           else "maintenance"

  # 5.6  Predictions, all from the commanded rate
  C2 <- controlled(S2)
  pk2 <- tciApiPeak(S2, K)
  reached <- tciApiTargetReached(S2, CT, site, I, K, pump, conv)
  prediction <- list(
    units  = tunits,
    peakCe = if (is.null(pk2)) NULL else pk2$ce,
    peakAt = if (is.null(pk2)) NULL else tciApiFormatTime(t2 + pk2$tau * 60),
    onTarget = if (CT > 0) abs(C2 - CT) <= TCI_API_ON_TARGET * CT else FALSE,
    deviation = if (CT > 0) (C2 - CT) / CT else NULL,
    targetReachedAt = if (is.null(reached)) NULL else tciApiFormatTime(t2 + reached * 60)
  )
  if (!is.null(req$lookAhead)) {
    d <- tciApiDecrement(S0, K, site, req$lookAhead)
    prediction$lookAhead <- list(
      site = site, concentration = req$lookAhead, units = tunits,
      decrementSeconds = d$seconds, horizonSeconds = TCI_API_LOOKAHEAD_MAX,
      beyondHorizon = d$beyond
    )
  }

  hasInfusate <- !is.null(req$infusate)
  mlPerMin <- function(baseRate) baseRate / conv$concBase
  syringe <- pump$syringeVolumeRemaining
  if (!is.null(syringe)) {
    remaining <- syringe$value
    usedInLatency <- mlPerMin(running) * L / 60
    emptyAt <- NULL
    if (usedInLatency >= remaining && running > 0) {
      emptyAt <- t0 + remaining / mlPerMin(running) * 60
    } else if (plan$base > 0) {
      emptyAt <- t1 + (remaining - usedInLatency) / mlPerMin(plan$base) * 60
    }
    prediction["syringeEmptyAt"] <- list(if (is.null(emptyAt)) NULL else tciApiFormatTime(emptyAt))
    needed <- (plan$base + fallback$base) * dt / conv$concBase
    if (needed > remaining)
      tciApiWarn(w, "SYRINGE_LOW",
                 sprintf("the planned segment and the fallback segment need %.2f mL and %.2f mL remain",
                         needed, remaining), "/pump/syringeVolumeRemaining")
  }

  reported <- NULL
  if (!is.null(pump$volumeInfused)) {
    reported <- tciApiToBase(pump$volumeInfused$value, pump$volumeInfused$units, base,
                             conv$weight, conv$concBase)
    tol <- max(TCI_API_DELIVERY_TOLERANCE * H$delivered,
               if (hasInfusate) TCI_API_DELIVERY_TOLERANCE_ML * conv$concBase else 0)
    if (abs(reported - H$delivered) > tol)
      tciApiWarn(w, "DELIVERY_MISMATCH",
                 sprintf("pump.volumeInfused is %g %s and the history integrates to %g %s",
                         reported, base, H$delivered, base), "/pump/volumeInfused")
  }

  amount <- plan$base * dt
  rate <- list(value = plan$value, units = pump$rateUnits)
  if (hasInfusate) rate$mlPerHour <- mlPerMin(plan$base) * 60
  rate$massPerMinute <- plan$base
  rate$massUnits <- base
  rate$ideal <- plan$ideal
  rate$limitedBy <- I(plan$limitedBy)
  segment <- list(holdSeconds = I, amount = amount, amountUnits = base)
  if (hasInfusate) segment$volumeMl <- amount / conv$concBase

  out <- list(
    apiVersion = TCI_API_VERSION,
    requestId  = req$requestId,
    drug       = req$drug,
    target     = list(site = site, value = CT, units = tunits),
    controller = list(plan = plan$plan, phase = phase),
    rate       = rate,
    segment    = segment,
    fallback   = list(rate = fallback$value, units = pump$rateUnits, maxSeconds = I),
    # `now` is the request's own string, verbatim, so that a controller can
    # match the response to its request by comparing the two (section 7, rule
    # 1); the times the server computes are formatted in UTC.
    timing     = list(now = req$nowText, effectiveAt = tciApiFormatTime(t1),
                      holdUntil = tciApiFormatTime(t2), nextRequestAt = tciApiFormatTime(nextAt),
                      latency = L, interval = I),
    state      = list(units = tunits,
                      now = list(cp = sum(S0$p), ce = sum(S0$e), p = S0$p, e = S0$e),
                      effectiveAt = list(cp = sum(S1$p), ce = sum(S1$e)),
                      holdUntil = list(cp = sum(S2$p), ce = sum(S2$e))),
    prediction = prediction,
    delivered  = list(fromHistory = H$delivered, reported = reported, units = base),
    model      = tciApiModelBlock(PK, req$adjustToFFM),
    algorithm  = list(name = "shafer-gregg-1992", plasmaSwitch = TCI_PLASMA_SWITCH,
                      tauGridSeconds = 1),
    warnings   = w$items,
    server     = tciApiServerBlock(receivedAt, Sys.time())
  )
  # The belt: nothing a pump acts on may be NaN or infinite.
  tciApiCheckFinite(out[c("rate", "segment", "fallback", "state", "prediction", "delivered")])
  out
}

# Every number in the parts of a response a pump acts on must be finite.  The
# validation bounds every input, so a NaN or an Inf here is a fault of the
# server's own arithmetic, and the request is refused as 500 INTERNAL rather
# than answered with a number no pump can run.  Walks the lists; strings,
# logicals and nulls are not numbers.
tciApiCheckFinite <- function(x, pointer = "")
{
  if (is.list(x)) {
    nm <- names(x)
    for (i in seq_along(x)) {
      key <- if (is.null(nm) || !nzchar(nm[i])) as.character(i - 1L) else nm[i]
      tciApiCheckFinite(x[[i]], paste0(pointer, "/", key))
    }
  } else if (is.numeric(x) && !all(is.finite(x))) {
    tciApiStop(500L, "INTERNAL",
               sprintf("the server computed a number that is not finite at %s", pointer), pointer)
  }
  invisible(x)
}


# ---------------------------------------------------------------------------
# The other endpoints

#' The TCI API's drug list
#'
#' The body of \code{GET /tci/v1/drugs}: every drug whose library row offers a
#' target unit, with its concentration and mass units, typical target range,
#' hard target ceiling, time to peak effect (for the reference patient
#' named in the body) and citation; the accepted unit tokens; and the API's
#' limits (the \code{TCI_API_*} constants and the covariate bounds).
#'
#' @returns a list ready for
#'   \code{jsonlite::toJSON(auto_unbox = TRUE, digits = NA, null = "null")}
#'
#' @examples
#' drugs <- tciApiDrugs()
#' vapply(drugs$drugs, `[[`, "", "drug")
#'
#' @export
tciApiDrugs <- function()
{
  reference <- list(weight = 70, height = 170, age = 50, sex = "male")
  drugs <- lapply(tciApiDrugNames(), function(d) {
    defaults <- getDrugDefaults(d)
    PK <- tciApiPK(d, reference$weight, reference$height, reference$age, reference$sex, TRUE)
    units <- tciApiBaseUnits(defaults$Concentration.Units)
    offered <- defaults$Units[[1]]
    list(
      drug               = d,
      concentrationUnits = units$concentration,
      massUnits          = units$mass,
      sites              = I(c(if (TCI_UNIT_EFFECT %in% offered) "effect",
                               if (TCI_UNIT_PLASMA %in% offered) "plasma")),
      typicalTarget      = defaults$Typical,
      typicalRange       = list(lower = defaults$Lower, upper = defaults$Upper),
      targetCeiling      = TCI_API_TARGET_CEILING * defaults$Upper,
      tPeak              = PK$tPeak,
      reference          = PK$reference
    )
  })
  list(
    apiVersion = TCI_API_VERSION,
    drugs = drugs,
    referencePatient = reference,
    units = list(
      rate          = I(TCI_API_RATE_UNITS),
      amount        = I(TCI_API_AMOUNT_UNITS),
      bolus         = I(TCI_API_BOLUS_UNITS),
      concentration = I(TCI_API_CONCENTRATION_UNITS),
      infusate      = I(TCI_API_INFUSATE_UNITS),
      volume        = I(TCI_API_VOLUME_UNITS)
    ),
    limits = list(
      minInterval         = TCI_API_MIN_INTERVAL,
      maxInterval         = TCI_API_MAX_INTERVAL,
      maxHistory          = TCI_API_MAX_HISTORY,
      lookAheadMax        = TCI_API_LOOKAHEAD_MAX,
      historyClockSkew    = TCI_API_CLOCK_SKEW,
      serverClockSkew     = TCI_API_SERVER_SKEW,
      maxRate             = TCI_API_MAX_RATE,
      minInfusate         = TCI_API_MIN_INFUSATE,
      targetCeiling       = TCI_API_TARGET_CEILING,
      onTarget            = TCI_API_ON_TARGET,
      deliveryTolerance   = TCI_API_DELIVERY_TOLERANCE,
      deliveryToleranceMl = TCI_API_DELIVERY_TOLERANCE_ML,
      plasmaSwitch        = TCI_PLASMA_SWITCH,
      weight              = list(min = MIN_WEIGHT, max = MAX_WEIGHT),
      height              = list(min = MIN_HEIGHT, max = MAX_HEIGHT),
      age                 = list(min = MIN_AGE, max = MAX_AGE),
      sex                 = I(SEX_VALUES),
      pumpStates          = I(TCI_API_PUMP_STATES)
    )
  )
}

#' The TCI API's model block for a patient
#'
#' The body of \code{GET /tci/v1/drugs/{drug}/model}, and the \code{model}
#' block of every rate response: the drug's PK parameters as resolved for the
#' patient, with the units, the time to peak effect, the citation and an MD5
#' hash of the parameters.  Query-string values (\code{"70"}, \code{"true"})
#' are accepted for the numbers and the switch.
#'
#' @param drug the drug, a library id such as \code{"propofol"}
#' @param weight weight in kg (\code{MIN_WEIGHT} to \code{MAX_WEIGHT})
#' @param height height in cm (\code{MIN_HEIGHT} to \code{MAX_HEIGHT})
#' @param age age in years (\code{MIN_AGE} to \code{MAX_AGE})
#' @param sex \code{"male"} or \code{"female"}
#' @param adjustToFFM the app's fat-free-mass switch, passed to
#'   \code{getDrugPK()}; models with their own size covariate ignore it
#'
#' @returns a list ready for
#'   \code{jsonlite::toJSON(auto_unbox = TRUE, digits = NA, null = "null")}.
#'   Signals \code{tci_api_error} for an unknown or non-TCI drug or a
#'   covariate out of range.
#'
#' @examples
#' tciApiModel("propofol", 70, 170, 50, "male")$parameters$v1
#'
#' @export
tciApiModel <- function(drug, weight, height, age, sex, adjustToFFM = TRUE)
{
  drug <- tciApiCheckDrug(drug)
  weight <- tciApiNumber(tciApiCoerceNumber(weight), "/weight", min = MIN_WEIGHT, max = MAX_WEIGHT)
  height <- tciApiNumber(tciApiCoerceNumber(height), "/height", min = MIN_HEIGHT, max = MAX_HEIGHT)
  age    <- tciApiNumber(tciApiCoerceNumber(age), "/age", min = MIN_AGE, max = MAX_AGE)
  sex    <- tciApiString(sex, "/sex", allowed = SEX_VALUES)
  adjustToFFM <- tciApiLogical(tciApiCoerceLogical(adjustToFFM), "/adjustToFFM", required = FALSE)
  if (is.null(adjustToFFM)) adjustToFFM <- TRUE
  PK <- tciApiPK(drug, weight, height, age, sex, adjustToFFM)
  tciApiModelBlock(PK, adjustToFFM)
}

#' The TCI API's liveness body
#'
#' The body of \code{GET /tci/v1/health}: the API version, the package
#' version and commit, the server's clock and the number of TCI drugs.
#'
#' @param now the server's clock (POSIXct), reported as \code{serverTime}
#'
#' @returns a list ready for
#'   \code{jsonlite::toJSON(auto_unbox = TRUE, digits = NA, null = "null")}
#'
#' @examples
#' tciApiHealth()$tciDrugs
#'
#' @export
tciApiHealth <- function(now = Sys.time())
{
  desc <- tciApiDescription()
  list(
    apiVersion = TCI_API_VERSION,
    status     = "ok",
    package    = "stanpumpR",
    version    = as.character(desc$Version),
    commit     = if (is.null(desc$RemoteSha)) NULL else as.character(desc$RemoteSha),
    serverTime = tciApiFormatTime(tciApiSeconds(now)),
    tciDrugs   = length(tciApiDrugNames())
  )
}
