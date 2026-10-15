# The TCI API (tci-api.R): docs/tci-api.md, version 1.0.  Drafted by Claude
# Code, 2026-10-12, at the request of Steven L. Shafer; run on R 4.3.3.
#
# The core, without HTTP: tciNextRate() and its companions are driven by a
# simulated pump controller in closed loop (simulatePump(), below), and the
# true concentrations are tracked by the same closed form the app uses, so
# that what the API predicts can be checked against what the pump would have
# produced.  The router is tested in test-tci-api-http.R.

local_mocked_bindings(outputComments = function(...) {})

# ---------------------------------------------------------------------------
# Helpers

START <- as.numeric(lubridate::ymd_hms("2026-10-11T14:00:00Z", tz = "UTC"))
noEvents <- data.frame(Time = double(), Event = character())
TCI_DRUGS <- c("propofol", "remifentanil", "fentanyl", "alfentanil", "sufentanil",
               "hydromorphone", "oxycodone", "ketamine", "etomidate", "lidocaine")

atMin <- function(min) tciApiFormatTime(START + min * 60)
minutesOf <- function(stamp) (as.numeric(lubridate::ymd_hms(stamp, tz = "UTC")) - START) / 60
pkOf <- function(drug, weight = 70, height = 170, age = 50)
  getDrugPK(drug, weight, height, age, "male", getDrugDefaults(drug))
massUnitOf <- function(PK) if (PK$Concentration.Units == "ng") "mcg" else "mg"
concUnitOf <- function(PK) if (PK$Concentration.Units == "ng") "ng/mL" else "mcg/mL"
pct <- function(x, target) 100 * (x / target - 1)

# A well-formed request, propofol to 3 mcg/mL by default.
baseRequest <- function(drug = "propofol", now = atMin(0), target = 3, site = "effect",
                        units = "mcg/mL", rateUnits = "mg/min", maxRate = 1e5,
                        latency = 0, interval = 10, history = list(), requestId = "ch1-000001")
{
  list(
    requestId = requestId, drug = drug, now = now,
    patient = list(weight = 70, height = 170, age = 50, sex = "male"),
    pump = list(rateUnits = rateUnits, maxRate = maxRate),
    target = list(site = site, value = target, units = units),
    timing = list(latency = latency, interval = interval),
    history = history
  )
}

# Call the API with the server's clock set to the request's own `now`, so that
# CLOCK_SKEW never fires by accident.
api <- function(req, drug = req[["drug"]], now = req[["now"]])
  tciNextRate(req, drug = drug, receivedAt = as.numeric(lubridate::ymd_hms(now, tz = "UTC")))

apiError <- function(req, drug = req[["drug"]])
{
  tryCatch(api(req, drug = drug), tci_api_error = function(e) e)
}

expectApiError <- function(req, status, code, pointer, drug = req[["drug"]], info = code)
{
  e <- apiError(req, drug = drug)
  expect_s3_class(e, "tci_api_error")
  expect_s3_class(e, "error")
  expect_equal(e$status, status, info = info)
  expect_equal(e$code, code, info = info)
  expect_equal(e$pointer, pointer, info = info)
  invisible(e)
}

warningCodes <- function(resp) vapply(resp$warnings, `[[`, "", "code")

rateChange <- function(min, rate, units = "mg/min") list(time = atMin(min), rate = rate, units = units)
segment <- function(from, to, delivered, units = "mg")
  list(from = atMin(from), to = atMin(to), delivered = delivered, units = units)
bolus <- function(min, amount, units = "mg") list(time = atMin(min), bolus = amount, units = units)

# The TCI dose table as simCpCe() hands it to tciSchedule(): one target row,
# already in base units and flagged.
convertedTarget <- function(target = 3, drug = "propofol")
  data.frame(Drug = drug, Time = 0, Dose = target, Units = TCI_UNIT_EFFECT, Bolus = FALSE,
             PO = FALSE, IM = FALSE, IN = FALSE, SL = FALSE, RA = FALSE, RAslow = FALSE,
             PO2 = FALSE, RArate = FALSE)

# A pump controller in closed loop with the API.
#
# Time is in minutes from START.  Each request carries the whole history.  The
# pump switches `actualLatency` s after a response it accepts (the request
# declares `latency`); the true state follows the same closed form as the
# app's tciSchedule() (tciAdvance()), sampled every second for the maximum of
# Ce.  `accept(i, t)` says whether response i, requested at t, reaches the
# pump.  One that does not is "no response" (section 7, rule 4): at holdUntil
# the pump runs the fallback for fallback.maxSeconds (lateRule "fallback"),
# or -- the old rule the design review rejected -- the commanded rate for one
# more interval (lateRule "hold"), and then stops.  `follow` is when the
# controller asks again: every interval, or at the response's nextRequestAt.
# A rate change is recorded only when the rate changes, as a pump would
# record it; recordRepeats = TRUE records every command.
simulatePump <- function(drug = "propofol", target = 3, site = "effect", interval = 10,
                         latency = 0, actualLatency = latency, rateUnits = "mg/min",
                         maxRate = 1e5, minRate = NULL, rateResolution = NULL,
                         infusate = NULL, minutes = 10, weight = 70, height = 170, age = 50,
                         accept = function(i, t) TRUE, lateRule = c("fallback", "hold"),
                         follow = c("interval", "nextRequestAt"), recordRepeats = FALSE,
                         extra = list())
{
  lateRule <- match.arg(lateRule)
  follow <- match.arg(follow)
  PK <- pkOf(drug, weight, height, age)
  K <- tciKinetics(PK)
  base <- massUnitOf(PK)
  concBase <- if (is.null(infusate)) NA_real_ else tciApiInfusateBase(infusate, base)
  toBase <- function(v) tciApiToBase(v, rateUnits, base, weight, concBase)

  S <- list(p = numeric(3), e = numeric(4))   # the true state ...
  tState <- 0                                 # ... at this time
  pumpRate <- 0                               # the pump's actual rate, base/min
  pumpValue <- 0                              # the same in the pump's units
  history <- list()
  responses <- list()
  ceTrace <- numeric(0)
  ceTime <- numeric(0)

  advanceTo <- function(t) {
    if (t <= tState) return(invisible())
    tau <- seq_len(floor((t - tState) * 60 + 1e-9)) / 60
    if (length(tau)) {
      d4 <- exp(-outer(K$lambda4, tau))
      ce <- as.vector(S$e %*% d4) + pumpRate * as.vector(K$einf %*% (1 - d4))
      ceTrace <<- c(ceTrace, ce)
      ceTime <<- c(ceTime, tState + tau)
    }
    S <<- tciAdvance(S, t - tState, pumpRate, K)
    tState <<- t
  }
  setRate <- function(t, value) {
    advanceTo(t)
    if (recordRepeats || value != pumpValue)
      history[[length(history) + 1]] <<- list(time = atMin(t), rate = value, units = rateUnits)
    pumpValue <<- value
    pumpRate <<- toBase(value)
  }

  t <- 0
  i <- 0
  pending <- NULL
  while (t < minutes - 1e-9) {
    i <- i + 1
    req <- baseRequest(drug = drug, now = atMin(t), target = target, site = site,
                       units = concUnitOf(PK), rateUnits = rateUnits, maxRate = maxRate,
                       latency = latency, interval = interval, history = history,
                       requestId = sprintf("sim-%06d", i))
    req$patient <- list(weight = weight, height = height, age = age, sex = "male")
    if (!is.null(minRate)) req$pump$minRate <- minRate
    if (!is.null(rateResolution)) req$pump$rateResolution <- rateResolution
    if (!is.null(infusate)) req$infusate <- infusate
    for (key in names(extra)) req[[key]] <- extra[[key]]
    r <- api(req)
    responses[[i]] <- r
    accepted <- accept(i, t)
    tSwitch <- t + actualLatency / 60
    tNext <- if (follow == "interval") t + interval / 60 else minutesOf(r$timing$nextRequestAt)
    if (tNext <= t + 1e-9) tNext <- t + 1 / 60
    # A pending fallback or stop happens unless a newer command lands first;
    # one landing exactly at holdUntil is the newer command.
    limit <- if (accepted) tSwitch - 1e-9 else tNext + 1e-9
    while (!is.null(pending) && pending$at <= limit) {
      setRate(pending$at, pending$rate)
      pending <- if (is.finite(pending$stopAt))
        list(at = pending$stopAt, rate = 0, stopAt = Inf) else NULL
    }
    if (accepted) {
      setRate(tSwitch, r$rate$value)
      holdEnd <- tSwitch + interval / 60
      pending <- list(at = holdEnd,
                      rate = if (lateRule == "fallback") r$fallback$rate else r$rate$value,
                      stopAt = holdEnd + r$fallback$maxSeconds / 60)
    }
    t <- tNext
  }
  advanceTo(minutes)
  list(responses = responses, history = history, state = S, PK = PK, K = K,
       rates = vapply(responses, function(r) r$rate$value, 0),
       ce = ceTrace, ceTime = ceTime,
       maxCe = max(ceTrace), maxCeAt = ceTime[which.max(ceTrace)])
}

# ---------------------------------------------------------------------------
# 1. Equivalence with the app

test_that("a closed loop of API calls reproduces the app's TCI schedule rate for rate", {
  PK <- pkOf("propofol")
  for (minutes in c(60, 120)) {
    X <- simCpCe(data.frame(Drug = "propofol", Time = 0, Dose = 3, Units = TCI_UNIT_EFFECT),
                 noEvents, PK, minutes, FALSE)
    app <- X$tci$rates$Rate * 70                       # the display is per kg
    loop <- simulatePump(minutes = minutes, recordRepeats = TRUE)
    expect_length(loop$rates, length(app))
    expect_equal(loop$rates, app, tolerance = 1e-10)
    # The off period is genuinely off: an exact zero, not a rounding residual.
    expect_true(any(loop$rates == 0))
  }
})

test_that("recording only the rate changes gives the same loop as recording every command", {
  every <- simulatePump(minutes = 20, recordRepeats = TRUE)
  changes <- simulatePump(minutes = 20, recordRepeats = FALSE)
  expect_equal(changes$rates, every$rates, tolerance = 1e-12)
  expect_lt(length(changes$history), length(every$history))
})

test_that("a capped pump reproduces the app's capped schedule", {
  PK <- pkOf("propofol")
  cap <- 50                                            # mg/min: 300 mL/hr of 1%
  app <- tciSchedule(convertedTarget(), PK, 60, maxRate = cap)$rates$Rate
  loop <- simulatePump(minutes = 60, maxRate = cap, recordRepeats = TRUE)
  expect_equal(loop$rates, app, tolerance = 1e-10)
  expect_equal(sum(loop$rates == cap), sum(app == cap))
  expect_gt(sum(loop$rates == cap), 3)
})

# ---------------------------------------------------------------------------
# 2. Engine cross-check

test_that("the state the API reports agrees with simCpCe() on the same delivery", {
  loop <- simulatePump(minutes = 60, recordRepeats = TRUE)
  final <- api(baseRequest(now = atMin(60), history = loop$history))
  dose <- data.frame(
    Drug = "propofol",
    Time = vapply(loop$history, function(h) minutesOf(h$time), 0),
    Dose = vapply(loop$history, function(h) h$rate, 0),
    Units = "mg/min"
  )
  X <- simCpCe(dose, noEvents, loop$PK, 60, FALSE)
  engine <- function(site) {
    r <- X$results[X$results$Site == site, ]
    r$Y[which.min(abs(r$Time - 60))]
  }
  expect_equal(final$state$now$cp, engine("Plasma"), tolerance = 1e-6)
  expect_equal(final$state$now$ce, engine("Effect Site"), tolerance = 1e-6)
  # and with the simulator's own recurrence
  expect_equal(final$state$now$cp, sum(loop$state$p), tolerance = 1e-9)
  expect_equal(final$state$now$ce, sum(loop$state$e), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 3. Latency

test_that("with the declared latency the loop reaches the target without overshoot and holds", {
  PK <- pkOf("propofol")
  for (actual in c(1.5, 1.0)) {
    loop <- simulatePump(latency = 1.5, actualLatency = actual, minutes = 6)
    expect_lte(loop$maxCe, 3 * 1.002, label = sprintf("max Ce, actual latency %g s", actual))
    late <- loop$ce[loop$ceTime > 2 * PK$tPeak]
    expect_lt(max(abs(late / 3 - 1)), 0.005, label = sprintf("hold, actual latency %g s", actual))
    # The response reports the delay it planned for.
    r <- loop$responses[[1]]
    expect_equal(minutesOf(r$timing$effectiveAt) * 60, 1.5)
    expect_equal(minutesOf(r$timing$holdUntil) * 60, 11.5)
    expect_equal(minutesOf(r$timing$nextRequestAt) * 60, 10)
  }
})

test_that("a latency 1.5 s longer than declared overshoots the loading by about 15% (section 5.5)", {
  loop <- simulatePump(latency = 1.5, actualLatency = 3.0, minutes = 6)
  over <- pct(loop$maxCe, 3)
  expect_gt(over, 10)
  expect_lt(over, 20)
  expect_equal(over, 15, tolerance = 0.05)
})

# ---------------------------------------------------------------------------
# 4. Fallback

test_that("running the fallback when responses are lost during loading never overshoots; holding the loading rate doubles the dose", {
  lostDuringLoading <- function(i, t) i == 1 || t >= 2
  safe <- simulatePump(minutes = 6, accept = lostDuringLoading, lateRule = "fallback")
  expect_lte(safe$maxCe, 3 * 1.01)
  # The fallback after the loading interval is "off until the peak" ...
  expect_equal(safe$responses[[1]]$fallback$rate, 0)
  expect_equal(safe$responses[[1]]$fallback$maxSeconds, 10)
  expect_equal(safe$responses[[1]]$fallback$units, "mg/min")
  # ... and in maintenance it is the next maintenance rate.
  last <- safe$responses[[length(safe$responses)]]
  expect_gt(last$fallback$rate, 0)
  expect_lt(abs(last$fallback$rate / last$rate$value - 1), 0.05)

  old <- simulatePump(minutes = 6, accept = lostDuringLoading, lateRule = "hold")
  expect_gt(old$maxCe, 3 * 1.5)
  expect_equal(pct(old$maxCe, 3), 100, tolerance = 0.02)
})

# ---------------------------------------------------------------------------
# 5. Interval

test_that("one plan then pump off touches the target at every interval (the grid depends on dt)", {
  K <- tciKinetics(pkOf("propofol"))
  for (sec in c(1, 5, 10, 30, 60, 120)) {
    dt <- sec / 60
    rate <- tciRate(numeric(3), numeric(4), 3, "effect", dt, K)$rate
    e <- K$einf * rate * (1 - exp(-K$lambda4 * dt))
    tau <- seq(0, 10, by = 1 / 600)
    peak <- max(as.vector(e %*% exp(-outer(K$lambda4, tau))))
    expect_equal(peak, 3, tolerance = 1e-4, label = sprintf("peak after a %d s plan", sec))
    expect_lte(peak, 3 * (1 + 1e-4))
  }
})

test_that("every TCI drug reaches its target at intervals of 1, 5 and 10 s with at most 0.2% overshoot", {
  for (drug in TCI_DRUGS) {
    PK <- pkOf(drug)
    rateUnits <- paste0(massUnitOf(PK), "/min")
    for (I in c(1, 5, 10)) {
      loop <- simulatePump(drug = drug, target = PK$typical, interval = I, rateUnits = rateUnits,
                           minutes = PK$tPeak + 0.5, follow = "nextRequestAt")
      expect_lte(loop$maxCe, PK$typical * 1.002, label = sprintf("%s at %d s", drug, I))
      expect_gt(loop$maxCe, PK$typical * 0.99, label = sprintf("%s at %d s reaches the target", drug, I))
    }
  }
})

# At 30 and 60 s the loop still reaches the target, but the fast drugs
# overshoot: once the effect site is within 5% of the target -- which happens
# before the peak -- the controller hands off to the plasma, whose plan only
# lands Cp on the target at the END of the interval, and over a long interval
# the plasma above target meanwhile pushes Ce over.  That is the Shafer-Gregg
# hand-off at a long interval, not the search grid (the test above shows one
# plan touching the target exactly at 60 and 120 s), and it is why the
# response warns INTERVAL_LONG above tPeak / 3.  Measured on 2026-10-14:
# propofol and remifentanil up to 2.6% at 60 s, and etomidate 4.2% at 60 s
# (1.4% at 30 s) since its ke0 became Arden's 0.433 /min with a 1.9 min peak,
# the slowest of the fast drugs against a 60 s interval.  The figures are
# pinned here so that a change in either direction is noticed.
test_that("at 30 and 60 s every TCI drug reaches its target within the hand-off's excursion", {
  for (drug in TCI_DRUGS) {
    PK <- pkOf(drug)
    rateUnits <- paste0(massUnitOf(PK), "/min")
    for (I in c(30, 60)) {
      loop <- simulatePump(drug = drug, target = PK$typical, interval = I, rateUnits = rateUnits,
                           minutes = PK$tPeak + 1.5, follow = "nextRequestAt")
      bound <- if (drug == "etomidate" && I == 60) 1.045 else 1.03
      expect_lte(loop$maxCe, PK$typical * bound, label = sprintf("%s at %d s", drug, I))
      expect_gt(loop$maxCe, PK$typical * 0.99, label = sprintf("%s at %d s reaches the target", drug, I))
    }
  }
  etoPK <- pkOf("etomidate")
  etomidate60 <- simulatePump(drug = "etomidate", target = etoPK$typical, interval = 60,
                              rateUnits = paste0(massUnitOf(etoPK), "/min"),
                              minutes = etoPK$tPeak + 1.5, follow = "nextRequestAt")
  expect_equal(etomidate60$maxCe / etoPK$typical, 1.042, tolerance = 0.005)
  propofol60 <- simulatePump(interval = 60, minutes = 5)
  expect_equal(pct(propofol60$maxCe, 3), 1.56, tolerance = 0.05)
  expect_true("INTERVAL_LONG" %in% warningCodes(propofol60$responses[[1]]))
})

# ---------------------------------------------------------------------------
# 6. Cap and resolution

test_that("a rate ceiling spreads the loading over several capped intervals without overshoot", {
  syringe <- list(concentration = 10, units = "mg/mL")
  for (cap in c(1200, 300)) {
    loop <- simulatePump(latency = 1.5, minutes = 6, rateUnits = "mL/hr", maxRate = cap,
                         rateResolution = 0.1, infusate = syringe)
    expect_lte(loop$maxCe, 3 * 1.002, label = sprintf("cap %d mL/hr", cap))
    expect_gt(loop$maxCe, 3 * 0.995, label = sprintf("cap %d mL/hr reaches the target", cap))
    limited <- lapply(loop$responses, function(r) r$rate$limitedBy)
    capped <- vapply(limited, function(l) "maxRate" %in% l, NA)
    expect_true(all(loop$rates[capped] == cap))
    expect_true(all(loop$rates[!capped] < cap))
    expect_equal(sum(capped), if (cap == 1200) 1L else 5L)
    # The partial interval after the capped ones, then off until the peak.
    expect_gt(loop$rates[sum(capped) + 1], 0)
    expect_equal(loop$rates[sum(capped) + 2], 0)
    # Rounding is always down, to a multiple of the resolution.
    ideal <- vapply(loop$responses, function(r) r$rate$ideal, 0)
    expect_true(all(loop$rates <= ideal + 1e-9))
    expect_true(all(abs(loop$rates / 0.1 - round(loop$rates / 0.1)) < 1e-9))
    rounded <- vapply(limited, function(l) "rateResolution" %in% l, NA)
    expect_equal(rounded[!capped], (abs(loop$rates - ideal) > 1e-9)[!capped])
    expect_true(any(rounded))
    # mL/hr is reported beside the pump's units, and the segment in mL.
    r <- loop$responses[[1]]
    expect_equal(r$rate$mlPerHour, r$rate$value)
    expect_equal(r$rate$massPerMinute, r$rate$value * 10 / 60)
    expect_equal(r$segment$volumeMl, r$segment$amount / 10)
    expect_equal(r$segment$holdSeconds, 10)
  }
})

test_that("limitedBy names each limit that changed the value, and minRate sends zero", {
  loop <- simulatePump(minutes = 5)
  maint <- loop$responses[[length(loop$responses)]]
  expect_equal(maint$controller$phase, "maintenance")
  expect_equal(maint$rate$limitedBy, I(character(0)))
  # The same instant as the loop's last request, with the history as it then was.
  req <- baseRequest(now = maint$timing$now,
                     history = loop$history[vapply(loop$history, function(h) minutesOf(h$time), 0) <
                                              minutesOf(maint$timing$now)])
  expect_equal(api(req)$rate$value, maint$rate$value)
  # Below minRate: zero, and the pump is told why.
  req$pump$minRate <- maint$rate$ideal * 2
  r <- api(req)
  expect_equal(r$rate$value, 0)
  expect_equal(r$rate$limitedBy, I("minRate"))
  expect_equal(r$rate$ideal, maint$rate$ideal)
  # With the pump off, the phase reads from the controlled concentration.
  expect_equal(r$controller$phase, if (r$state$effectiveAt$ce > 3) "aboveTarget" else "waitingForPeak")
  # A ceiling below the maintenance rate: the ceiling.
  req$pump$minRate <- NULL
  req$pump$maxRate <- maint$rate$ideal / 2
  r <- api(req)
  expect_equal(r$rate$value, maint$rate$ideal / 2)
  expect_equal(r$rate$limitedBy, I("maxRate"))
  # A resolution: rounded down, never up.
  req$pump$maxRate <- 1e5
  req$pump$rateResolution <- 1
  r <- api(req)
  expect_equal(r$rate$value, floor(maint$rate$ideal))
  expect_equal(r$rate$limitedBy, I("rateResolution"))
  expect_equal(tciApiLimit(24.6, list(maxRate = 100, rateResolution = 0.1))$value, 24.6)
  expect_equal(tciApiLimit(24.6, list(maxRate = 100, rateResolution = 0.1))$limitedBy, character(0))
  expect_equal(tciApiLimit(24.69, list(maxRate = 100, rateResolution = 0.1))$value, 24.6)
  expect_equal(tciApiLimit(0.05, list(maxRate = 100, rateResolution = 0.1))$value, 0)
  expect_equal(tciApiLimit(500, list(maxRate = 100, minRate = 1, rateResolution = 0.3))$limitedBy,
               c("maxRate", "rateResolution"))
})

test_that("a rate that already is a multiple of the resolution is kept as typed", {
  # 9999 * 0.1 is 999.90000000000009 in binary, one ulp above the double for
  # 999.9: the multiple is the rate itself, not a whole step below it.  The
  # first version of tciApiLimit() pulled such a rate back a full step and
  # named rateResolution as a limit: a third of all one-decimal ceilings lost
  # a step that way (999.9 -> 999.8, 36.3 -> 36.2, 1.2 -> 1.1).
  for (v in c(999.9, 36.3, 0.3, 0.6, 0.7, 1.2, 1.4, 2.3)) {
    lim <- tciApiLimit(v, list(maxRate = 1e5, rateResolution = 0.1))
    expect_identical(lim$value, v, label = sprintf("%g at 0.1", v))
    expect_equal(lim$limitedBy, character(0), label = sprintf("%g at 0.1", v))
  }
  for (res in c(0.1, 0.01, 0.05, 0.2)) {
    typed <- as.numeric(sprintf("%.12g", (1:5000) * res))
    out <- vapply(typed, function(v) tciApiLimit(v, list(maxRate = 1e9, rateResolution = res))$value, 0)
    expect_identical(out, typed, label = sprintf("every multiple of %g", res))
  }
  # The result is never above the rate that came in, and a rate that is not a
  # multiple still rounds down.
  expect_equal(tciApiLimit(999.9 + 1e-6, list(maxRate = 1e5, rateResolution = 0.1))$value, 999.9)
  expect_equal(tciApiLimit(999.95, list(maxRate = 1e5, rateResolution = 0.1))$value, 999.9)
  expect_equal(tciApiLimit(999.95, list(maxRate = 1e5, rateResolution = 0.1))$limitedBy, "rateResolution")
  # A ceiling that is a multiple of the resolution is reached exactly, and is
  # the only limit named.
  expect_equal(tciApiLimit(1e4, list(maxRate = 999.9, rateResolution = 0.1)),
               list(value = 999.9, limitedBy = "maxRate"))
  # Through the API: a 1% propofol pump whose ceiling is 36.3 mL/hr at a
  # resolution of 0.1 mL/hr runs the loading at 36.3, limited by the ceiling
  # alone, and falls back to it too.
  req <- baseRequest(rateUnits = "mL/hr", maxRate = 36.3)
  req$infusate <- list(concentration = 10, units = "mg/mL")
  req$pump$rateResolution <- 0.1
  r <- api(req)
  expect_identical(r$rate$value, 36.3)
  expect_equal(r$rate$limitedBy, I("maxRate"))
  expect_identical(r$fallback$rate, 36.3)
  expect_gt(r$rate$ideal, 36.3)
})

test_that("a pump floor that zeroes the loading keeps the usual nextRequestAt: its pump-off peak reaches nothing", {
  r <- api(local({ req <- baseRequest(); req$pump$minRate <- 1000; req }))
  expect_equal(r$rate$value, 0)
  expect_equal(r$rate$limitedBy, I("minRate"))
  expect_gt(r$rate$ideal, 0)
  expect_equal(r$controller$phase, "waitingForPeak")
  expect_equal(r$prediction$peakCe, 0)
  # Nothing is on its way to the target, so the ask is at the hold's end as
  # usual, not (as the peak rule read before) at once.
  expect_equal(r$timing$nextRequestAt, r$timing$holdUntil)
  expect_equal(minutesOf(r$timing$nextRequestAt) * 60, 10)
})

# ---------------------------------------------------------------------------
# 7. History forms

test_that("delivered segments give the same state as the rate changes they summarise", {
  loop <- simulatePump(minutes = 30, recordRepeats = TRUE)
  fromRates <- api(baseRequest(now = atMin(30), history = loop$history))
  times <- vapply(loop$history, function(h) minutesOf(h$time), 0)
  rates <- vapply(loop$history, function(h) h$rate, 0)
  ends <- c(times[-1], 30)
  segments <- Map(function(a, b, r) segment(a, b, r * (b - a)), times, ends, rates)
  fromSegments <- api(baseRequest(now = atMin(30), history = segments))
  expect_equal(fromSegments$state$now, fromRates$state$now, tolerance = 1e-9)
  expect_equal(fromSegments$delivered$fromHistory, fromRates$delivered$fromHistory, tolerance = 1e-9)
  # The pump is running nothing after the last segment: no rate change covers it.
  expect_equal(fromSegments$rate$value, api(baseRequest(now = atMin(30), history = c(segments, list(rateChange(30, 0)))))$rate$value)
  # Merged into one segment per minute, as section 4.2 recommends for a long case.
  minute <- floor(times)
  perMinute <- Map(function(m) {
    inside <- which(minute == m)
    a <- max(m, times[inside[1]]); b <- min(m + 1, 30)
    starts <- pmax(times[inside], a); stops <- pmin(c(times[inside][-1], 30), b)
    segment(a, b, sum(rates[inside] * (stops - starts)))
  }, unique(minute))
  compact <- api(baseRequest(now = atMin(30), history = c(perMinute, list(rateChange(30, rates[length(rates)])))))
  expect_equal(compact$state$now$cp, fromRates$state$now$cp, tolerance = 1e-3)
})

test_that("a delivered segment overrides the rate changes over its span", {
  plain <- api(baseRequest(now = atMin(10), history = list(
    rateChange(0, 10), rateChange(5, 0))))
  overridden <- api(baseRequest(now = atMin(10), history = list(
    rateChange(0, 10), rateChange(2, 500), segment(1, 4, 30), rateChange(5, 0))))
  # Over 1-4 min the segment says 10 mg/min whatever the 500 mg/min rate change
  # claims; after it the last rate change at or before (500 at 2 min) runs
  # from 4 to 5 min.
  expected <- api(baseRequest(now = atMin(10), history = list(
    rateChange(0, 10), rateChange(4, 500), rateChange(5, 0))))
  expect_equal(overridden$state$now, expected$state$now, tolerance = 1e-9)
  expect_equal(overridden$delivered$fromHistory, 10 * 4 + 500 * 1)
  expect_false(isTRUE(all.equal(overridden$state$now$cp, plain$state$now$cp)))
})

test_that("a syringe change mid-case converts each span with the syringe then in force", {
  byMass <- api(baseRequest(now = atMin(10), history = list(
    rateChange(0, 100, "mg/min"), rateChange(5, 100, "mg/min"))))
  # Every syringe is an infusate event, the first one included; the request's
  # infusate is the syringe now on the pump (the last event).
  req <- baseRequest(now = atMin(10), history = list(
    list(time = atMin(0), infusate = list(concentration = 10, units = "mg/mL")),
    rateChange(0, 600, "mL/hr"),                                  # 10 mg/mL: 100 mg/min
    list(time = atMin(5), infusate = list(concentration = 20, units = "mg/mL")),
    rateChange(5, 300, "mL/hr")                                   # 20 mg/mL: 100 mg/min
  ))
  req$infusate <- list(concentration = 20, units = "mg/mL")
  req$pump$rateUnits <- "mL/hr"
  byVolume <- api(req)
  expect_equal(byVolume$state$now, byMass$state$now, tolerance = 1e-9)
  expect_equal(byVolume$delivered$fromHistory, byMass$delivered$fromHistory)
  expect_equal(byVolume$rate$massPerMinute, byMass$rate$massPerMinute, tolerance = 1e-9)
  expect_equal(byVolume$rate$value, byMass$rate$value * 60 / 20, tolerance = 1e-9)
  # Without the first event the syringe before the change is unknown.  The
  # request's syringe is the one on the pump NOW (it must equal the last
  # event), and pricing the earlier millilitres with it would double the
  # modelled dose here and halve it for a change the other way; so the
  # history is refused until the initial syringe is recorded too.
  noFirst <- req
  noFirst$history <- noFirst$history[-1]
  e <- expectApiError(noFirst, 422, "INFUSATE_REQUIRED", "/history/0/units")
  expect_match(e$message, "record the initial syringe as an infusate event")
  # Either direction of change, as a controller following the request's own
  # example (its infusate = the current syringe, one event at the change)
  # would record it.  The true delivery is 100 mg/min throughout.
  change <- function(oldConc, newConc, oldRate, newRate) {
    r <- baseRequest(latency = 1.5, rateUnits = "mL/hr", maxRate = 1200, history = list(
      rateChange(-10, oldRate, "mL/hr"),
      list(time = atMin(-1), infusate = list(concentration = newConc, units = "mg/mL")),
      rateChange(-1, newRate, "mL/hr")))
    r$pump$rateResolution <- 0.1
    r$infusate <- list(concentration = newConc, units = "mg/mL")
    r
  }
  expectApiError(change(20, 10, 300, 600), 422, "INFUSATE_REQUIRED", "/history/0/units")
  expectApiError(change(10, 20, 600, 300), 422, "INFUSATE_REQUIRED", "/history/0/units")
  # A mL segment that starts before the first recorded syringe, or a mL bolus
  # before it, likewise; a mL quantity at the same instant uses that syringe;
  # a mass quantity before it needs no syringe at all.
  first <- list(time = atMin(2), infusate = list(concentration = 20, units = "mg/mL"))
  withFirst <- function(...) {
    r <- baseRequest(now = atMin(10), rateUnits = "mL/hr", history = list(first, ...))
    r$infusate <- list(concentration = 20, units = "mg/mL")
    r
  }
  expectApiError(withFirst(segment(1, 3, 10, "mL"), rateChange(3, 0, "mL/hr")),
                 422, "INFUSATE_REQUIRED", "/history/1/units")
  expectApiError(withFirst(bolus(1, 2, "mL")), 422, "INFUSATE_REQUIRED", "/history/1/units")
  expectApiError(withFirst(rateChange(0, 300, "mL/hr")), 422, "INFUSATE_REQUIRED", "/history/1/units")
  sameInstant <- api(withFirst(rateChange(2, 300, "mL/hr"), rateChange(7, 0, "mL/hr")))
  expect_equal(sameInstant$delivered$fromHistory, 100 * 5)
  massBefore <- api(withFirst(rateChange(0, 100, "mg/min"), rateChange(2, 300, "mL/hr"), rateChange(7, 0, "mL/hr")))
  expect_equal(massBefore$delivered$fromHistory, 100 * 7)
  # The request's syringe must be the last one recorded.
  req$history[[3]]$infusate$concentration <- 10
  expectApiError(req, 422, "INFUSATE_MISMATCH", "/infusate")
})

test_that("a hand bolus enters the state and stops the controller until the target is regained", {
  K <- tciKinetics(pkOf("propofol"))
  r <- api(baseRequest(now = atMin(1), history = list(bolus(0, 20))))
  expect_equal(r$state$now$p, K$pbolus * 20 * exp(-K$lambda3 * 1))
  expect_equal(r$state$now$e, K$ebolus * 20 * exp(-K$lambda4 * 1))
  expect_equal(r$state$now$cp, sum(K$pbolus * 20 * exp(-K$lambda3)))
  # Not through the pump: not in the pump's total.
  expect_equal(r$delivered$fromHistory, 0)
  # 200 mg: the effect site is far above a 3 mcg/mL target.
  big <- api(baseRequest(now = atMin(2), history = list(bolus(0, 200))))
  expect_equal(big$rate$value, 0)
  expect_equal(big$controller$phase, "aboveTarget")
  expect_gt(big$state$effectiveAt$ce, 3)
  # Two boluses at one instant are summed; mg/kg scales by weight; mL by the syringe.
  two <- api(baseRequest(now = atMin(1), history = list(bolus(0, 12), bolus(0, 8))))
  expect_equal(two$state$now, r$state$now)
  perKg <- api(baseRequest(now = atMin(1), history = list(bolus(0, 20 / 70, "mg/kg"))))
  expect_equal(perKg$state$now, r$state$now)
  req <- baseRequest(now = atMin(1), history = list(bolus(0, 2, "mL")))
  req$infusate <- list(concentration = 10, units = "mg/mL")
  expect_equal(api(req)$state$now, r$state$now)
})

test_that("a pump that is not running is projected at rate 0 and says so", {
  running <- baseRequest(now = atMin(10), latency = 5, history = list(rateChange(0, 10)))
  paused <- running
  paused$pump$state <- "paused"
  paused$pump$currentRate <- 999                      # not cross-checked while not running
  r0 <- api(running)
  r1 <- api(paused)
  expect_equal(warningCodes(r0), character(0))
  expect_equal(warningCodes(r1), "PUMP_NOT_RUNNING")
  expect_equal(r1$warnings[[1]]$pointer, "/pump/state")
  expect_equal(r1$state$now, r0$state$now)
  expect_lt(r1$state$effectiveAt$cp, r0$state$effectiveAt$cp)
  K <- tciKinetics(pkOf("propofol"))
  expect_equal(r1$state$effectiveAt$cp, sum(r0$state$now$p * exp(-K$lambda3 * 5 / 60)))
  for (state in TCI_API_PUMP_STATES) {
    paused$pump$state <- state
    expect_equal(length(warningCodes(api(paused))), if (state == "running") 1L else 1L)
  }
})

test_that("the pump's totaliser is checked against the history, hand boluses excluded", {
  history <- list(rateChange(0, 10), rateChange(5, 0), bolus(2, 20))
  req <- baseRequest(now = atMin(10), history = history)
  req$pump$volumeInfused <- list(value = 50, units = "mg")
  r <- api(req)
  expect_equal(r$delivered, list(fromHistory = 50, reported = 50, units = "mg"))
  expect_equal(warningCodes(r), character(0))
  req$pump$volumeInfused <- list(value = 50.9, units = "mg")      # within 2%
  expect_equal(warningCodes(api(req)), character(0))
  req$pump$volumeInfused <- list(value = 52, units = "mg")        # beyond 2%
  r <- api(req)
  expect_equal(warningCodes(r), "DELIVERY_MISMATCH")
  expect_equal(r$warnings[[1]]$pointer, "/pump/volumeInfused")
  expect_equal(r$delivered$reported, 52)
  # In mL, through the syringe, with the 0.5 mL allowance.
  req$infusate <- list(concentration = 10, units = "mg/mL")
  req$pump$volumeInfused <- list(value = 5.4, units = "mL")       # 54 mg: 0.4 mL off
  expect_equal(warningCodes(api(req)), character(0))
  req$pump$volumeInfused <- list(value = 5.6, units = "mL")       # 0.6 mL off
  expect_equal(warningCodes(api(req)), "DELIVERY_MISMATCH")
  # Without a totaliser, nothing is reported.
  expect_null(api(baseRequest(now = atMin(10), history = history))$delivered$reported)
})

test_that("an event a moment after now is clamped to now with a warning; later is refused", {
  exact <- api(baseRequest(now = atMin(10), history = list(rateChange(0, 10), rateChange(10, 0))))
  skewed <- api(baseRequest(now = atMin(10), history = list(rateChange(0, 10), rateChange(10 + 1 / 60, 0))))
  expect_equal(warningCodes(skewed), "HISTORY_CLOCK_SKEW")
  expect_equal(skewed$warnings[[1]]$pointer, "/history/1/time")
  expect_equal(skewed$state$now, exact$state$now)
  expect_equal(skewed$rate$value, exact$rate$value)
  expectApiError(baseRequest(now = atMin(10), history = list(rateChange(10 + 3 / 60, 0))),
                 422, "HISTORY_IN_FUTURE", "/history/0/time")
  expectApiError(baseRequest(now = atMin(10), history = list(segment(9, 10 + 3 / 60, 1))),
                 422, "HISTORY_IN_FUTURE", "/history/0/to")
})

test_that("two rate changes at one instant are contradictory", {
  expectApiError(baseRequest(now = atMin(10), history = list(rateChange(0, 10), rateChange(0, 20))),
                 422, "HISTORY_INVALID", "/history/1")
  # Sorted by time, whatever the order in the array.
  a <- api(baseRequest(now = atMin(10), history = list(rateChange(5, 0), rateChange(0, 10))))
  b <- api(baseRequest(now = atMin(10), history = list(rateChange(0, 10), rateChange(5, 0))))
  expect_equal(a$state$now, b$state$now)
})

test_that("the running rate is the history's, and pump.currentRate only cross-checks it", {
  req <- baseRequest(now = atMin(10), history = list(rateChange(0, 10)))
  req$pump$currentRate <- 10
  expect_equal(warningCodes(api(req)), character(0))
  req$pump$currentRate <- 12
  r <- api(req)
  expect_equal(warningCodes(r), "RATE_MISMATCH")
  expect_equal(r$warnings[[1]]$pointer, "/pump/currentRate")
  # The record wins: the state is the same whatever currentRate says.
  expect_equal(r$state$now, api(baseRequest(now = atMin(10), history = list(rateChange(0, 10))))$state$now)
  # Within the pump's resolution is not a mismatch.
  req$pump$currentRate <- 10.04
  req$pump$rateResolution <- 0.1
  expect_equal(warningCodes(api(req)), character(0))
})

# ---------------------------------------------------------------------------
# 8. Validation

test_that("every error code is raised with its status and pointer", {
  json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)
  mutate <- function(f) { req <- baseRequest(history = list(rateChange(0, 10))); f(req) }
  cases <- list(
    # 400
    list(mutate(function(r) { r$patient$weight <- "70"; r }), 400, "INVALID_TYPE", "/patient/weight"),
    list(mutate(function(r) { r$patient$weight <- TRUE; r }), 400, "INVALID_TYPE", "/patient/weight"),
    list(mutate(function(r) { r$now <- 1791729125; r }), 400, "INVALID_TYPE", "/now"),
    list(mutate(function(r) { r$history[[1]]$time <- 1791729125; r }), 400, "INVALID_TYPE", "/history/0/time"),
    list(mutate(function(r) { r$history[[1]]$rate <- "10"; r }), 400, "INVALID_TYPE", "/history/0/rate"),
    list(mutate(function(r) { r$history <- list(a = 1); r }), 400, "INVALID_TYPE", "/history"),
    list(mutate(function(r) { r$history <- list(5); r }), 400, "INVALID_TYPE", "/history/0"),
    list(mutate(function(r) { r$patient <- 5; r }), 400, "INVALID_TYPE", "/patient"),
    list(mutate(function(r) { r$options <- list(adjustToFFM = "true"); r }), 400, "INVALID_TYPE", "/options/adjustToFFM"),
    list(mutate(function(r) { r$target$site <- 1; r }), 400, "INVALID_TYPE", "/target/site"),
    # An empty object or array is a present value of the wrong type; only null is absent.
    list(mutate(function(r) { r$history[[1]]$rate <- json("{}"); r }), 400, "INVALID_TYPE", "/history/0/rate"),
    list(mutate(function(r) { r$history[[1]]$rate <- json("[]"); r }), 400, "INVALID_TYPE", "/history/0/rate"),
    list(mutate(function(r) { r$history[[1]]$units <- json("{}"); r }), 400, "INVALID_TYPE", "/history/0/units"),
    list(mutate(function(r) { r$history[[1]]$time <- json("[]"); r }), 400, "INVALID_TYPE", "/history/0/time"),
    list(mutate(function(r) { r$history <- list(list(time = atMin(0), infusate = json("{}"))); r }), 400, "MISSING_FIELD", "/history/0/infusate/concentration"),
    list(mutate(function(r) { r$history <- list(list(time = atMin(0), infusate = json("[]"))); r }), 400, "INVALID_TYPE", "/history/0/infusate"),
    list(mutate(function(r) { r$now <- NULL; r }), 400, "MISSING_FIELD", "/now"),
    list(json('{"drug":"propofol","now":null,"patient":{"weight":70,"height":170,"age":50,"sex":"male"},"pump":{"rateUnits":"mg/min","maxRate":100},"target":{"site":"effect","value":3,"units":"mcg/mL"},"timing":{"latency":0,"interval":10},"history":[]}'),
         400, "MISSING_FIELD", "/now"),
    list(mutate(function(r) { r$patient$sex <- NULL; r }), 400, "MISSING_FIELD", "/patient/sex"),
    list(mutate(function(r) { r$timing$latency <- NULL; r }), 400, "MISSING_FIELD", "/timing/latency"),
    list(mutate(function(r) { r$history <- NULL; r }), 400, "MISSING_FIELD", "/history"),
    list(mutate(function(r) { r$history[[1]]$units <- NULL; r }), 400, "MISSING_FIELD", "/history/0/units"),
    list(mutate(function(r) { r$pump$rateUnits <- NULL; r }), 400, "MISSING_FIELD", "/pump/rateUnits"),
    list(mutate(function(r) { r$foo <- 1; r }), 400, "UNKNOWN_FIELD", "/foo"),
    list(mutate(function(r) { r$patient$weigth <- 70; r }), 400, "UNKNOWN_FIELD", "/patient/weigth"),
    list(mutate(function(r) { r$history[[1]]$rateUnits <- "mg/min"; r }), 400, "UNKNOWN_FIELD", "/history/0/rateUnits"),
    list(mutate(function(r) { r$pump$volumeInfused <- list(value = 1, units = "mg", source = "x"); r }), 400, "UNKNOWN_FIELD", "/pump/volumeInfused/source"),
    list(json('{"drug":"propofol","now":"2026-10-11T14:00:00Z","patient":{"weight":70,"weight":71,"height":170,"age":50,"sex":"male"},"pump":{"rateUnits":"mg/min","maxRate":100},"target":{"site":"effect","value":3,"units":"mcg/mL"},"timing":{"latency":0,"interval":10},"history":[]}'),
         400, "DUPLICATE_KEY", "/patient/weight"),
    list(json('{"drug":"propofol","now":"2026-10-11T14:00:00Z","patient":{"weight":70,"height":170,"age":50,"sex":"male"},"pump":{"rateUnits":"mg/min","maxRate":100},"target":{"site":"effect","value":3,"units":"mcg/mL"},"timing":{"latency":0,"interval":10},"history":[{"time":"2026-10-11T13:00:00Z","time":"2026-10-11T13:00:00Z","rate":1,"units":"mg/min"}]}'),
         400, "DUPLICATE_KEY", "/history/0/time"),
    list(mutate(function(r) { r$drug <- "remifentanil"; r }), 400, "DRUG_MISMATCH", "/drug"),
    # 413
    list(mutate(function(r) { r$history <- rep(list(rateChange(0, 1)), TCI_API_MAX_HISTORY + 1L); r }), 413, "HISTORY_TOO_LONG", "/history"),
    # 422
    list(mutate(function(r) { r$patient$weight <- 0; r }), 422, "INVALID_VALUE", "/patient/weight"),
    list(json('{"drug":"propofol","now":"2026-10-11T14:00:00Z","patient":{"weight":1e400,"height":170,"age":50,"sex":"male"},"pump":{"rateUnits":"mg/min","maxRate":100},"target":{"site":"effect","value":3,"units":"mcg/mL"},"timing":{"latency":0,"interval":10},"history":[]}'),
         422, "INVALID_VALUE", "/patient/weight"),
    list(mutate(function(r) { r$patient$height <- 5; r }), 422, "INVALID_VALUE", "/patient/height"),
    list(mutate(function(r) { r$patient$age <- 91; r }), 422, "INVALID_VALUE", "/patient/age"),
    list(mutate(function(r) { r$patient$sex <- "Male"; r }), 422, "INVALID_VALUE", "/patient/sex"),
    list(mutate(function(r) { r$target$site <- "brain"; r }), 422, "INVALID_VALUE", "/target/site"),
    list(mutate(function(r) { r$target$value <- -1; r }), 422, "INVALID_VALUE", "/target/value"),
    list(mutate(function(r) { r$timing$interval <- 0.5; r }), 422, "INVALID_VALUE", "/timing/interval"),
    list(mutate(function(r) { r$timing$interval <- 61; r }), 422, "INVALID_VALUE", "/timing/interval"),
    list(mutate(function(r) { r$timing$latency <- -1; r }), 422, "INVALID_VALUE", "/timing/latency"),
    list(mutate(function(r) { r$pump$maxRate <- 0; r }), 422, "INVALID_VALUE", "/pump/maxRate"),
    list(mutate(function(r) { r$pump$maxRate <- 2e5; r }), 422, "INVALID_VALUE", "/pump/maxRate"),
    list(mutate(function(r) { r$pump$minRate <- r$pump$maxRate; r }), 422, "INVALID_VALUE", "/pump/minRate"),
    list(mutate(function(r) { r$pump$rateResolution <- r$pump$maxRate / 5; r }), 422, "INVALID_VALUE", "/pump/rateResolution"),
    list(mutate(function(r) { r$pump$state <- "stopped"; r }), 422, "INVALID_VALUE", "/pump/state"),
    list(mutate(function(r) { r$pump$volumeInfused <- list(value = -1, units = "mg"); r }), 422, "INVALID_VALUE", "/pump/volumeInfused/value"),
    list(mutate(function(r) { r$requestId <- strrep("x", 129); r }), 422, "INVALID_VALUE", "/requestId"),
    list(mutate(function(r) { r$requestId <- "caf\u00e9"; r }), 422, "INVALID_VALUE", "/requestId"),
    list(mutate(function(r) { r$history[[1]]$rate <- -1; r }), 422, "INVALID_VALUE", "/history/0/rate"),
    list(mutate(function(r) { r$history <- list(bolus(0, 0)); r }), 422, "INVALID_VALUE", "/history/0/bolus"),
    list(mutate(function(r) { r$history <- list(segment(5, 5, 1)); r }), 422, "INVALID_VALUE", "/history/0/to"),
    list(mutate(function(r) { r$history[[1]]$source <- strrep("s", 33); r }), 422, "INVALID_VALUE", "/history/0/source"),
    list(mutate(function(r) { r$lookAhead <- list(concentration = 0); r }), 422, "INVALID_VALUE", "/lookAhead/concentration"),
    list(mutate(function(r) { r$infusate <- list(concentration = 0, units = "mg/mL"); r }), 422, "INVALID_VALUE", "/infusate/concentration"),
    list(mutate(function(r) { r$pump$rateUnits <- "mL/h"; r }), 422, "UNIT_UNKNOWN", "/pump/rateUnits"),
    list(mutate(function(r) { r$history[[1]]$units <- "\u00b5g/min"; r }), 422, "UNIT_UNKNOWN", "/history/0/units"),
    list(mutate(function(r) { r$target$units <- "mg/L"; r }), 422, "UNIT_UNKNOWN", "/target/units"),
    list(mutate(function(r) { r$target$units <- "ug/ml"; r }), 422, "UNIT_UNKNOWN", "/target/units"),
    list(mutate(function(r) { r$infusate <- list(concentration = 10, units = "mg/ml"); r }), 422, "UNIT_UNKNOWN", "/infusate/units"),
    list(mutate(function(r) { r$history <- list(bolus(0, 1, "mg/min")); r }), 422, "UNIT_UNKNOWN", "/history/0/units"),
    list(mutate(function(r) { r$target$units <- "ng/mL"; r }), 422, "UNIT_MISMATCH", "/target/units"),
    list(mutate(function(r) { r$pump$rateUnits <- "mL/hr"; r }), 422, "INFUSATE_REQUIRED", "/infusate"),
    list(mutate(function(r) { r$history <- list(bolus(0, 1, "mL")); r }), 422, "INFUSATE_REQUIRED", "/infusate"),
    list(mutate(function(r) { r$pump$volumeInfused <- list(value = 1, units = "mL"); r }), 422, "INFUSATE_REQUIRED", "/infusate"),
    list(mutate(function(r) { r$pump$syringeVolumeRemaining <- list(value = 1, units = "mL"); r }), 422, "INFUSATE_REQUIRED", "/infusate"),
    list(mutate(function(r) {
      r$infusate <- list(concentration = 10, units = "mg/mL")
      r$history <- list(list(time = atMin(-5), infusate = list(concentration = 20, units = "mg/mL")))
      r }), 422, "INFUSATE_MISMATCH", "/infusate"),
    list(mutate(function(r) { r$now <- "2026-10-11T14:00:00"; r }), 422, "TIME_INVALID", "/now"),
    list(mutate(function(r) { r$now <- "2026-13-45T14:00:00Z"; r }), 422, "TIME_INVALID", "/now"),
    list(mutate(function(r) { r$now <- "1969-12-31T23:59:59Z"; r }), 422, "TIME_INVALID", "/now"),
    list(mutate(function(r) { r$now <- "1791729125"; r }), 422, "TIME_INVALID", "/now"),
    list(mutate(function(r) { r$history[[1]]$time <- "yesterday"; r }), 422, "TIME_INVALID", "/history/0/time"),
    # Offsets and clock fields outside their RFC 3339 ranges would otherwise parse and shift the clock.
    list(mutate(function(r) { r$now <- "2026-10-11T14:00:00+24:00"; r }), 422, "TIME_INVALID", "/now"),
    list(mutate(function(r) { r$now <- "2026-10-11T14:00:00-24:00"; r }), 422, "TIME_INVALID", "/now"),
    list(mutate(function(r) { r$now <- "2026-10-11T14:00:00+99:99"; r }), 422, "TIME_INVALID", "/now"),
    list(mutate(function(r) { r$now <- "2026-10-11T24:00:00Z"; r }), 422, "TIME_INVALID", "/now"),
    list(mutate(function(r) { r$now <- "2026-10-11T14:00:61Z"; r }), 422, "TIME_INVALID", "/now"),
    list(mutate(function(r) { r$history[[1]]$time <- "2026-10-11T13:60:00Z"; r }), 422, "TIME_INVALID", "/history/0/time"),
    list(mutate(function(r) { r$history <- list(rateChange(-1, 1), rateChange(-1, 2)); r }), 422, "HISTORY_INVALID", "/history/1"),
    list(mutate(function(r) {
      r$infusate <- list(concentration = 20, units = "mg/mL")
      r$history <- list(list(time = atMin(-5), infusate = list(concentration = 10, units = "mg/mL")),
                        list(time = atMin(-5), infusate = list(concentration = 20, units = "mg/mL")))
      r }), 422, "HISTORY_INVALID", "/history/1"),
    list(mutate(function(r) { r$history[[1]]$bolus <- 5; r }), 422, "HISTORY_INVALID", "/history/0"),
    list(mutate(function(r) { r$history <- list(list(time = atMin(0), source = "x")); r }), 422, "HISTORY_INVALID", "/history/0"),
    list(mutate(function(r) { r$history[[1]]$from <- atMin(0); r }), 422, "HISTORY_INVALID", "/history/0/from"),
    list(mutate(function(r) { r$history <- list(segment(-5, -2, 1), segment(-3, -1, 1)); r }), 422, "HISTORY_OVERLAP", "/history/1"),
    list(mutate(function(r) { r$history <- list(rateChange(1, 1)); r }), 422, "HISTORY_IN_FUTURE", "/history/0/time"),
    list(mutate(function(r) { r$timing$latency <- 10; r }), 422, "LATENCY_INVALID", "/timing/latency"),
    list(mutate(function(r) { r$target$value <- 21; r }), 422, "TARGET_OUT_OF_RANGE", "/target/value")
  )
  for (case in cases)
    expectApiError(case[[1]], case[[2]], case[[3]], case[[4]], drug = "propofol",
                   info = paste(case[[3]], case[[4]]))

  # The drug of the path.
  expectApiError(baseRequest(drug = NULL), 400, "MISSING_FIELD", "/drug", drug = NULL)
  expectApiError(baseRequest(drug = NULL), 400, "INVALID_TYPE", "/drug", drug = 5)
  expectApiError(baseRequest(drug = "nope"), 404, "UNKNOWN_DRUG", "/drug")
  expectApiError(baseRequest(drug = "Propofol"), 404, "UNKNOWN_DRUG", "/drug")
  expectApiError(baseRequest(drug = "rocuronium"), 422, "DRUG_NOT_TCI", "/drug")
  # Not an object at all.
  e <- tryCatch(tciNextRate(list(1, 2)), tci_api_error = function(e) e)
  expect_equal(e$code, "INVALID_TYPE")
  expect_equal(e$pointer, "")
  # Structure before values: a duplicate key wins over a missing field.
  expectApiError(json('{"drug":"propofol","drug":"propofol"}'), 400, "DUPLICATE_KEY", "/drug")
  # The messages name the nearest token.
  expect_match(apiError(mutate(function(r) { r$pump$rateUnits <- "mL/h"; r }))$message, "mL/hr")
  expect_match(apiError(mutate(function(r) { r$history[[1]]$units <- "\u00b5g/min"; r }))$message, "mcg/min")
  expect_match(apiError(mutate(function(r) { r$target$units <- "ng/mL"; r }))$message, "mcg/mL for propofol")
})

test_that("the request id is echoed on an error and the error body has the section 6.3 shape", {
  req <- baseRequest(requestId = "ch1-000412")
  req$patient$weight <- "70"
  e <- apiError(req)
  expect_equal(e$requestId, "ch1-000412")
  body <- tciApiErrorBody(e, receivedAt = START, respondedAt = START + 0.01)
  expect_equal(names(body), c("apiVersion", "requestId", "errors", "server"))
  expect_equal(body$apiVersion, TCI_API_VERSION)
  expect_equal(body$requestId, "ch1-000412")
  expect_equal(body$errors[[1]], list(code = "INVALID_TYPE", message = e$message,
                                      pointer = "/patient/weight"))
  expect_equal(body$server$package, "stanpumpR")
  expect_null(body$server$computeSeconds)
  txt <- jsonlite::toJSON(body, auto_unbox = TRUE, digits = NA, null = "null")
  expect_match(txt, '"errors":\\[\\{"code":"INVALID_TYPE"')
  # No usable id: null.
  e2 <- apiError(baseRequest(requestId = NULL, now = 5))
  expect_null(e2$requestId)
  expect_match(jsonlite::toJSON(tciApiErrorBody(e2), auto_unbox = TRUE, null = "null"), '"requestId":null')
})

test_that("every warning code appears when it should, and never otherwise", {
  clean <- api(baseRequest(history = list(rateChange(-1, 10))))
  expect_equal(clean$warnings, list())
  codes <- function(f) { req <- baseRequest(history = list(rateChange(-1, 10))); warningCodes(api(f(req))) }
  expect_equal(codes(function(r) { r$target$value <- 5; r }), "TARGET_HIGH")
  expect_equal(codes(function(r) { r$timing$latency <- 6; r }), "LATENCY_LONG")
  expect_equal(codes(function(r) { r$timing$interval <- 60; r }), "INTERVAL_LONG")
  expect_equal(codes(function(r) { r$timing$interval <- 30; r }), character(0))   # propofol: tPeak / 3 = 32 s
  expect_equal(codes(function(r) { r$pump$state <- "alarm"; r }), "PUMP_NOT_RUNNING")
  expect_equal(codes(function(r) { r$pump$currentRate <- 0; r }), "RATE_MISMATCH")
  expect_equal(codes(function(r) { r$pump$volumeInfused <- list(value = 100, units = "mg"); r }), "DELIVERY_MISMATCH")
  expect_equal(codes(function(r) { r$history[[1]]$time <- atMin(1 / 60); r }), "HISTORY_CLOCK_SKEW")
  expect_equal(codes(function(r) {
    r$pump$rateUnits <- "mL/hr"; r$pump$maxRate <- 1200
    r$infusate <- list(concentration = 10, units = "mg/mL")
    r$pump$syringeVolumeRemaining <- list(value = 0.5, units = "mL")
    r$history[[1]]$units <- "mL/hr"
    r }), "SYRINGE_LOW")
  skewed <- tciNextRate(baseRequest(), receivedAt = START + 31)
  expect_equal(warningCodes(skewed), "CLOCK_SKEW")
  expect_equal(skewed$warnings[[1]]$pointer, "/now")
  expect_equal(warningCodes(tciNextRate(baseRequest(), receivedAt = START + 29)), character(0))
  # Several at once, each with its pointer.
  many <- api(local({
    r <- baseRequest(history = list(rateChange(-1, 10)), latency = 6)
    r$target$value <- 5
    r$pump$state <- "paused"
    r
  }))
  expect_setequal(warningCodes(many), c("TARGET_HIGH", "LATENCY_LONG", "PUMP_NOT_RUNNING"))
  expect_equal(vapply(many$warnings, `[[`, "", "pointer"),
               c("/target/value", "/timing/latency", "/pump/state"))
})

test_that("the syringe concentration has a floor, in the request and in the history alike", {
  # A concentration of 1e-305 is finite and positive, and would turn every
  # millilitre into a rate the arithmetic cannot carry.  TCI_API_MIN_INFUSATE
  # is a thousandth of a unit per mL, far below any syringe a pump is loaded
  # with; the floor is the same for the request's syringe and for a syringe
  # change in the history, and is published under limits.
  expect_equal(TCI_API_MIN_INFUSATE, 1e-3)
  syringe <- function(c) list(concentration = c, units = "mg/mL")
  req <- baseRequest(rateUnits = "mL/hr", maxRate = 1200)
  req$infusate <- syringe(1e-305)
  expectApiError(req, 422, "INVALID_VALUE", "/infusate/concentration")
  req$infusate <- syringe(0)
  expectApiError(req, 422, "INVALID_VALUE", "/infusate/concentration")
  req$infusate <- syringe(TCI_API_MIN_INFUSATE)
  expect_equal(api(req)$rate$value, 1200)                       # accepted at the floor; 0.02 mg/min, capped
  hist <- baseRequest(history = list(list(time = atMin(-1), infusate = syringe(1e-4))))
  expectApiError(hist, 422, "INVALID_VALUE", "/history/0/infusate/concentration")
  hist <- baseRequest(history = list(rateChange(-1, 10), list(time = atMin(-0.5), infusate = syringe(0.0001))))
  expectApiError(hist, 422, "INVALID_VALUE", "/history/1/infusate/concentration")
  expect_equal(tciApiDrugs()$limits$minInfusate, TCI_API_MIN_INFUSATE)
  spec <- jsonlite::fromJSON(system.file("api", "openapi.json", package = "stanpumpR"), simplifyVector = FALSE)
  expect_equal(spec$components$schemas$Infusate$properties$concentration$minimum, TCI_API_MIN_INFUSATE)
  expect_true("minInfusate" %in% unlist(spec$components$schemas$Drugs$properties$limits$required))
})

test_that("a response that would carry a number that is not finite is refused as 500 INTERNAL", {
  # The belt under the validation's braces: nothing a pump acts on may be NaN
  # or infinite.  The checker walks the lists a pump reads; strings, logicals
  # and nulls are not numbers, and a sound response passes through unchanged.
  e <- tryCatch(tciApiCheckFinite(list(rate = list(value = NaN, units = "mL/hr"))), tci_api_error = function(e) e)
  expect_s3_class(e, "tci_api_error")
  expect_equal(c(e$status, e$code, e$pointer), c("500", "INTERNAL", "/rate/value"))
  e <- tryCatch(tciApiCheckFinite(list(state = list(now = list(cp = 3, p = c(1, Inf, 2))))), tci_api_error = function(e) e)
  expect_equal(e$pointer, "/state/now/p")
  e <- tryCatch(tciApiCheckFinite(list(prediction = list(lookAhead = list(decrementSeconds = -Inf)))), tci_api_error = function(e) e)
  expect_equal(e$pointer, "/prediction/lookAhead/decrementSeconds")
  r <- api(baseRequest())
  acted <- r[c("rate", "segment", "fallback", "state", "prediction", "delivered")]
  expect_identical(tciApiCheckFinite(acted), acted)
  expect_silent(tciApiCheckFinite(list(a = "x", b = TRUE, c = NULL, d = I(character(0)), e = list())))
  # Through tciNextRate(): a prediction helper that misbehaves is caught
  # before the response leaves, with the request id on the error.
  local_mocked_bindings(tciApiPeak = function(S, K) list(ce = NaN, tau = 0))
  e <- apiError(baseRequest(requestId = "ch1-000009"))
  expect_equal(c(e$status, e$code, e$pointer, e$requestId),
               c("500", "INTERNAL", "/prediction/peakCe", "ch1-000009"))
})

# ---------------------------------------------------------------------------
# 9. Every TCI drug

test_that("every TCI drug reaches its typical target without overshoot and holds it", {
  for (drug in TCI_DRUGS) {
    PK <- pkOf(drug)
    loop <- simulatePump(drug = drug, target = PK$typical, latency = 1,
                         rateUnits = paste0(massUnitOf(PK), "/min"),
                         minutes = 2 * PK$tPeak + 2, follow = "nextRequestAt")
    expect_lte(loop$maxCe, PK$typical * 1.002, label = paste(drug, "max Ce"))
    late <- loop$ce[loop$ceTime > 2 * PK$tPeak]
    expect_lt(max(abs(late / PK$typical - 1)), 0.005, label = paste(drug, "hold"))
    first <- loop$responses[[1]]
    expect_equal(first$model$drug, drug)
    expect_equal(first$model$massUnits, massUnitOf(PK))
    expect_equal(first$state$units, concUnitOf(PK))
    expect_equal(first$rate$massUnits, massUnitOf(PK))
    expect_equal(first$controller$phase, "loading")
    expect_equal(first$controller$plan, "effectSite")
  }
})

test_that("a rate that is a rounding residual of zero is exactly zero, down to the smallest patient", {
  # At MIN_WEIGHT the coefficients are large, and the effect-site search left
  # remifentanil's first fallback at 1.5e-13 mcg/min under a snap tolerance of
  # a billionth of the target, with phase "loading" read from it.  The snap is
  # a millionth: a maintenance rate holds the target itself, so nothing the
  # controller means lies in (0, 1e-6) of any unit, for any TCI drug.
  for (drug in TCI_DRUGS) {
    PK <- pkOf(drug, 0.1, 50, 0)
    loop <- simulatePump(drug = drug, target = PK$typical, weight = 0.1, height = 50, age = 0,
                         rateUnits = paste0(massUnitOf(PK), "/min"), minutes = PK$tPeak + 1)
    commanded <- loop$rates
    fallback <- vapply(loop$responses, function(r) r$fallback$rate, 0)
    expect_false(any(commanded > 0 & commanded < 1e-6), label = paste(drug, "commanded rates"))
    expect_false(any(fallback > 0 & fallback < 1e-6), label = paste(drug, "fallback rates"))
    first <- loop$responses[[1]]
    expect_identical(first$fallback$rate, 0)
    expect_gt(first$rate$value, 1e-6)
    expect_equal(first$controller$phase, "loading")
    expect_equal(first$model$parameters$v1, PK$PK[[1]]$v1)
    # and the loop still reaches its target
    expect_lte(loop$maxCe, PK$typical * 1.002, label = paste(drug, "max Ce at 0.1 kg"))
  }
})

test_that("a plasma target is held exactly from the end of the first interval", {
  loop <- simulatePump(site = "plasma", minutes = 5)
  cp <- vapply(loop$responses, function(r) r$state$now$cp, 0)
  expect_equal(cp[-(1:2)], rep(3, length(cp) - 2), tolerance = 1e-6)
  expect_true(all(vapply(loop$responses, function(r) r$controller$plan, "") == "plasma"))
  expect_lte(loop$maxCe, 3 * 1.001)
  expect_equal(loop$responses[[2]]$controller$phase, "maintenance")
  expect_equal(loop$responses[[1]]$controller$phase, "loading")
})

test_that("a target of 0 stops the infusion", {
  r <- api(baseRequest(target = 0, now = atMin(10), history = list(rateChange(0, 10))))
  expect_equal(r$rate$value, 0)
  expect_equal(r$controller, list(plan = "off", phase = "stopped"))
  expect_equal(r$fallback$rate, 0)
  expect_false(r$prediction$onTarget)
  expect_null(r$prediction$deviation)
  expect_null(r$prediction$targetReachedAt)
  expect_equal(r$target$value, 0)
})

test_that("tciApiDrugs() lists exactly the TCI drugs with their units, ranges and limits", {
  d <- tciApiDrugs()
  expect_equal(d$apiVersion, TCI_API_VERSION)
  expect_setequal(vapply(d$drugs, `[[`, "", "drug"), TCI_DRUGS)
  expect_equal(tciApiDrugNames(), vapply(d$drugs, `[[`, "", "drug"))
  propofol <- d$drugs[[which(vapply(d$drugs, `[[`, "", "drug") == "propofol")]]
  expect_equal(propofol$concentrationUnits, "mcg/mL")
  expect_equal(propofol$massUnits, "mg")
  expect_equal(propofol$typicalTarget, 3)
  expect_equal(propofol$typicalRange, list(lower = 2.5, upper = 4))
  expect_equal(propofol$targetCeiling, 20)
  expect_equal(propofol$tPeak, pkOf("propofol")$tPeak)
  expect_match(propofol$reference, "Eleveld")
  expect_equal(propofol$sites, I(c("effect", "plasma")))
  remi <- d$drugs[[which(vapply(d$drugs, `[[`, "", "drug") == "remifentanil")]]
  expect_equal(remi$concentrationUnits, "ng/mL")
  expect_equal(remi$massUnits, "mcg")
  expect_equal(d$units$rate, I(TCI_API_RATE_UNITS))
  expect_equal(d$limits$maxHistory, TCI_API_MAX_HISTORY)
  expect_equal(d$limits$weight, list(min = MIN_WEIGHT, max = MAX_WEIGHT))
  expect_equal(d$limits$pumpStates, I(TCI_API_PUMP_STATES))
  txt <- jsonlite::toJSON(d, auto_unbox = TRUE, digits = NA, null = "null")
  expect_match(txt, '"volume":\\["mL"\\]')
  expect_match(txt, '"drugs":\\[\\{"drug":"propofol"')
})

test_that("tciApiModel() is the response's model block and takes query-string values", {
  r <- api(baseRequest())
  m <- tciApiModel("propofol", 70, 170, 50, "male")
  expect_equal(m, r$model)
  expect_equal(tciApiModel("propofol", "70", "170", "50", "male", "true"), r$model)
  expect_match(m$hash, "^md5:[0-9a-f]{32}$")
  expect_equal(names(m$parameters), c("v1", "v2", "v3", "cl1", "cl2", "cl3", "k10", "k12", "k13",
                                      "k21", "k31", "lambda_1", "lambda_2", "lambda_3", "ke0"))
  expect_equal(m$parameters$ke0, pkOf("propofol")$PK[[1]]$ke0)
  expect_equal(m$tPeak, 1.6)
  expect_true(m$adjustToFFM)
  # Another patient, another hash; the switch is echoed.
  m2 <- tciApiModel("fentanyl", 100, 180, 30, "female", FALSE)
  expect_false(m2$adjustToFFM)
  expect_false(identical(m2$hash, tciApiModel("fentanyl", 100, 180, 30, "female", TRUE)$hash))
  expect_equal(m2$concentrationUnits, "ng/mL")
  e <- tryCatch(tciApiModel("nope", 70, 170, 50, "male"), tci_api_error = function(e) e)
  expect_equal(c(e$status, e$code, e$pointer), c("404", "UNKNOWN_DRUG", "/drug"))
  e <- tryCatch(tciApiModel("propofol", "heavy", 170, 50, "male"), tci_api_error = function(e) e)
  expect_equal(c(e$status, e$code, e$pointer), c("400", "INVALID_TYPE", "/weight"))
  e <- tryCatch(tciApiModel("propofol", 700, 170, 50, "male"), tci_api_error = function(e) e)
  expect_equal(c(e$status, e$code, e$pointer), c("422", "INVALID_VALUE", "/weight"))
  e <- tryCatch(tciApiModel("propofol", 70, 170, 50, "M"), tci_api_error = function(e) e)
  expect_equal(c(e$code, e$pointer), c("INVALID_VALUE", "/sex"))
  # Strings are compared exactly: the query takes true and false, not TRUE.
  e <- tryCatch(tciApiModel("propofol", 70, 170, 50, "male", "TRUE"), tci_api_error = function(e) e)
  expect_equal(c(e$status, e$code, e$pointer), c("400", "INVALID_TYPE", "/adjustToFFM"))
  expect_false(tciApiModel("propofol", 70, 170, 50, "male", "false")$adjustToFFM)
})

test_that("the PK cache is bounded and a hit returns the same object", {
  # A controller asks every few seconds for the same patient, so the PK is
  # kept; a client varying its covariates on every request must not grow the
  # server without limit (about 11 KB per entry).  When the cache is full the
  # entry inserted first goes.
  expect_equal(TCI_API_PK_CACHE_SIZE, 256L)
  # Cleared before and after, explicitly: withr is not a declared dependency.
  tciApiPKCacheClear()
  on.exit(tciApiPKCacheClear(), add = TRUE)
  expect_length(tciApiPKCacheKeys(), 0)
  first <- tciApiPK("propofol", 70, 170, 50, "male", TRUE)
  expect_identical(tciApiPK("propofol", 70, 170, 50, "male", TRUE), first)
  expect_equal(tciApiPKCacheKeys(), "propofol|70|170|50|male|TRUE")
  expect_equal(first$PK[[1]]$v1, pkOf("propofol")$PK[[1]]$v1)
  for (wt in 1:300) tciApiPK("propofol", wt, 170, 50, "male", TRUE)
  keys <- tciApiPKCacheKeys()
  expect_lte(length(keys), TCI_API_PK_CACHE_SIZE)
  expect_length(keys, 256L)
  # The oldest went first: the first entry (70 kg, a hit in the loop, so not
  # re-inserted) and the lightest weights; the newest are all there.
  expect_false("propofol|70|170|50|male|TRUE" %in% keys)
  expect_false("propofol|1|170|50|male|TRUE" %in% keys)
  expect_true(all(paste0("propofol|", 200:300, "|170|50|male|TRUE") %in% keys))
  expect_equal(keys[length(keys)], "propofol|300|170|50|male|TRUE")
  a <- tciApiPK("propofol", 300, 170, 50, "male", TRUE)
  expect_identical(tciApiPK("propofol", 300, 170, 50, "male", TRUE), a)
  expect_equal(a, getDrugPK("propofol", 300, 170, 50, "male", getDrugDefaults("propofol"), adjustToFFM = TRUE))
  expect_length(tciApiPKCacheKeys(), 256L)
  # Another sex, switch or drug is another entry; the hit is by every covariate.
  tciApiPK("propofol", 300, 170, 50, "female", TRUE)
  tciApiPK("propofol", 300, 170, 50, "male", FALSE)
  tciApiPK("remifentanil", 300, 170, 50, "male", TRUE)
  expect_length(tciApiPKCacheKeys(), 256L)
  expect_true(all(c("propofol|300|170|50|female|TRUE", "propofol|300|170|50|male|FALSE",
                    "remifentanil|300|170|50|male|TRUE") %in% tciApiPKCacheKeys()))
  # The response does not depend on whether the PK came from the cache.
  tciApiPKCacheClear()
  cold <- api(baseRequest())
  warm <- api(baseRequest())
  cold$server <- warm$server <- NULL
  expect_identical(cold, warm)
})

test_that("tciApiHealth() reports the version, the clock and the number of TCI drugs", {
  h <- tciApiHealth(now = as.POSIXct(START, origin = "1970-01-01", tz = "UTC"))
  expect_equal(h$apiVersion, TCI_API_VERSION)
  expect_equal(h$status, "ok")
  expect_equal(h$package, "stanpumpR")
  expect_equal(h$version, as.character(utils::packageVersion("stanpumpR")))
  expect_equal(h$serverTime, "2026-10-11T14:00:00.000Z")
  expect_equal(h$tciDrugs, length(TCI_DRUGS))
  expect_true("commit" %in% names(h))
})

# ---------------------------------------------------------------------------
# 10. Predictions

test_that("peakAt and peakCe are the touching point when the pump is off after the hold", {
  # Only the first response reaches the pump; the fallback (0) then runs and
  # the pump stops, so the true trajectory is the one the prediction assumed.
  loop <- simulatePump(minutes = 4, accept = function(i, t) i == 1)
  r <- loop$responses[[1]]
  expect_equal(minutesOf(r$prediction$peakAt), loop$maxCeAt, tolerance = 1e-9)
  expect_equal(r$prediction$peakCe, loop$maxCe, tolerance = 1e-9)
  expect_equal(r$prediction$peakCe, 3, tolerance = 1e-6)
  expect_equal(r$prediction$units, "mcg/mL")
  expect_false(r$prediction$onTarget)
  expect_lt(r$prediction$deviation, -0.5)
  # The pump stopped at 10 s (the fallback) and the history says so.
  expect_length(loop$history, 2)
  expect_equal(loop$history[[2]]$rate, 0)
  # Asked within an interval of the peak, the response says: ask again at the
  # peak, which is later than the hold's end.
  r2 <- api(baseRequest(now = atMin(1.5), history = loop$history))
  expect_equal(r2$rate$value, 0)
  expect_equal(r2$controller$phase, "waitingForPeak")
  expect_equal(r2$timing$nextRequestAt, r2$prediction$peakAt)
  expect_equal(r2$prediction$peakAt, r$prediction$peakAt)
  expect_gt(minutesOf(r2$timing$nextRequestAt), minutesOf(r2$timing$holdUntil))
  # Asked further from the peak, it is the hold's end as usual.
  r2b <- api(baseRequest(now = atMin(0.5), history = loop$history))
  expect_equal(r2b$timing$nextRequestAt, r2b$timing$holdUntil)
  expect_equal(r2b$controller$phase, "waitingForPeak")
  # With a latency the ask is brought forward by it.
  r3 <- api(baseRequest(now = atMin(1.5), history = loop$history, latency = 2))
  expect_equal(minutesOf(r3$prediction$peakAt) - minutesOf(r3$timing$nextRequestAt), 2 / 60, tolerance = 1e-9)
})

test_that("targetReachedAt is the hold's end when on target, later for a capped loading", {
  loop <- simulatePump(minutes = 5)
  maint <- loop$responses[[length(loop$responses)]]
  expect_true(maint$prediction$onTarget)
  expect_lt(abs(maint$prediction$deviation), 0.01)
  expect_equal(maint$prediction$targetReachedAt, maint$timing$holdUntil)
  free <- loop$responses[[1]]
  capped <- api(local({
    r <- baseRequest(rateUnits = "mL/hr", maxRate = 300)
    r$infusate <- list(concentration = 10, units = "mg/mL")
    r
  }))
  expect_gt(minutesOf(capped$prediction$targetReachedAt), minutesOf(free$prediction$targetReachedAt))
  expect_equal(minutesOf(free$prediction$targetReachedAt) * 60, 90)
  expect_equal(minutesOf(capped$prediction$targetReachedAt) * 60, 120)
  expect_equal(capped$rate$limitedBy, I("maxRate"))
  # Far above the target with a slow decay: null within the horizon.
  high <- api(baseRequest(target = 0.001, now = atMin(1), history = list(bolus(0, 500))))
  expect_null(high$prediction$targetReachedAt)
  expect_equal(high$controller$phase, "aboveTarget")
})

test_that("targetReachedAt equals a plan-per-interval replay of the controller", {
  # The server takes shortcuts (a closed-form bound for a ceiling that cannot
  # reach the target, the pump-off trajectory while the controller is off by
  # choice); the time must be the one a plan at every interval would give.
  replay <- function(req, hours = 1) {
    v <- tciApiValidate(req, req$drug, tciApiWarnings())
    K <- tciKinetics(tciApiPK(v$drug, 70, 170, 50, "male", TRUE))
    H <- tciApiHistory(v$history, v$units$mass, 70, v$infusate)
    conv <- list(base = v$units$mass, weight = 70, concBase = H$concBase,
                 rateFactor = tciApiUnitFactor(v$pump$rateUnits, v$units$mass, 70, H$concBase))
    I <- v$timing$interval; dt <- I / 60; CT <- v$target$value; site <- v$target$site
    S <- tciAdvance(tciStatesFromHistory(H, K), v$timing$latency / 60, H$runningRate, K)
    S <- tciAdvance(S, dt, tciApiPlan(S, CT, site, I, K, v$pump, conv)$base, K)   # at holdUntil
    controlled <- function(S) if (site == "effect") sum(S$e) else sum(S$p)
    for (k in 0:(hours * 3600 / I)) {
      if (abs(controlled(S) - CT) <= TCI_API_ON_TARGET * CT)
        return(minutesOf(req$now) + (v$timing$latency + I + k * I) / 60)
      S <- tciAdvance(S, dt, tciApiPlan(S, CT, site, I, K, v$pump, conv)$base, K)
    }
    NULL
  }
  cases <- list(
    baseRequest(interval = 10), baseRequest(interval = 5), baseRequest(interval = 30, latency = 2),
    baseRequest(interval = 10, maxRate = 50), baseRequest(interval = 1, maxRate = 50),
    baseRequest(interval = 10, maxRate = 8),                               # just above the maintenance need
    baseRequest(interval = 10, now = atMin(1), history = list(bolus(0, 20))),   # above target, decaying
    baseRequest(interval = 10, now = atMin(1), history = list(bolus(0, 20)), site = "plasma"),
    baseRequest(interval = 10, site = "plasma", maxRate = 50),
    baseRequest(drug = "remifentanil", target = 4, units = "ng/mL", rateUnits = "mcg/min", interval = 5, maxRate = 100)
  )
  for (i in seq_along(cases)) {
    req <- cases[[i]]
    expected <- replay(req)
    got <- api(req)$prediction$targetReachedAt
    if (is.null(expected)) expect_null(got, label = sprintf("case %d", i))
    else expect_equal(minutesOf(got), expected, tolerance = 1e-9, label = sprintf("case %d", i))
  }
  # A ceiling that cannot reach the target is decided at once, and agrees
  # with the replay over the first hour.
  low <- local({ r <- baseRequest(interval = 10, rateUnits = "mL/hr", maxRate = 20); r$infusate <- list(concentration = 10, units = "mg/mL"); r })
  expect_null(api(low)$prediction$targetReachedAt)
  expect_null(replay(low))
})

test_that("lookAhead is the closed-form decay from now with the pump off", {
  loop <- simulatePump(minutes = 5)
  K <- loop$K
  for (level in c(1, 2.5)) {
    r <- api(local({
      req <- baseRequest(now = atMin(5), history = loop$history)
      req$lookAhead <- list(concentration = level)
      req
    }))
    la <- r$prediction$lookAhead
    expect_equal(la$site, "effect")
    expect_equal(la$concentration, level)
    expect_equal(la$units, "mcg/mL")
    expect_equal(la$horizonSeconds, TCI_API_LOOKAHEAD_MAX)
    expect_false(la$beyondHorizon)
    tau <- la$decrementSeconds / 60
    S <- list(e = r$state$now$e)
    ce <- function(t) sum(S$e * exp(-K$lambda4 * t))
    expect_equal(ce(tau), level, tolerance = 1e-6)
    expect_gt(ce(tau - 1 / 60), level)
    expect_lt(ce(tau + 1 / 60), level)
  }
  # Already below: 0.  Never reached: null and beyond the horizon.
  below <- api(local({ req <- baseRequest(now = atMin(5), history = loop$history); req$lookAhead <- list(concentration = 10); req }))
  expect_equal(below$prediction$lookAhead$decrementSeconds, 0)
  never <- api(local({ req <- baseRequest(now = atMin(5), history = loop$history); req$lookAhead <- list(concentration = 1e-12); req }))
  expect_null(never$prediction$lookAhead$decrementSeconds)
  expect_true(never$prediction$lookAhead$beyondHorizon)
  # A plasma target looks at the plasma, and not asking for it gives nothing.
  plasma <- api(local({ req <- baseRequest(now = atMin(5), site = "plasma", history = loop$history); req$lookAhead <- list(concentration = 1); req }))
  expect_equal(plasma$prediction$lookAhead$site, "plasma")
  expect_equal(sum(plasma$state$now$p * exp(-K$lambda3 * plasma$prediction$lookAhead$decrementSeconds / 60)), 1, tolerance = 1e-6)
  expect_false("lookAhead" %in% names(api(baseRequest(now = atMin(5), history = loop$history))$prediction))
})

test_that("syringeEmptyAt follows from the remaining volume and the commanded rate", {
  syringe <- list(concentration = 10, units = "mg/mL")
  req <- baseRequest(rateUnits = "mL/hr", maxRate = 1200, latency = 2)
  req$infusate <- syringe
  req$pump$syringeVolumeRemaining <- list(value = 50, units = "mL")
  r <- api(req)
  expect_equal(r$rate$value, 1200)
  hours <- 50 / 1200
  expect_equal(minutesOf(r$prediction$syringeEmptyAt), minutesOf(r$timing$effectiveAt) + hours * 60, tolerance = 1e-6)
  expect_equal(warningCodes(r), character(0))
  # Too little for the loading segment and its fallback: a warning.
  req$pump$syringeVolumeRemaining <- list(value = 2, units = "mL")
  low <- api(req)
  expect_equal(warningCodes(low), "SYRINGE_LOW")
  expect_gt(low$segment$volumeMl + low$fallback$rate / 60 * 10 / 60, 2)
  # The old rate empties it during the latency.
  req2 <- baseRequest(rateUnits = "mL/hr", maxRate = 1200, latency = 5, now = atMin(1),
                      history = list(rateChange(0, 1200, "mL/hr")))
  req2$infusate <- syringe
  req2$pump$syringeVolumeRemaining <- list(value = 1, units = "mL")
  r2 <- api(req2)
  expect_equal(minutesOf(r2$prediction$syringeEmptyAt) * 60, 60 + 1 / 1200 * 3600, tolerance = 1e-6)
  expect_equal(r2$rate$value, 0)                      # far above target after 200 mg
  expect_equal(warningCodes(r2), character(0))        # nothing planned, nothing low
  # Rate 0: null.
  req3 <- baseRequest(rateUnits = "mL/hr", maxRate = 1200, target = 0)
  req3$infusate <- syringe
  req3$pump$syringeVolumeRemaining <- list(value = 50, units = "mL")
  expect_null(api(req3)$prediction$syringeEmptyAt)
  expect_true("syringeEmptyAt" %in% names(api(req3)$prediction))
  expect_false("syringeEmptyAt" %in% names(api(baseRequest())$prediction))
})

# ---------------------------------------------------------------------------
# 11. Determinism and serialization

test_that("two calls with the same body give identical bodies apart from server.*", {
  loop <- simulatePump(minutes = 3, latency = 1.5)
  req <- baseRequest(now = atMin(3), history = loop$history, latency = 1.5)
  req$lookAhead <- list(concentration = 1)
  now <- START + 3 * 60
  a <- tciNextRate(req, receivedAt = now)
  Sys.sleep(0.01)
  b <- tciNextRate(req, receivedAt = now + 5)
  expect_false(identical(a$server, b$server))
  a$server <- NULL
  b$server <- NULL
  expect_identical(a, b)
  expect_identical(jsonlite::toJSON(a, auto_unbox = TRUE, digits = NA, null = "null"),
                   jsonlite::toJSON(b, auto_unbox = TRUE, digits = NA, null = "null"))
  # The server's clock enters nothing but server.* and the CLOCK_SKEW warning.
  c <- tciNextRate(req, receivedAt = now + 3600)
  expect_equal(warningCodes(c), "CLOCK_SKEW")
  c$server <- NULL
  c$warnings <- list()
  expect_identical(c, a)
})

test_that("the response serializes to the section 6 shape at full precision", {
  loop <- simulatePump(minutes = 3, rateUnits = "mL/hr", maxRate = 1200,
                       infusate = list(concentration = 10, units = "mg/mL"))
  req <- baseRequest(requestId = "ch1-000412", rateUnits = "mL/hr", maxRate = 1200, latency = 1.5,
                     now = atMin(3), history = loop$history)
  req$infusate <- list(concentration = 10, units = "mg/mL")
  req$pump$rateResolution <- 0.1
  req$pump$volumeInfused <- list(value = 0, units = "mL")
  r <- api(req)
  expect_equal(r$controller$phase, "maintenance")
  expect_equal(names(r), c("apiVersion", "requestId", "drug", "target", "controller", "rate",
                           "segment", "fallback", "timing", "state", "prediction", "delivered",
                           "model", "algorithm", "warnings", "server"))
  txt <- jsonlite::toJSON(r, auto_unbox = TRUE, digits = NA, null = "null")
  back <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  expect_equal(back$apiVersion, "1.0")
  expect_equal(back$requestId, "ch1-000412")
  expect_equal(back$timing$now, "2026-10-11T14:03:00.000Z")
  expect_equal(back$timing$effectiveAt, "2026-10-11T14:03:01.500Z")
  expect_equal(back$timing$holdUntil, "2026-10-11T14:03:11.500Z")
  expect_equal(back$timing$nextRequestAt, "2026-10-11T14:03:10.000Z")
  expect_equal(warningCodes(r), "DELIVERY_MISMATCH")   # a totaliser of 0 after 3 min
  r$warnings <- list()
  txt <- jsonlite::toJSON(r, auto_unbox = TRUE, digits = NA, null = "null")
  back <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  expect_true(is.list(back$warnings) && length(back$warnings) == 0)
  expect_match(txt, '"warnings":\\[\\]')
  expect_match(txt, '"limitedBy":\\["rateResolution"\\]')
  expect_match(txt, '"commit":null')
  expect_true(is.list(back$state$now$p) && length(back$state$now$p) == 3)
  expect_true(is.list(back$state$now$e) && length(back$state$now$e) == 4)
  expect_equal(back$delivered$reported, 0)                     # a totaliser of 0 mL
  expect_match(jsonlite::toJSON(api(baseRequest()), auto_unbox = TRUE, digits = NA, null = "null"),
               '"reported":null')                              # none given
  expect_equal(back$algorithm, list(name = "shafer-gregg-1992", plasmaSwitch = 0.05, tauGridSeconds = 1))
  expect_equal(back$rate$units, "mL/hr")
  expect_equal(back$fallback$units, "mL/hr")
  expect_equal(back$segment$amountUnits, "mg")
  expect_equal(back$state$units, "mcg/mL")
  # Full double precision survives the round trip.
  expect_equal(back$rate$ideal, r$rate$ideal, tolerance = 1e-14)
  expect_equal(back$state$holdUntil$cp, r$state$holdUntil$cp, tolerance = 1e-14)
  expect_equal(back$model$parameters$ke0, r$model$parameters$ke0, tolerance = 1e-14)
  expect_match(txt, '"tPeak":1.6[,}]')
})

test_that("the documented example request produces the documented response", {
  # docs/tci-api.md sections 4 and 6.1, repeated as the RateRequest and
  # RateResponse examples of inst/api/openapi.json.  The body is a real
  # record: a closed loop of these requests every 10 s (the first rate
  # acknowledged at 14:00:00.000Z), 1% propofol to 3 mcg/mL effect site, a
  # 20 mg hand bolus 20 s before the pump started, the initial syringe, the
  # loading rate and the zero that followed it recorded as they happened and
  # the maintenance delivery merged into five segments that shorten toward
  # now.  The documented response rounds the numbers to four significant
  # digits.
  body <- list(
    requestId = "ch1-000193", drug = "propofol", now = "2026-10-11T14:31:58.500Z",
    patient = list(weight = 70, height = 170, age = 50, sex = "male"),
    infusate = list(concentration = 10, units = "mg/mL"),
    pump = list(rateUnits = "mL/hr", maxRate = 1200, minRate = 0.1, rateResolution = 0.1,
                currentRate = 40.4, state = "running",
                volumeInfused = list(value = 26.95, units = "mL"),
                syringeVolumeRemaining = list(value = 28.5, units = "mL")),
    target = list(site = "effect", value = 3, units = "mcg/mL"),
    timing = list(latency = 1.5, interval = 10),
    history = list(
      list(time = "2026-10-11T13:59:30.000Z", infusate = list(concentration = 10, units = "mg/mL")),
      list(time = "2026-10-11T13:59:40.000Z", bolus = 20, units = "mg", source = "manual"),
      list(time = "2026-10-11T14:00:00.000Z", rate = 995.1, units = "mL/hr"),
      list(time = "2026-10-11T14:00:10.000Z", rate = 0, units = "mL/hr"),
      list(from = "2026-10-11T14:01:30.000Z", to = "2026-10-11T14:10:00.000Z", delivered = 7.9, units = "mL"),
      list(from = "2026-10-11T14:10:00.000Z", to = "2026-10-11T14:20:00.000Z", delivered = 7.88, units = "mL"),
      list(from = "2026-10-11T14:20:00.000Z", to = "2026-10-11T14:25:00.000Z", delivered = 3.6, units = "mL"),
      list(from = "2026-10-11T14:25:00.000Z", to = "2026-10-11T14:30:00.000Z", delivered = 3.46, units = "mL"),
      list(from = "2026-10-11T14:30:00.000Z", to = "2026-10-11T14:31:50.000Z", delivered = 1.24, units = "mL"),
      list(time = "2026-10-11T14:31:50.000Z", rate = 40.4, units = "mL/hr")
    ),
    lookAhead = list(concentration = 1),
    options = list(adjustToFFM = TRUE)
  )
  r <- tciNextRate(body, receivedAt = tciApiParseTime("2026-10-11T14:31:58.662Z", "/now"))
  expect_equal(r$controller$plan, "plasma")
  expect_equal(r$controller$phase, "maintenance")
  expect_lt(abs(r$rate$value - 39.2), 0.05)
  expect_lt(abs(r$state$now$cp - 3.005), 1e-3)
  expect_lt(abs(r$state$now$ce - 3.007), 1e-3)
  expect_equal(r$warnings, list())
  expect_equal(r$rate$limitedBy, I("rateResolution"))
  expect_equal(r$fallback$rate, 40.2)
  expect_equal(r$timing$effectiveAt, "2026-10-11T14:32:00.000Z")
  expect_equal(r$timing$holdUntil, "2026-10-11T14:32:10.000Z")
  expect_equal(r$timing$nextRequestAt, "2026-10-11T14:32:08.500Z")
  expect_true(r$prediction$onTarget)
  expect_equal(r$prediction$peakAt, "2026-10-11T14:32:10.000Z")
  expect_equal(r$prediction$targetReachedAt, "2026-10-11T14:32:10.000Z")
  expect_equal(r$prediction$lookAhead$decrementSeconds, 636.4, tolerance = 1e-4)
  expect_equal(r$prediction$syringeEmptyAt, "2026-10-11T15:15:35.801Z")
  expect_equal(r$delivered$reported, 269.5)
  expect_equal(r$delivered$fromHistory, 269.4, tolerance = 1e-3)
  # The hash is pinned to the model, not to a literal: it is the MD5 of the
  # parameters at ten significant digits, the same on every platform, and the
  # literal in the documentation is illustrative.
  expect_match(r$model$hash, "^md5:[0-9a-f]{32}$")
  expect_equal(r$model$hash, tciApiModel("propofol", 70, 170, 50, "male")$hash)
  # The OpenAPI document repeats the body and the response as its examples,
  # with exactly the same keys as the real output.
  spec <- jsonlite::fromJSON(system.file("api", "openapi.json", package = "stanpumpR"), simplifyVector = FALSE)
  viaJson <- function(x) jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null"),
                                            simplifyVector = FALSE)
  expect_equal(spec$components$schemas$RateRequest$example, viaJson(body))
  ex <- spec$components$schemas$RateResponse$example
  keyTree <- function(x) if (is.list(x) && !is.null(names(x))) lapply(x, keyTree) else NULL
  expect_equal(keyTree(ex), keyTree(viaJson(r)))
  expect_equal(ex$controller, r$controller)
  expect_equal(ex$rate$value, r$rate$value)
  expect_equal(ex$fallback, viaJson(r$fallback))
  expect_equal(ex$timing, r$timing)
  expect_equal(ex$state$now$cp, r$state$now$cp, tolerance = 1e-3)
  expect_equal(ex$state$now$ce, r$state$now$ce, tolerance = 1e-3)
  expect_equal(ex$prediction$syringeEmptyAt, r$prediction$syringeEmptyAt)
  expect_match(ex$model$hash, "^md5:[0-9a-f]{32}$")
  expect_equal(ex$model$reference, r$model$reference)
  expect_equal(unlist(ex$rate$limitedBy), "rateResolution")
  expect_length(ex$warnings, 0)
})

test_that("times are parsed from any RFC 3339 offset and formatted in UTC with milliseconds", {
  expect_equal(tciApiParseTime("2026-10-11T14:32:05.250Z", "/now"), 1791729125.25)
  expect_equal(tciApiParseTime("2026-10-11T16:32:05.250+02:00", "/now"), 1791729125.25)
  expect_equal(tciApiParseTime("2026-10-11T09:02:05.250-05:30", "/now"), 1791729125.25)
  expect_equal(tciApiParseTime("2026-10-11T14:32:05Z", "/now"), 1791729125)
  expect_equal(tciApiParseTime("2026-10-11t14:32:05.1234567z", "/now"), 1791729125.1234567)
  expect_equal(tciApiFormatTime(1791729125.25), "2026-10-11T14:32:05.250Z")
  expect_equal(tciApiFormatTime(1791729125.9995), "2026-10-11T14:32:06.000Z")
  expect_equal(tciApiFormatTime(1791729125.001), "2026-10-11T14:32:05.001Z")
  expect_equal(tciApiFormatTime(1791729125.9994), "2026-10-11T14:32:05.999Z")
  expect_null(tciApiFormatTime(NULL))
  # timing.now is the request's own string, verbatim, whatever its offset or
  # precision, so that a controller can match a response to its request by
  # comparing the two (section 7, rule 1); the times the server computes are
  # UTC with milliseconds, on the controller's clock.
  r <- api(baseRequest(now = "2026-10-11T16:00:00.1234567+02:00"))
  expect_identical(r$timing$now, "2026-10-11T16:00:00.1234567+02:00")
  expect_equal(r$timing$effectiveAt, "2026-10-11T14:00:00.123Z")
  expect_equal(r$timing$holdUntil, "2026-10-11T14:00:10.123Z")
  expect_identical(api(baseRequest(now = "2026-10-11T14:00:00Z"))$timing$now, "2026-10-11T14:00:00Z")
  # A leap second parses; a 61st does not.
  expect_equal(tciApiParseTime("2026-10-11T23:59:60Z", "/now"), tciApiParseTime("2026-10-12T00:00:00Z", "/now"))
})

# ---------------------------------------------------------------------------
# 12. The compute budget (section 7.1)

# One plumber process serves one request at a time, so a request with
# TCI_API_MAX_HISTORY events, and one whose look-ahead cannot reach the target,
# must each compute well under 100 ms: about 70 ms for the 20000-event history
# on a 4-core sandbox, 10 ms for the look-ahead cases that decide at once.
# Before the history validation was vectorised and the look-ahead bounded they
# took 250-450 ms and 1-3 s.  These tests are regression guards against THAT,
# not a benchmark: two calls warm the byte-code compiler (R compiles a closure
# on its second call) and the least of three is measured, and they are skipped
# on CI, whose shared runners cannot keep a wall-clock promise.  The full suite
# running beside them on one machine was enough to fail a 100 ms budget once.
#
# A wall-clock budget alone, though, is not portable, and the look-ahead test
# had been failing on a Windows desktop (R 4.6.1) since the day it was
# written, outside CI, where nothing was watching.  What the look-ahead costs
# is PLANS -- tciApiPlan() calls, one tciRate() each -- and two of its four
# cases walk the whole horizon rather than deciding at once: the propofol
# ceiling just above the maintenance requirement costs 757 plans and the
# capped hydromorphone 634, against the bound the algorithm guarantees,
# TCI_API_LOOKAHEAD_EXACT_STEPS plans at the interval, about forty while the
# step grows by a tenth, and the rest of the horizon at
# TCI_API_LOOKAHEAD_MAX_STEP, so fewer than 900 whatever the interval.  Those
# are the same numbers on every machine, so they are what the look-ahead test
# asserts; the clock is only a ceiling, and it is now a test of its own, the
# one that carries the skips.  The counts therefore run everywhere -- on CI,
# on all four platforms the check matrix covers, and under
# testthat::test_file(), which sets no NOT_CRAN and so had been silently
# skipping this whole section, which is how the budget came to fail for two
# days without anyone seeing it.
#
# Hydromorphone's 634 plans are the dear ones -- tciRate() searches the effect
# site one second apart out to tPeak, 1187 grid points for its 19.6 min
# against propofol's 107 for 1.6 min -- and cost 0.58 s on that desktop
# against a 0.5 s budget while every other case stayed under 0.2 s.  Hence a
# second, looser ceiling for the two cases that walk the horizon: still well
# inside the 1-3 s the unbounded search cost, which is the regression being
# guarded.
# (Claude Code, 2026-10-14, at the request of Steven L. Shafer.)
TIME_BUDGET <- 0.5           # s: a request that decides in a handful of plans
HORIZON_TIME_BUDGET <- 2     # s: one that walks the look-ahead horizon
HORIZON_PLANS <- 900L        # plans: "a few", against one per interval of the
                             # horizon, thirty thousand at a 1 s interval
FEW_PLANS <- 10L             # plans: a request that decides at once
budget <- function(req) { for (i in 1:2) api(req); min(replicate(3, system.time(api(req))[["elapsed"]])) }

# The plans one request costs, the same count on every machine.
plansFor <- function(req)
{
  real <- tciApiPlan
  n <- 0L
  with_mocked_bindings(api(req), tciApiPlan = function(...) { n <<- n + 1L; real(...) })
  n
}

# The four look-ahead cases, built once for the two tests that take them: a
# propofol ceiling below the maintenance requirement, so the target is never
# reached and the answer is decided at once; one just above it, reached hours
# later, asymptotically, so the loop walks the whole horizon on the growing
# step; and the slowest TCI drug at the shortest interval, unlimited -- which
# reaches the target in a handful of plans -- and capped, which makes its
# approach asymptotic too, at hydromorphone's tPeak, where every plan is a
# long effect-site search.
lookAheadCases <- function()
{
  capped <- baseRequest(rateUnits = "mL/hr", maxRate = 20, latency = 0.5, interval = 1)
  capped$infusate <- list(concentration = 10, units = "mg/mL")
  slow <- capped
  slow$pump$maxRate <- 30
  hm <- baseRequest(drug = "hydromorphone", target = 4, units = "ng/mL", rateUnits = "mcg/min",
                    interval = 1, latency = 0.2)
  hmCapped <- hm
  hmCapped$pump$maxRate <- 10
  list(capped = capped, slow = slow, hm = hm, hmCapped = hmCapped)
}

test_that("a history of TCI_API_MAX_HISTORY events computes within the time budget", {
  skip_on_cran()
  skip_on_ci()
  n <- TCI_API_MAX_HISTORY
  starts <- -(n:1) * 10 / 60                                   # one event every 10 s for 55 hours
  values <- 1.2 + 0.1 * (seq_len(n) %% 7)
  rates <- Map(function(t, v) rateChange(t, v), starts, values)
  req <- baseRequest(history = rates)
  r <- api(req)
  expect_equal(r$delivered$fromHistory, sum(values * 10 / 60), tolerance = 1e-9)
  expect_lt(budget(req), TIME_BUDGET)
  # Delivered segments in millilitres, through the syringe, a hand bolus, and
  # a look-ahead.
  segs <- Map(function(a, b) segment(a, b, 0.2, "mL"), starts[-n], starts[-1])
  req <- baseRequest(rateUnits = "mL/hr", maxRate = 1200, history = c(segs, list(bolus(-30, 1, "mL"))))
  req$infusate <- list(concentration = 10, units = "mg/mL")
  req$lookAhead <- list(concentration = 1)
  r <- api(req)
  expect_equal(r$delivered$fromHistory, 0.2 * 10 * (n - 1), tolerance = 1e-9)
  expect_lt(budget(req), TIME_BUDGET)
})

test_that("targetReachedAt costs a few plans, not one per second of the horizon", {
  case <- lookAheadCases()
  r <- api(case$capped)
  expect_null(r$prediction$targetReachedAt)
  expect_equal(r$rate$value, 20)
  K <- tciKinetics(pkOf("propofol"))
  expect_lt(20 / 60 * 10 * sum(K$pinf), 3 * (1 - TCI_API_ON_TARGET))      # the steady state the ceiling can hold
  expect_lt(plansFor(case$capped), FEW_PLANS)
  r <- api(case$slow)
  expect_gt(minutesOf(r$prediction$targetReachedAt), 60)
  expect_lt(plansFor(case$slow), HORIZON_PLANS)
  expect_false(is.null(api(case$hm)$prediction$targetReachedAt))
  expect_lt(plansFor(case$hm), FEW_PLANS)
  expect_false(is.null(api(case$hmCapped)$prediction$targetReachedAt))
  expect_lt(plansFor(case$hmCapped), HORIZON_PLANS)
})

test_that("the look-ahead computes within its wall-clock ceiling", {
  skip_on_cran()
  skip_on_ci()
  case <- lookAheadCases()
  expect_lt(budget(case$capped), TIME_BUDGET)
  expect_lt(budget(case$slow), HORIZON_TIME_BUDGET)
  expect_lt(budget(case$hm), TIME_BUDGET)
  expect_lt(budget(case$hmCapped), HORIZON_TIME_BUDGET)
})
