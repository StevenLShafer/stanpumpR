# Physiology events (R/physiologyEvents.R): the physiology panel recorded at
# a time, physiology normal from 0 and each event's from its time, the
# amounts in the compartments carried across each step (convertState()).

# An independent reference: the compartment amounts and the effect site
# advanced by one augmented matrix exponential per piece, the amounts carried
# across every switch of parameters, a bolus added to the central amount.
physiologyEventReference <- function(pieces, boluses, t)
{
  K <- function(p, rate) {
    M <- matrix(0, 5, 5)
    M[1, 1:3] <- c(-(p$k10 + p$k12 + p$k13), p$k21, p$k31)
    M[2, 1:2] <- c(p$k12, -p$k21)
    M[3, c(1, 3)] <- c(p$k13, -p$k31)
    M[4, 1] <- p$ke0 / p$v1
    M[4, 4] <- -p$ke0
    M[1, 5] <- rate
    M
  }
  x <- c(0, 0, 0, 0, 1)
  last <- pieces[[1]]
  for (pc in pieces) {
    if (t <= pc$from) break
    x[1] <- x[1] + sum(boluses$amount[boluses$time == pc$from])
    dt <- min(t, pc$to) - pc$from
    x <- as.vector(as.matrix(Matrix::expm(Matrix::Matrix(K(pc$pk, pc$rate) * dt))) %*% x)
    last <- pc
  }
  c(Cp = x[1] / last$pk$v1, Ce = x[4])
}

test_that("a physiology event's settings survive the event table and refuse what is out of range", {
  x <- physiologyEventNormal()
  x[["cardiac"]] <- 0.5
  x[["temperature"]] <- 33.5
  x[["altitude"]] <- 2400
  x[["endobronchial"]] <- 1
  s <- physiologyEventEncode(x)
  expect_equal(physiologyEventDecode(s), checkPhysiologyEvent(x))
  expect_null(physiologyEventDecode("liver=1;liver=2"))
  expect_null(physiologyEventDecode("cardiac=50"))
  expect_null(physiologyEventDecode("altitude=-1"))
  expect_null(physiologyEventDecode("endobronchial=0.5"))
  expect_null(physiologyEventDecode("nonsense"))
  expect_null(physiologyEventDecode(strrep("a", 1000)))
  expect_null(physiologyEventDecode(c("liver=1", "liver=1")))
  expect_null(physiologyEventDecode(NA_character_))
  # A missing field is normal
  expect_equal(physiologyEventDecode("cardiac=0.5")[["liver"]], 1)
  expect_match(physiologyEventSummary(x), "Cardiac output x0.5")
  expect_match(physiologyEventSummary(x), "Endobronchial intubation")
  expect_false(grepl("Altitude", physiologyEventSummary(x, gas = FALSE)))
})

test_that("physiologyEventsFrom() reads only readable Physiology rows, in time order, the last of a tie", {
  a <- physiologyEventNormal(); a[["liver"]] <- 0.5
  b <- physiologyEventNormal(); b[["liver"]] <- 2
  ET <- data.frame(Time = c(90, 30, 30, 10, 20),
                   Event = c("Physiology", "Physiology", "Physiology", "CPB Start", "Physiology"),
                   Settings = c(physiologyEventEncode(a), physiologyEventEncode(a),
                                physiologyEventEncode(b), "", "liver=999"))
  pe <- physiologyEventsFrom(ET)
  expect_equal(pe$Time, c(30, 90))
  expect_equal(pe$liver, c(2, 0.5))
  expect_equal(nrow(physiologyEventsFrom(ET[, c("Time", "Event")])), 0)
  expect_equal(nrow(physiologyEventsFrom(NULL)), 0)
})

test_that("the clinical and physiology events interleave on one time line", {
  tl <- physiologyEventTimeline(
    data.frame(Time = c(30, 70), Event = c("CPBStart", "CPBEnd")),
    data.frame(Time = c(50, 70), State = 1:2))
  expect_equal(tl$Time, c(0, 30, 50, 70))
  expect_equal(tl$Event, c("default", "CPBStart", "CPBStart@physiology1",
                           "CPBEnd@physiology2"))
})

test_that("propofol steps to an event's physiology with the amounts carried across (matrix exponential)", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "propofol", ]
  s <- physiologyEventNormal()
  s[["cardiac"]] <- 0.5
  s[["liver"]] <- 0.3
  ET <- data.frame(Time = 40, Event = "Physiology", Settings = physiologyEventEncode(s))
  build <- function(p) getDrugPK("propofol", 70, 170, 50, "male", row, physiology = p)
  PK <- physiologyEventPK(build, physiologyEventsFrom(ET))
  expect_equal(names(PK$PK), c("default", "default@physiology1"))
  p0 <- PK$PK[["default"]]
  p1 <- PK$PK[["default@physiology1"]]
  # Normal physiology before the event: the set built with no physiology
  expect_equal(p0$v1, getDrugPK("propofol", 70, 170, 50, "male", row)$PK[[1]]$v1)
  expect_false(isTRUE(all.equal(p0$v1, p1$v1)))
  expect_false(isTRUE(all.equal(p0$ke0, p1$ke0)))

  DT <- data.frame(Drug = "propofol", Time = c(0, 0, 60, 90), Dose = c(2, 100, 1, 0),
                   Units = c("mg/kg", "mcg/kg/min", "mg/kg", "mcg/kg/min"))
  sim <- simCpCe(DT, ET, PK, maximum = 180, plotRecovery = FALSE)
  w <- sim$wide
  r <- 100 * 70 / 1000
  pieces <- list(list(from = 0, to = 40, pk = p0, rate = r),
                 list(from = 40, to = 60, pk = p1, rate = r),
                 list(from = 60, to = 90, pk = p1, rate = r),
                 list(from = 90, to = 1e9, pk = p1, rate = 0))
  boluses <- data.frame(time = c(0, 60), amount = c(140, 70))
  # Points either side of each switch, not on it (the plotted line carries
  # both values at a switch)
  g <- w$Time[w$Time > 0 & !(w$Time %in% c(40, 60, 90))]
  ref <- vapply(g, function(t) physiologyEventReference(pieces, boluses, t), numeric(2))
  i <- match(g, w$Time)
  expect_lt(max(abs(w$Plasma[i] / ref["Cp", ] - 1)), 1e-9)
  expect_lt(max(abs(w$`Effect Site`[i] / ref["Ce", ] - 1)), 1e-7)
  # The plasma concentration steps at the event (V1 halves); the amount does not
  before <- physiologyEventReference(pieces, boluses, 40 - 1e-9)[["Cp"]]
  after <- physiologyEventReference(pieces, boluses, 40 + 1e-9)[["Cp"]]
  expect_equal(after / before, p0$v1 / p1$v1, tolerance = 1e-6)
})

test_that("a bolus at the event's own time enters after the step, once", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "propofol", ]
  s <- physiologyEventNormal()
  s[["cardiac"]] <- 0.4
  ET <- data.frame(Time = 30, Event = "Physiology", Settings = physiologyEventEncode(s))
  build <- function(p) getDrugPK("propofol", 70, 170, 50, "male", row, physiology = p)
  PK <- physiologyEventPK(build, physiologyEventsFrom(ET))
  p0 <- PK$PK[["default"]]
  p1 <- PK$PK[["default@physiology1"]]
  DT <- data.frame(Drug = "propofol", Time = c(0, 30, 30), Dose = c(1, 1, 50),
                   Units = c("mg/kg", "mg/kg", "mcg/kg/min"))
  w <- simCpCe(DT, ET, PK, maximum = 120, plotRecovery = FALSE)$wide
  pieces <- list(list(from = 0, to = 30, pk = p0, rate = 0),
                 list(from = 30, to = 1e9, pk = p1, rate = 50 * 70 / 1000))
  boluses <- data.frame(time = c(0, 30), amount = c(70, 70))
  g <- w$Time[w$Time > 0 & w$Time != 30]
  ref <- vapply(g, function(t) physiologyEventReference(pieces, boluses, t), numeric(2))
  i <- match(g, w$Time)
  expect_lt(max(abs(w$Plasma[i] / ref["Cp", ] - 1)), 1e-9)
  expect_lt(max(abs(w$`Effect Site`[i] / ref["Ce", ] - 1)), 1e-7)
})

test_that("an oral dose absorbing across a step: the depot carries over, ka steps with the gut", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "metoprolol", ]
  s <- physiologyEventNormal()
  s[["cardiac"]] <- 0.6
  s[["gi"]] <- 3
  ET <- data.frame(Time = 45, Event = "Physiology", Settings = physiologyEventEncode(s))
  build <- function(p) getDrugPK("metoprolol", 70, 170, 50, "male", row, physiology = p)
  PK <- physiologyEventPK(build, physiologyEventsFrom(ET))
  p0 <- PK$PK[["default"]]
  p1 <- PK$PK[["default@physiology1"]]
  expect_equal(p1$ka_PO / p0$ka_PO, 3, tolerance = 1e-9)
  DT <- data.frame(Drug = "metoprolol", Time = 0, Dose = 100, Units = "mg PO")
  w <- simCpCe(DT, ET, PK, maximum = 240, plotRecovery = FALSE)$wide
  # The reference: a depot emptying at ka into the central compartment
  M <- function(p) {
    A <- matrix(0, 6, 6)
    A[1, 1:3] <- c(-(p$k10 + p$k12 + p$k13), p$k21, p$k31)
    A[2, 1:2] <- c(p$k12, -p$k21)
    A[3, c(1, 3)] <- c(p$k13, -p$k31)
    A[4, 1] <- p$ke0 / p$v1
    A[4, 4] <- -p$ke0
    A[1, 5] <- p$ka_PO
    A[5, 5] <- -p$ka_PO
    A
  }
  lag <- if (is.null(p0$tlag_PO)) 0 else p0$tlag_PO
  ref <- function(t) {
    x <- numeric(6)
    knots <- sort(unique(c(0, lag, 45, t)))
    knots <- knots[knots <= t]
    for (k in seq_along(knots)[-1]) {
      a <- knots[k - 1]
      # 100 mg in the engine's mcg (metoprolol is plotted in ng/mL)
      if (a == lag) x[5] <- x[5] + 1e5 * p0$bioavailability_PO
      p <- if (a < 45) p0 else p1
      x <- as.vector(as.matrix(Matrix::expm(Matrix::Matrix(M(p) * (knots[k] - a)))) %*% x)
    }
    c(Cp = x[1] / (if (t <= 45) p0 else p1)$v1, Ce = x[4])
  }
  g <- w$Time[w$Time > lag + 1 & w$Time != 45]
  r <- vapply(g, ref, numeric(2))
  i <- match(g, w$Time)
  expect_lt(max(abs(w$Plasma[i] / r["Cp", ] - 1)), 1e-8)
  expect_lt(max(abs(w$`Effect Site`[i] / r["Ce", ] - 1)), 1e-6)
})

test_that("physiology events and clinical events switch together (dexmedetomidine on bypass)", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "dexmedetomidine", ]
  s <- physiologyEventNormal()
  s[["cardiac"]] <- 2
  s[["liver"]] <- 0.5
  ET <- data.frame(Time = c(30, 50, 70), Event = c("CPB Start", "Physiology", "CPB End"),
                   Settings = c("", physiologyEventEncode(s), ""))
  build <- function(p) getDrugPK("dexmedetomidine", 7, 65, 0.5, "male", row, physiology = p)
  PK <- physiologyEventPK(build, physiologyEventsFrom(ET))
  sets <- names(PK$PK)
  pre <- PK$PK[[sets[1]]]
  on <- PK$PK[[grep("^CPBStart$", sets, value = TRUE)]]
  onP <- PK$PK[[grep("^CPBStart@physiology1$", sets, value = TRUE)]]
  postP <- PK$PK[[grep("^CPBEnd@physiology1$", sets, value = TRUE)]]
  DT <- data.frame(Drug = "dexmedetomidine", Time = c(0, 0, 90), Dose = c(1, 0.7, 0),
                   Units = c("mcg/kg", "mcg/kg/hr", "mcg/kg/hr"))
  w <- simCpCe(DT, ET, PK, maximum = 240, plotRecovery = FALSE)$wide
  r <- 0.7 * 7 / 60
  pieces <- list(list(from = 0, to = 30, pk = pre, rate = r),
                 list(from = 30, to = 50, pk = on, rate = r),
                 list(from = 50, to = 70, pk = onP, rate = r),
                 list(from = 70, to = 90, pk = postP, rate = r),
                 list(from = 90, to = 1e9, pk = postP, rate = 0))
  boluses <- data.frame(time = 0, amount = 7)
  g <- w$Time[w$Time > 0 & !(w$Time %in% c(30, 50, 70, 90))]
  ref <- vapply(g, function(t) physiologyEventReference(pieces, boluses, t)[["Cp"]], numeric(1))
  expect_lt(max(abs(w$Plasma[match(g, w$Time)] / ref - 1)), 1e-9)
})

test_that("without a physiology event nothing changes, and an event at normal physiology changes nothing", {
  DT <- data.frame(Drug = "propofol", Time = c(0, 0, 60), Dose = c(2, 100, 0),
                   Units = c("mg/kg", "mcg/kg/min", "mcg/kg/min"))
  ET0 <- data.frame(Time = 30, Event = "Intubation", Settings = "")
  plain <- simulateDrugsWithCovariates(DT, ET0, 70, 170, 50, "male", maximum = 120,
                                       plotRecovery = FALSE)
  ET1 <- rbind(ET0, data.frame(Time = 45, Event = "Physiology",
                               Settings = physiologyEventEncode(physiologyEventNormal())))
  same <- simulateDrugsWithCovariates(DT, ET1, 70, 170, 50, "male", maximum = 120,
                                      plotRecovery = FALSE)
  a <- plain$propofol$wide
  b <- same$propofol$wide
  b <- b[b$Time %in% a$Time & !duplicated(b$Time), ]
  a <- a[a$Time %in% b$Time & !duplicated(a$Time), ]
  expect_equal(b$Plasma, a$Plasma, tolerance = 1e-12)
  # With an event, the panel's physiology is not used: normal from 0
  s <- physiologyEventNormal(); s[["liver"]] <- 0.2
  phys <- PHYSIOLOGY_DEFAULT; phys[["liver"]] <- 0.2
  ET2 <- rbind(ET0, data.frame(Time = 45, Event = "Physiology", Settings = physiologyEventEncode(s)))
  ev <- simulateDrugsWithCovariates(DT, ET2, 70, 170, 50, "male", maximum = 120,
                                    plotRecovery = FALSE, physiology = phys)
  w <- ev$propofol$wide
  early <- w[w$Time > 0 & w$Time < 45, ]
  # (on the same time line: the event engine's, with an event at 45)
  ref <- same$propofol$wide
  expect_equal(early$Plasma, ref$Plasma[match(early$Time, ref$Time)], tolerance = 1e-12)
  late <- w[w$Time > 50, ]
  expect_gt(min(late$Plasma / ref$Plasma[match(late$Time, ref$Time)]), 1)
})

test_that("a drug the sliders cannot switch during a run says so and keeps normal physiology", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "phenytoin", ]
  s <- physiologyEventNormal(); s[["liver"]] <- 0.5
  build <- function(p) getDrugPK("phenytoin", 70, 170, 50, "male", row, physiology = p)
  PK <- physiologyEventPK(build, physiologyEventsFrom(
    data.frame(Time = 60, Event = "Physiology", Settings = physiologyEventEncode(s))))
  expect_type(PK$physiologyStepsSkipped, "character")
  expect_null(PK[["physiologyEvents"]])
})

# --- The inhaled agents --------------------------------------------------------

gasEventTable <- function(...) {
  s <- physiologyEventNormal()
  over <- list(...)
  for (n in names(over)) s[[n]] <- over[[n]]
  ev <- data.frame(Time = 20)
  for (f in PHYSIOLOGY_EVENT_FIELDS) ev[[f]] <- s[[f]]
  ev
}
gasDT <- data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"), Time = 0,
                    Dose = c(2, 1, 1, 5))

test_that("an inhaled agent's physiology event at normal physiology changes nothing", {
  plain <- simulateGases(gasDT, weight = 70, age = 40, maximum = 60)
  same <- simulateGases(gasDT, weight = 70, age = 40, maximum = 60,
                        physiologyEvents = gasEventTable())
  tb <- same$timeLine
  keep <- !duplicated(tb, fromLast = TRUE)
  i <- match(tb[keep], plain$timeLine)
  ok <- !is.na(i)
  # The extra change point only moves the sub-steps of the uptake coupling
  expect_lt(max(abs(same$state$sevoflurane[keep, ][ok, ] - plain$state$sevoflurane[i[ok], ])), 1e-8)
})

test_that("a temperature step carries the agent in each tissue across, and MAC steps with it", {
  st <- simulateGases(gasDT, weight = 70, age = 40, maximum = 60,
                      physiologyEvents = gasEventTable(temperature = 32, cardiac = 0.5))
  i <- which(st$timeLine == 20)
  expect_length(i, 2)
  expect_equal(st$bodyRow[i], 1:2)
  capacity <- function(body) {
    p <- gasPropertiesAt(body$temperature)
    l <- gasPartitionTissueGas(p[p$gas == "sevoflurane", ])
    v <- gasTissueVolumes(body)
    v * l[c("brain", "muscle", "fat", "brain")[seq_along(v)]]
  }
  b0 <- st$bodySteps$body[[1]]
  b1 <- st$bodySteps$body[[2]]
  # Both bodies split the vessel-rich group: one has cardiac output off normal
  expect_equal(gasStateCount(b0), 6L)
  before <- st$state$sevoflurane[i[1], ]
  after <- st$state$sevoflurane[i[2], ]
  expect_equal(sum(after[3:6] * capacity(b1)), sum(before[3:6] * capacity(b0)), tolerance = 1e-12)
  # The gas phase keeps its partial pressure; a cooled tissue holds the same
  # agent at a lower one
  expect_equal(after[1:2], before[1:2])
  expect_true(all(after[3:6] < before[3:6]))
  # MAC is lower at 32 C, so the summed MAC steps up at the same alveolar
  mac <- st$results[st$results$Drug == "MAC", ]
  m <- mac$Y[mac$Time == 20]
  expect_gt(m[2], m[1])
  # The time until threshold is computed per segment, on each segment's body
  w <- gasWashout(st, data.frame(Time = gasDT$Time, Drug = gasDT$Drug, Dose = gasDT$Dose), weight = 70)
  expect_length(w$segments, 2)
  r <- gasRecoveryTime(w, "sevoflurane", 1)
  expect_length(r, length(st$timeLine))
  expect_true(all(is.finite(r)))
  entries <- gasDrugEntries(st, gasDT, maximum = 60, washout = w, age = 40)
  expect_true(all(c("sevoflurane", "MAC") %in% names(entries)))
})

test_that("the Visualizer draws a cardiac-output step and refuses a temperature step", {
  co <- simulateGases(gasDT, weight = 70, age = 40, maximum = 60,
                      physiologyEvents = gasEventTable(cardiac = 0.5))
  entries <- gasDrugEntries(co, gasDT, maximum = 60, age = 40)
  run <- viewerGasRun(entries$sevoflurane, 0, 60)
  expect_length(run$cardiacOutput, length(run$t))
  expect_equal(range(run$cardiacOutput), co$bodySteps$body[[1]]$Q_cardiac * c(0.5, 1))
  expect_length(run$physiologySteps, 2)
  expect_equal(run$physiologySteps[[2]]$time, 20 * 60)
  expect_equal(run$physiologySteps[[2]]$cardiac, 0.5)
  cold <- simulateGases(gasDT, weight = 70, age = 40, maximum = 60,
                        physiologyEvents = gasEventTable(temperature = 33))
  coldEntries <- gasDrugEntries(cold, gasDT, maximum = 60, age = 40)
  expect_error(viewerGasRun(coldEntries$sevoflurane, 0, 60), "temperature or intubation")
})

# --- The event table -----------------------------------------------------------

test_that("a Physiology row needs readable settings wherever an event table is read", {
  s <- physiologyEventNormal(); s[["cardiac"]] <- 0.5
  good <- eventTableRows(c(10, 20), c("Intubation", "Physiology"), c("", physiologyEventEncode(s)))
  expect_true(validateEventTableInput(good))
  bad <- eventTableRows(20, "Physiology", "cardiac=99")
  expect_error(validateEventTableInput(bad), "Create event")
  expect_error(validateEventTableInput(eventTableRows(20, "Physiology", "")), "Create event")
  # A table with no Settings column (a scenario, the API) is still read
  expect_true(validateEventTableInput(data.frame(Time = 10, Event = "Intubation")))
  # A link: an unreadable Physiology row is dropped, a readable one kept,
  # and another event's settings are emptied
  link <- list(Time = c(10, 20, 30), Event = c("Intubation", "Physiology", "Physiology"),
               Settings = c("liver=0.5", physiologyEventEncode(s), "<script>"))
  r <- restoredEventTable(link, getEventDefaults(), eventTableInit)
  expect_true(r$changed)
  expect_equal(r$table$Time, c(10, 20))
  expect_equal(r$table$Settings, c("", physiologyEventEncode(s)))
  # Settings show only what is off normal
  expect_identical(physiologyEventEncode(s), "cardiac=0.5")
  expect_identical(physiologyEventEncode(physiologyEventNormal()), "normal")
  expect_equal(physiologyEventDecode("normal"), physiologyEventNormal())
})

test_that("the hover tip names the Physiology event under the pointer", {
  s <- physiologyEventNormal(); s[["liver"]] <- 0.2
  ET <- eventTableRows(c(10, 60), c("Intubation", "Physiology"), c("", physiologyEventEncode(s)))
  tip <- physiologyEventHover(ET, 61, 180, gas = FALSE)
  expect_equal(tip$time, 60)
  expect_match(tip$text, "Liver x0.2")
  expect_null(physiologyEventHover(ET, 100, 180))
  expect_null(physiologyEventHover(ET[1, ], 10, 180))
})

test_that("the workbook records the physiology events in place of the panel", {
  s <- physiologyEventNormal(); s[["renal"]] <- 0.3
  events <- physiologyEventsFrom(eventTableRows(45, "Physiology", physiologyEventEncode(s)))
  rows <- physiologyCovariateRows(c(liver = 2), 37, 0, FALSE, events)
  expect_equal(nrow(rows), 2)
  expect_match(rows$Value[1], "normal physiology from time 0")
  expect_match(rows$Value[2], "Kidney x0.3")
})

test_that("the Visualizer of an intravenous drug follows the physiology events", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "propofol", ]
  s <- physiologyEventNormal(); s[["cardiac"]] <- 0.5
  ET <- eventTableRows(40, "Physiology", physiologyEventEncode(s))
  events <- physiologyEventsFrom(ET)
  PK <- physiologyEventPK(function(p) getDrugPK("propofol", 70, 170, 50, "male", row, physiology = p),
                          events)
  entry <- PK
  entry$ET <- ET
  inForce <- viewerSetsInForce(entry, PK$PK, c(0, 39, 40, 100), 180)
  expect_equal(inForce, c("default", "default", "default@physiology1", "default@physiology1"))
  steps <- viewerPhysiologySteps(events, 10)
  expect_equal(vapply(steps, `[[`, 0, "time"), c(600, 2400))
  expect_equal(steps[[2]]$cardiac, 0.5)
})

test_that("Create event records the panel's settings in the event table", {
  local_mocked_bindings(outputComments = function(...) {})
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  shiny::testServer(app_server, {
    session$setInputs(timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00",
                      maximum = "240", weight = 70, weightUnit = "1", height = 170,
                      heightUnit = "1", age = 40, ageUnit = "1", sex = "male")
    session$setInputs(physiology_cardiac = log10(0.5), physiology_temperature = 34)
    session$setInputs(physiologyCreateEvent = 1)
    session$setInputs(physiologyEventTime = "abc", physiologyCreateEventBtn = 1)
    expect_equal(nrow(eventTable()), 0)
    session$setInputs(physiologyEventTime = "30", physiologyCreateEventBtn = 2)
    ET <- eventTable()
    expect_equal(ET$Time, 30)
    expect_equal(ET$Event, "Physiology")
    x <- physiologyEventDecode(ET$Settings)
    expect_equal(x[["cardiac"]], 0.5, tolerance = 1e-9)
    expect_equal(x[["temperature"]], 34)
    expect_equal(nrow(physiologyEvents()), 1)
    # Add event never offers Physiology, which has no settings to record
    session$setInputs(clickTimeEvent = "50", clickEvent = "Physiology", addEventBtn = 1)
    expect_equal(nrow(eventTable()), 1)
  })
})
