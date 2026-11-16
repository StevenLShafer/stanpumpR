# Carbon's fourth UI audit (Gemini, of PR #730 at 59094cf, 2026-11-15),
# findings C3 and M6: the engines.
#
# C3: legal regimens held a worker.  Seven interacting antiseizure drugs over
# 52 weeks took 92 s here (Carbon: past 9 min 50 s on its machine), because
#   - advanceClosedForm1() found the PK set at each point and each segment's
#     points with a pass over the time line for every event, and read the
#     parameters by row NAME (pmatch() over every row for every point); the
#     lookups are now findInterval() and row numbers, bit for bit the same;
#   - the Michaelis-Menten engine put every interaction switch on its time
#     line as a knot, with the fine fill after it and the instant before it
#     (phenytoin: 36,469 points for 2,565 switches, against 9,100 alone);
#     the switches are now plain points of the line, with the grid on which
#     the interactions read the drug's concentration;
#   - the time until threshold was computed in a final pass that
#     re-simulated every victim, phenytoin's whole integration included,
#     where for a saturable drug or a drug without a threshold it costs
#     nothing and is now computed in every pass;
#   - the engine's derivative searched an unhashed frame of some 110
#     variables for every name it read; it is now built by mmDerivative().
# And the Drug Library's first use built the PK of every drug in mcg or ng to
# learn which are on the saturable engine (11.8 s cold on Carbon's machine);
# drugOnSaturableEngine() is now a list, held here to getDrugPK().
#
# M6: voriconazole's Vmax declines with time since the first dose, which the
# engine took as the first ROW, a dose of 0 included (the startup menu writes
# one at time 0): the first real dose a day later met a Vmax already
# inhibited.  The first dose is now the first above 0.
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

noEvents <- data.frame(Time = numeric(0), Event = character(0))
G <- getDrugDefaultsGlobal()

simulatedFor <- function(DT, maximum, plotRecovery = FALSE, asm = FALSE, defaults = G,
                         events = noEvents) {
  drugs <- recalculatePK(NULL, defaults, DT, age = 50, weight = 70, height = 170, sex = "male",
                         antiseizureInteractions = asm)
  processdoseTable(DT, events, drugs, maximum, plotRecovery, antiseizureInteractions = asm)
}

# --- M6: a dose of 0 is not the first dose -----------------------------------

test_that("a 0 mg row before voriconazole's first dose does not start its Vmax decline (M6)", {
  # Carbon's example: 400 mg PO a day in, with and without the startup menu's
  # 0 mg row at time 0.  At 59094cf the 0 row started the decline a day early
  # and the peak read 3.187 against 3.146 mcg/mL (1.3%; Carbon reported 26.9
  # against 7.9, which did not reproduce), more after it.
  real <- data.frame(Drug = "voriconazole", Time = 1440, Dose = 400, Units = "mg PO",
                     stringsAsFactors = FALSE)
  for (zero in c("mg PO", "mg/hr")) {
    withZero <- rbind(data.frame(Drug = "voriconazole", Time = 0, Dose = 0, Units = zero,
                                 stringsAsFactors = FALSE), real)
    a <- simulatedFor(withZero, 4320)$voriconazole$wide
    b <- simulatedFor(real, 4320)$voriconazole$wide
    # (the 0 mg PO row's absorption lag puts a few points of its own on the
    # line, where both curves are 0)
    common <- intersect(a$Time, b$Time)
    expect_gt(sum(common > 1440), 100)
    expect_equal(a$Plasma[match(common, a$Time)], b$Plasma[match(common, b$Time)],
                 tolerance = 1e-12)
  }
})

# --- C3: the saturable-engine list -------------------------------------------

test_that("drugOnSaturableEngine() is getDrugPK()'s answer for every drug in the library (C3)", {
  # Its old definition, at the reference adult, for every drug; and, where the
  # model can be built, at a child
  pkHasMM <- function(drug, ...) {
    PK <- tryCatch(getDrugPK(drug, ...), error = function(e) NULL)
    if (is.null(PK)) NA else !is.null(PK$michaelisMenten)
  }
  drugs <- as.character(G$Drug)
  adult <- vapply(drugs, function(d) isTRUE(pkHasMM(d, weight = 70, height = 170, age = 40,
                                                     sex = SEX_MALE)), logical(1))
  listed <- vapply(drugs, drugOnSaturableEngine, logical(1))
  child <- vapply(drugs, pkHasMM, logical(1), weight = 20, height = 115, age = 6, sex = SEX_FEMALE)
  # A group whose model is chosen by age is listed when either patient's
  # model is on the engine (drugOnSaturableEngine())
  expect_identical(listed, adult | (!is.na(child) & child))
  expect_identical(sort(names(listed)[listed]),
                   sort(c("cetagliptin", "exenatide", "GHB", "linagliptin", "phenytoin",
                          "vildagliptin", "voriconazole",
                          "cyclophosphamide", "cyclophosphamideMcCune", "fluorouracil",
                          "fluorouracilBriki", "fluorouracilTan", "ifosfamide", "paclitaxel",
                          "paclitaxelCrombag", "paclitaxelJoerger", "paclitaxelProteinBound",
                          "thioguanine", "thioguanineIV",
                          "vincristine", "vincristinePediatric")))
  known <- !is.na(child)
  # every drug on the engine for a child is listed
  expect_true(all(listed[known][child[known]]))
  # and the only drugs listed that a child's model leaves off it are the
  # groups whose adult model is on it
  expect_true(all(adult[known][listed[known] & !child[known]]))
})

test_that("the Drug Library's locked units cost nothing to compute (C3)", {
  elapsed <- system.time(
    locked <- concentrationUnitsLocked(as.character(G$Drug), as.character(G$Concentration.Units))
  )[["elapsed"]]
  expect_lt(elapsed, 1)
  expect_true(locked[G$Drug == "phenytoin"])
  expect_false(locked[G$Drug == "propofol"])
  expect_false(drugOnSaturableEngine(NA_character_))
})

# --- C3: the event engine's lookups ------------------------------------------

test_that("pkEventIndex() is the event table's last row at or before each time (C3)", {
  # The loop advanceClosedForm1() used, for every case it can meet: sorted,
  # unsorted, tied, a time before every event, no event at all
  bruteForce <- function(times, eventTimes) {
    out <- integer(length(times))
    for (j in seq_along(eventTimes)) out[times >= eventTimes[j]] <- j
    out
  }
  set.seed(4)
  times <- sort(c(0, 0.5, 10, 10, 59.99, 60, 61, 1e5, runif(200, 0, 2000)))
  cases <- list(c(0, 60, 120), c(0, 60, 60, 120), c(60, 0, 120, 30), c(5, 5, 5),
                c(100, 200), numeric(0), sample(round(runif(50, 0, 2000))), 0)
  for (eventTimes in cases)
    expect_identical(pkEventIndex(times, eventTimes), bruteForce(times, eventTimes))
})

test_that("an interaction run through the event engine is unchanged (C3)", {
  # Pinned at 59094cf, before the lookups changed: they read the same rows,
  # so the same doubles (to a tolerance only for other platforms' libm)
  DT <- data.frame(Drug = c("lamotrigine", "carbamazepine", "valproate"), Time = c(0, 0, 4320),
                   Dose = c(100, 400, 500), Units = c("mg PO bid", "mg PO bid", "mg PO DR bid"),
                   stringsAsFactors = FALSE)
  out <- simulatedFor(DT, 21 * 1440, asm = TRUE)
  at <- function(drug, times) out[[drug]]$wide$Plasma[match(times, out[[drug]]$wide$Time)]
  expect_equal(nrow(out$lamotrigine$interaction$CL), 191)
  expect_equal(at("lamotrigine", c(720, 10080, 20160, 30240)),
               c(0.90953119360635593, 4.1974622486443804, 3.9708570414423954,
                 3.8655746842418091), tolerance = 1e-9)
  expect_equal(sum(out$lamotrigine$wide$Plasma), 23335.340149939024, tolerance = 1e-9)
  expect_equal(at("carbamazepine", c(720, 10080, 20160, 30240)),
               c(2.4497747968288675, 8.0169027613422763, 6.8105332273069896,
                 6.643078530623538), tolerance = 1e-9)
  expect_equal(sum(out$carbamazepine$wide$Plasma), 18873.621889626153, tolerance = 1e-9)
  expect_equal(sum(out$valproate$wide$Plasma), 71494.365389305283, tolerance = 1e-9)
})

test_that("dexmedetomidine across bypass is unchanged by the event lookups (C3)", {
  DT <- data.frame(Drug = "dexmedetomidine", Time = c(0, 0, 30, 200),
                   Dose = c(1, 0.5, 0.7, 0), Units = c("mcg/kg", "mcg/kg/hr", "mcg/kg/hr", "mcg/kg/hr"),
                   stringsAsFactors = FALSE)
  ET <- data.frame(Time = c(60, 70, 80, 90, 100, 110, 120, 180),
                   Event = c("CPBStart", "CPB36", "CPB35", "CPB34", "CPB33", "CPB32", "CPB31", "CPBEnd"),
                   stringsAsFactors = FALSE)
  x <- simulatedFor(DT, 600, plotRecovery = TRUE, events = ET)$dexmedetomidine$wide
  # Pinned at 59094cf
  expect_equal(nrow(x), 87)
  expect_equal(sum(x$Plasma), 91.255105748065972, tolerance = 1e-9)
  expect_equal(sum(x$`Effect Site`), 55.25942864272389, tolerance = 1e-9)
  expect_equal(sum(x$Recovery, na.rm = TRUE), 10918.180203764487, tolerance = 1e-9)
})

# --- C3: the time until threshold of the interactions ------------------------

test_that("the time until threshold costs no extra simulation where it is free (C3)", {
  DT <- data.frame(Drug = c("lamotrigine", "valproate", "phenytoin"), Time = c(0, 0, 720),
                   Dose = c(100, 500, 300), Units = c("mg PO bid", "mg PO DR bid", "mg PO ER qd"),
                   stringsAsFactors = FALSE)
  calls <- function(plotRecovery, defaults = G) {
    seen <- character(0)
    original <- asmSimulateVictim
    local_mocked_bindings(asmSimulateVictim = function(PK, ...) {
      seen <<- c(seen, PK$drug)
      original(PK, ...)
    })
    out <- simulatedFor(DT, 7 * 1440, plotRecovery = plotRecovery, asm = TRUE, defaults = defaults)
    list(seen = table(seen), out = out)
  }
  without <- calls(FALSE)
  with <- calls(TRUE)
  # No threshold on any of them, and phenytoin's engine computes none: no
  # final pass (at 59094cf, one more simulation of each)
  expect_identical(with$seen, without$seen)
  # A threshold edited onto lamotrigine is solved in one final pass of its own
  edited <- G
  edited$endCe[edited$Drug == "lamotrigine"] <- 2
  withThreshold <- calls(TRUE, edited)
  expect_identical(as.vector(withThreshold$seen[c("phenytoin", "valproate")]),
                   as.vector(without$seen[c("phenytoin", "valproate")]))
  expect_identical(withThreshold$seen[["lamotrigine"]], without$seen[["lamotrigine"]] + 1L)
  expect_gt(max(withThreshold$out$lamotrigine$wide$Recovery, na.rm = TRUE), 0)

  # Each result is the simulation of its own final segments with the time
  # until threshold, as the final pass made it
  out <- with$out
  drugs <- recalculatePK(NULL, G, DT, age = 50, weight = 70, height = 170, sex = "male",
                         antiseizureInteractions = TRUE)
  maximum <- 7 * 1440
  for (v in c("lamotrigine", "valproate")) {
    PK <- asmSegmentPK(drugs[[v]], out[[v]]$interaction$CL)
    X <- simCpCe(DT[DT$Drug == v, ], noEvents, PK, maximum, TRUE)
    expect_identical(X$wide, out[[v]]$wide)
    expect_identical(X$results, out[[v]]$results)
  }
  seg <- out$phenytoin$interaction
  one <- function(p) if (is.null(seg[[p]])) data.frame(start = 0, value = 1) else seg[[p]]
  starts <- sort(unique(c(one("Vmax")$start, one("Km")$start)))
  value <- function(s) s$value[findInterval(starts, s$start)]
  PK <- drugs$phenytoin
  PK$interactionMM <- list(time = starts, vmax = value(one("Vmax")), km = value(one("Km")),
                           sample = asmInteractionGrid(maximum))
  X <- simCpCe(DT[DT$Drug == "phenytoin", ], noEvents, PK, maximum, TRUE)
  expect_identical(X$wide, out$phenytoin$wide)
  expect_identical(X$results, out$phenytoin$results)
})

# --- C3: the saturable engine's interaction switches -------------------------

test_that("phenytoin's interaction switches are points of its line, not knots, and it stays exact (C3)", {
  DT <- data.frame(Drug = c("phenytoin", "valproate"), Time = c(0, 0), Dose = c(300, 500),
                   Units = c("mg PO ER qd", "mg PO DR bid"), stringsAsFactors = FALSE)
  maximum <- 14 * 1440
  captured <- NULL
  original <- asmSimulateVictim
  local_mocked_bindings(asmSimulateVictim = function(PK, multipliers, time, DT, ET, maximum,
                                                     plotRecovery) {
    X <- original(PK, multipliers, time, DT, ET, maximum, plotRecovery)
    if (PK$drug == "phenytoin")
      captured <<- list(PK = PK, multipliers = multipliers, time = time, DT = DT, ET = ET)
    X
  })
  out <- simulatedFor(DT, maximum, asm = TRUE)
  seg <- out$phenytoin$interaction
  switches <- sort(unique(c(seg$Vmax$start, seg$Km$start)))
  expect_gt(length(switches), 20)
  line <- out$phenytoin$wide$Time
  # Every switch, and every time the interactions read phenytoin at, is a
  # point; no switch brings the instant before it (no dose falls there)
  expect_true(all(switches %in% line))
  expect_true(all(asmInteractionGrid(maximum) %in% line))
  doses <- c(0, 1440 * (0:13))
  lonely <- setdiff(switches[switches > 0], doses)
  expect_false(any((lonely - PRE_DOSE_OFFSET) %in% line))

  # Against the engine's second method, the fixed classical Runge-Kutta step
  # (held within its region of stability, 2 minutes at most), on the same
  # segments: within the adaptive step's tolerance (at 59094cf, with the
  # switches as knots, the error was of the same size)
  PK <- captured$PK
  one <- function(p) if (is.null(seg[[p]])) data.frame(start = 0, value = 1) else seg[[p]]
  value <- function(s) s$value[findInterval(switches, s$start)]
  PK$interactionMM <- list(time = switches, vmax = value(one("Vmax")), km = value(one("Km")),
                           sample = captured$time)
  adaptive <- simCpCe(captured$DT, captured$ET, PK, maximum, FALSE)$wide
  expect_identical(adaptive, out$phenytoin$wide)
  PK$michaelisMenten$adaptiveStep <- FALSE
  fixed <- simCpCe(captured$DT, captured$ET, PK, maximum, FALSE)$wide
  expect_identical(fixed$Time, adaptive$Time)
  big <- fixed$Plasma > 0.05 * max(fixed$Plasma)
  expect_lt(max(abs(adaptive$Plasma - fixed$Plasma)[big] / fixed$Plasma[big]), 1e-6)
})

# --- C3: the derivative's frame ----------------------------------------------

test_that("the saturable engine's derivative reads a small frame and keeps its byte code (C3)", {
  # Each simulation's derivative, as the adaptive step receives it
  seen <- list()
  original <- mmDormandPrince
  local_mocked_bindings(mmDormandPrince = function(f, x, t0, t1, h, rtol, atol, clean = NULL) {
    if (length(seen) < run) seen[[run]] <<- get("deriv", envir = environment(f))
    original(f, x, t0, t1, h, rtol, atol, clean)
  })
  DT <- data.frame(Drug = "vildagliptin", Time = 0, Dose = 50, Units = "mg PO bid",
                   stringsAsFactors = FALSE)
  for (run in 1:2) simulatedFor(DT, 2880)
  deriv <- seen[[2]]
  # Its frame holds the run's constants, not the engine's 110 variables
  expect_lt(length(ls(environment(deriv), all.names = TRUE)), 45)
  expect_true(all(c("V", "ka", "rmaxP", "koffT") %in% ls(environment(deriv))))
  # Compiled in the second simulation as in the first: moved into a frame of
  # its own with environment<-, it lost its byte code and the JIT did not
  # compile it again, so every simulation after a process's first ran at
  # half speed
  compiled <- function(f) {
    utils::capture.output(code <- try(compiler::disassemble(f), silent = TRUE))
    !inherits(code, "try-error")
  }
  if (!compiled(advanceMichaelisMenten)) skip("the engine is not byte-compiled here")
  expect_true(compiled(seen[[1]]))
  expect_true(compiled(seen[[2]]))
})
