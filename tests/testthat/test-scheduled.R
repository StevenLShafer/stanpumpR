# Scheduled (repeating) doses: qd, bid, tid, qid (scheduled.R).  Drafted by
# Claude Code, 2026-10-07, at the request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

noEvents <- data.frame(Time = double(), Event = character())
doses <- function(Time, Dose, Units, drug = "cefazolin") {
  data.frame(Drug = drug, Time = Time, Dose = Dose, Units = Units)
}

test_that("each frequency repeats at its interval out to the end of the plot", {
  for (f in names(SCHEDULE_INTERVALS)) {
    h <- SCHEDULE_INTERVALS[[f]]
    x <- expandScheduledDoses(doses(0, 1000, paste("mg", f)), 2 * MINS_PER_DAY)
    expect_equal(x$scheduled$Time, seq(h, 2 * MINS_PER_DAY - 1, by = h))
    expect_true(all(x$scheduled$Dose == 1000))
    expect_true(all(x$scheduled$Units == "mg"))
    # The dose to simulate: the first dose as an ordinary dose, then the repeats
    expect_equal(sort(x$dose$Time), seq(0, 2 * MINS_PER_DAY - 1, by = h))
    expect_true(all(x$dose$Units == "mg"))
  }
  expect_equal(unname(SCHEDULE_INTERVALS), c(24, 12, 8, 6) * 60)
})

test_that("the sequence starts at the entered time, not at zero", {
  x <- expandScheduledDoses(doses(90, 500, "mg PO bid", "metronidazole"), MINS_PER_DAY)
  expect_equal(sort(x$dose$Time), c(90, 810))
  expect_equal(x$scheduled$Time, 810)
  expect_equal(x$scheduled$Units, "mg PO")
})

test_that("a scheduled dose of 0 for the same route stops the sequence", {
  x <- expandScheduledDoses(
    doses(c(0, 1500), c(1000, 0), c("mg bid", "mg tid")),
    4 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(720, 1440))
})

test_that("a stop for one route leaves another route running", {
  x <- expandScheduledDoses(
    doses(c(0, 0, 600), c(100, 200, 0), c("mg bid", "mg PO bid", "mg/kg PO qd"), "clindamycin"),
    MINS_PER_DAY * 2
  )
  iv <- x$scheduled[x$scheduled$Units == "mg", ]
  po <- x$scheduled[x$scheduled$Units == "mg PO", ]
  expect_equal(iv$Time, c(720, 1440, 2160))
  expect_equal(nrow(po), 0)
})

test_that("an ordinary dose of 0 does not stop the sequence", {
  x <- expandScheduledDoses(doses(c(0, 300), c(1000, 0), c("mg qid", "mg")), MINS_PER_DAY)
  expect_equal(x$scheduled$Time, c(360, 720, 1080))
})

test_that("a new scheduled dose for the same route replaces the running one", {
  x <- expandScheduledDoses(
    doses(c(0, 1440), c(1000, 2000), c("mg tid", "mg bid")),
    3 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(480, 960, 2160, 2880, 3600))
  expect_equal(x$scheduled$Dose, c(1000, 1000, 2000, 2000, 2000))
  # The tid dose that would have fallen at 1440 is not given twice
  expect_equal(sum(x$dose$Time == 1440), 1)
})

test_that("two scheduled rows for one route at the same time: the last entered wins", {
  x <- expandScheduledDoses(doses(c(0, 0), c(1000, 2000), c("mg qd", "mg bid")), MINS_PER_DAY)
  expect_equal(x$dose$Dose, c(2000, 2000))
  expect_equal(x$dose$Time, c(0, 720))
})

test_that("a dose table without scheduled units passes through untouched", {
  d <- doses(c(0, 60), c(1000, 500), c("mg", "mg/hr"))
  x <- expandScheduledDoses(d, 120)
  expect_identical(x$dose, d)
  expect_null(x$scheduled)
})

test_that("simulating a bid dose equals simulating the doses written out", {
  PK <- getDrugPK("cefazolin", 70, 170, 50, "male", getDrugDefaults("cefazolin"))
  PK$endCe <- getDrugDefaults("cefazolin")$endCe
  max <- 2 * MINS_PER_DAY
  scheduled <- simCpCe(doses(0, 2000, "mg bid"), noEvents, PK, max, FALSE)
  explicit  <- simCpCe(doses(c(0, 720, 1440, 2160), 2000, "mg"), noEvents, PK, max, FALSE)
  expect_equal(scheduled$equiSpace$Ce, explicit$equiSpace$Ce)
  expect_equal(scheduled$max, explicit$max)
  expect_equal(scheduled$scheduled$Time, c(720, 1440, 2160))
  expect_null(explicit$scheduled)
})

test_that("an oral scheduled dose takes the oral route", {
  PK <- getDrugPK("cefalexin", 70, 170, 50, "male", getDrugDefaults("cefalexin"))
  PK$endCe <- getDrugDefaults("cefalexin")$endCe
  scheduled <- simCpCe(doses(0, 500, "mg PO qid", "cefalexin"), noEvents, PK, MINS_PER_DAY, FALSE)
  explicit  <- simCpCe(doses(c(0, 360, 720, 1080), 500, "mg PO", "cefalexin"), noEvents, PK, MINS_PER_DAY, FALSE)
  expect_equal(scheduled$equiSpace$Ce, explicit$equiSpace$Ce)
})

test_that("the repeats are merged into the exported dose table", {
  doseTable <- doses(c(0, 0), c(2000, 15), c("mg tid", "mg/kg"), c("cefazolin", "vancomycin"))
  drugs <- processdoseTable(
    doseTable, noEvents,
    recalculatePK(NULL, getDrugDefaultsGlobal(FALSE), doseTable, 50, 70, 170, "male"),
    MINS_PER_DAY, FALSE
  )
  expect_equal(drugs$cefazolin$scheduled$Time, c(480, 960))
  expect_null(drugs$vancomycin$scheduled)
  out <- exportDoseTable(doseTable, drugs)
  expect_equal(names(out), c("Drug", "Time", "Dose", "Units"))
  cz <- out[out$Drug == "cefazolin", ]
  expect_equal(cz$Time, c(0, 480, 960))
  expect_equal(cz$Units, c("mg tid", "mg", "mg"))
  expect_equal(nrow(out[out$Drug == "vancomycin", ]), 1)

  # Shown in another time unit, the same table with that unit beside the
  # minutes: 480 min is a third of a day.  Minutes, the default, add nothing.
  expect_identical(exportDoseTable(doseTable, drugs, timeUnit = "minutes"), out)
  inDays <- exportDoseTable(doseTable, drugs, timeUnit = "days")
  expect_equal(names(inDays), c("Drug", "Time", "Time (days)", "Dose", "Units"))
  expect_equal(inDays[, names(out)], out)
  expect_equal(inDays$`Time (days)`[inDays$Drug == "cefazolin"], c(0, 1, 2) / 3)

  # ...and when nothing is merged in, so the typed rows are returned as they are
  plain <- doses(c(0, 90), c(1000, 1000), c("mg", "mg"))
  inHours <- exportDoseTable(plain, list(), timeUnit = "hours")
  expect_equal(names(inHours), c("Drug", "Time", "Time (hours)", "Dose", "Units"))
  expect_equal(inHours$`Time (hours)`, c(0, 1.5))
  expect_identical(exportDoseTable(plain, list()), plain)
})

test_that("the scheduled units are offered and validated", {
  units <- unlist(getDrugDefaultsGlobal()$Units)
  offered <- units[grepl(" (qd|bid|tid|qid)$", units)]
  expect_gt(length(offered), 0)
  expect_true(all(offered %in% scheduledUnits))
  expect_true(all(nchar(scheduledUnits) <= MAX_UNIT_STRING_LENGTH))
  expect_true("mg PO bid" %in% getDrugDefaults("cefalexin")$Units[[1]])
  expect_false("mg bid" %in% getDrugDefaults("propofol")$Units[[1]])
  expect_true(validateDoseTableInput(doses("0", "1", "g qid")))
  expect_equal(doseRoute(c("mg bid", "mg/kg PO qd", "mg IM tid", "mcg IN qid")),
               c("IV", "PO", "IM", "IN"))
})
