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

test_that("an ordinary dose of 0 does not stop an intravenous sequence", {
  x <- expandScheduledDoses(doses(c(0, 300), c(1000, 0), c("mg qid", "mg")), MINS_PER_DAY)
  expect_equal(x$scheduled$Time, c(360, 720, 1080))
})

# Oral dosing (Shafer, 2026-10-24): any later oral dose ends a running oral
# schedule.
test_that("an oral bid row entered on a tid schedule replaces it", {
  x <- expandScheduledDoses(
    doses(c(0, 1440), c(200, 400), c("mg PO tid", "mg PO bid"), "carbamazepine"),
    3 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(480, 960, 2160, 2880, 3600))
  expect_equal(x$scheduled$Dose, c(200, 200, 400, 400, 400))
  expect_equal(sort(x$dose$Time), c(0, 480, 960, 1440, 2160, 2880, 3600))
})

test_that("an ordinary oral dose of 0 stops an oral schedule", {
  x <- expandScheduledDoses(
    doses(c(0, 1440), c(200, 0), c("mg PO tid", "mg PO"), "carbamazepine"),
    3 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(480, 960))
})

test_that("an ordinary non-zero oral dose ends an oral schedule too", {
  x <- expandScheduledDoses(
    doses(c(0, 1000), c(200, 600), c("mg PO tid", "mg PO"), "carbamazepine"),
    3 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(480, 960))
  expect_equal(sort(x$dose$Time), c(0, 480, 960, 1000))
})

test_that("any oral form ends an oral schedule of another form", {
  x <- expandScheduledDoses(
    doses(c(0, 1440), c(100, 0), c("mg PO tablet tid", "mg PO ER"), "phenytoin"),
    3 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(480, 960))
})

test_that("an oral dose at the schedule's first dose does not end it", {
  x <- expandScheduledDoses(
    doses(c(0, 0), c(400, 200), c("mg PO", "mg PO bid"), "carbamazepine"),
    2 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(720, 1440, 2160))
})

test_that("oral and sublingual doses end each other's schedules, and their own", {
  # No drug offers both today (buprenorphine is the only SL drug): the rule
  # reads the units, so a made-up drug carries it.
  x <- expandScheduledDoses(
    doses(c(0, 1000), c(200, 0), c("mg PO tid", "mg SL"), "made-up"),
    3 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(480, 960))
  x <- expandScheduledDoses(
    doses(c(0, 1000), c(200, 400), c("mg PO tid", "mg SL bid"), "made-up"),
    2 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time[x$scheduled$Units == "mg PO"], c(480, 960))
  expect_equal(x$scheduled$Time[x$scheduled$Units == "mg SL"], c(1720, 2440))
  # An oral dose, 0 or not, ends a sublingual schedule
  x <- expandScheduledDoses(
    doses(c(0, 700), c(2, 0), c("mg SL qid", "mg PO"), "made-up"),
    MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, 360)
  x <- expandScheduledDoses(
    doses(c(0, 700), c(2, 200), c("mg SL qid", "mg PO bid"), "made-up"),
    2 * MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time[x$scheduled$Units == "mg SL"], 360)
  expect_equal(x$scheduled$Time[x$scheduled$Units == "mg PO"], c(1420, 2140, 2860))
  # An ordinary sublingual dose ends a sublingual schedule too: 0 stops it,
  # and an extra dose is given once with no schedule after it
  x <- expandScheduledDoses(
    doses(c(0, 700), c(2, 0), c("mg SL qid", "mg SL"), "buprenorphine"),
    MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, 360)
  x <- expandScheduledDoses(
    doses(c(0, 700), c(2, 4), c("mg SL qid", "mg SL"), "buprenorphine"),
    MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, 360)
  expect_equal(sort(x$dose$Time), c(0, 360, 700))
  # ...but not one taken with the schedule's first dose
  x <- expandScheduledDoses(
    doses(c(0, 0), c(4, 2), c("mg SL", "mg SL qid"), "buprenorphine"),
    MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(360, 720, 1080))
})

test_that("an oral or sublingual row with no readable time ends no schedule", {
  x <- expandScheduledDoses(
    doses(c("0", "abc", NA), c(200, 0, 0), c("mg PO tid", "mg SL", "mg PO"), "made-up"),
    MINS_PER_DAY
  )
  expect_equal(x$scheduled$Time, c(480, 960))
})

test_that("an oral dose leaves other routes' schedules running", {
  x <- expandScheduledDoses(
    doses(c(0, 0, 600), c(100, 200, 0), c("mg bid", "mg PO bid", "mg PO"), "clindamycin"),
    MINS_PER_DAY * 2
  )
  expect_equal(x$scheduled$Time[x$scheduled$Units == "mg"], c(720, 1440, 2160))
  expect_equal(sum(x$scheduled$Units == "mg PO"), 0)
})

test_that("a repeat a rounding hair before the row that ends it is not given", {
  # 8 h written in days reads back as 480 +/- 0.001 min
  x <- expandScheduledDoses(
    doses(c(0, 480.0004), c(200, 0), c("mg PO tid", "mg PO"), "carbamazepine"),
    MINS_PER_DAY
  )
  expect_null(x$scheduled)
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

# Skip next dose (skipNextOralDose()): the dialog's button, as a dose table
# rewrite.
skipTable <- function(Time, Dose, Units, drug = "carbamazepine") {
  dt <- data.frame(Drug = c(rep(drug, length(Time)), ""),
                   Time = c(as.character(Time), ""),
                   Dose = c(as.character(Dose), ""),
                   Units = c(Units, ""))
  dt
}
minutesFormat <- timeFormat("minutes", "relative")
cbzUnits <- c("mg PO", "mg PO ER", "mg PO bid", "mg PO tid", "mg PO ER qd", "mg PO ER bid")
simulatedTimes <- function(dt, maximum) {
  d <- cleanDoseTable(dt)
  d$Time <- as.numeric(d$Time)
  x <- expandScheduledDoses(d, maximum)$dose
  sort(x$Time[x$Dose > 0])
}

test_that("skip next dose drops exactly the next oral dose and resumes the schedule", {
  dt <- skipTable(0, 200, "mg PO tid")
  skip <- skipNextOralDose(dt, "carbamazepine", 500, 3 * MINS_PER_DAY, minutesFormat, units = cbzUnits)
  expect_equal(skip$time, 960)
  expect_equal(skip$resumes, 1440)
  expect_equal(skip$table$Time[1:3], c("0", "960", "1440"))
  expect_equal(skip$table$Dose[1:3], c("200", "0", "200"))
  expect_equal(skip$table$Units[1:3], c("mg PO tid", "mg PO", "mg PO tid"))
  expect_equal(skip$table$Drug[4], "")
  expect_equal(simulatedTimes(skip$table, 3 * MINS_PER_DAY),
               setdiff(seq(0, 3 * MINS_PER_DAY - 1, by = 480), 960))
})

test_that("skipping a schedule's first dose turns that row into the 0 dose", {
  dt <- skipTable(c(0, 1440), c(200, 400), c("mg PO tid", "mg PO bid"))
  skip <- skipNextOralDose(dt, "carbamazepine", 1000, 3 * MINS_PER_DAY, minutesFormat, units = cbzUnits)
  expect_equal(skip$time, 1440)
  expect_equal(skip$resumes, 2160)
  expect_equal(simulatedTimes(skip$table, 3 * MINS_PER_DAY), c(0, 480, 960, 2160, 2880, 3600))
})

test_that("no restart is written when the next row would end the schedule first", {
  dt <- skipTable(c(0, 1000), c(200, 0), c("mg PO tid", "mg PO"))
  skip <- skipNextOralDose(dt, "carbamazepine", 500, 3 * MINS_PER_DAY, minutesFormat, units = cbzUnits)
  expect_equal(skip$time, 960)
  expect_true(is.na(skip$resumes))
  expect_equal(nrow(skip$table), nrow(dt) + 1)
  expect_equal(simulatedTimes(skip$table, 3 * MINS_PER_DAY), c(0, 480))
})

test_that("skip next dose works on a sublingual schedule", {
  bupUnits <- c("mg SL", "mcg SL", "mg SL bid", "mg SL tid", "mg SL qid")
  dt <- skipTable(0, 2, "mg SL tid", "buprenorphine")
  skip <- skipNextOralDose(dt, "buprenorphine", 500, 3 * MINS_PER_DAY, minutesFormat, units = bupUnits)
  expect_equal(skip$time, 960)
  expect_equal(skip$resumes, 1440)
  expect_equal(skip$table$Time[1:3], c("0", "960", "1440"))
  expect_equal(skip$table$Dose[1:3], c("2", "0", "2"))
  expect_equal(skip$table$Units[1:3], c("mg SL tid", "mg SL", "mg SL tid"))
  expect_equal(simulatedTimes(skip$table, 3 * MINS_PER_DAY),
               setdiff(seq(0, 3 * MINS_PER_DAY - 1, by = 480), 960))
})

test_that("skip next dose works on an intramuscular schedule, with a scheduled 0", {
  # An ordinary 0 does not stop an IM schedule, so the skip writes the
  # schedule's own unit, even when the drug offers the base unit
  imUnits <- c("mg", "mg IM", "mg IM qid")
  dt <- skipTable(0, 1, "mg IM qid", "hydromorphone")
  skip <- skipNextOralDose(dt, "hydromorphone", 100, 2 * MINS_PER_DAY, minutesFormat, units = imUnits)
  expect_equal(skip$time, 360)
  expect_equal(skip$resumes, 720)
  expect_equal(skip$table$Time[1:3], c("0", "360", "720"))
  expect_equal(skip$table$Dose[1:3], c("1", "0", "1"))
  expect_equal(skip$table$Units[1:3], c("mg IM qid", "mg IM qid", "mg IM qid"))
  expect_equal(simulatedTimes(skip$table, 2 * MINS_PER_DAY),
               setdiff(seq(0, 2 * MINS_PER_DAY - 1, by = 360), 360))
  # The first dose: that row becomes the scheduled 0
  skip <- skipNextOralDose(dt, "hydromorphone", 0, 2 * MINS_PER_DAY, minutesFormat, units = imUnits)
  expect_equal(skip$table$Units[1:2], c("mg IM qid", "mg IM qid"))
  expect_equal(skip$table$Dose[1:2], c("0", "1"))
  expect_equal(simulatedTimes(skip$table, 2 * MINS_PER_DAY), seq(360, 2 * MINS_PER_DAY - 1, by = 360))
})

test_that("skip next dose is not offered without a running oral schedule", {
  expect_null(skipNextOralDose(skipTable(0, 200, "mg PO"), "carbamazepine", 0, MINS_PER_DAY, minutesFormat))
  expect_null(skipNextOralDose(skipTable(0, 1000, "mg bid", "cefazolin"), "cefazolin", 0, MINS_PER_DAY, minutesFormat))
  # nor on an intranasal one
  expect_null(skipNextOralDose(skipTable(0, 4, "mcg IN qid", "fentanyl"), "fentanyl", 0, MINS_PER_DAY, minutesFormat))
  expect_null(skipNextOralDose(skipTable(0, 200, "mg PO tid"), "levetiracetam", 0, MINS_PER_DAY, minutesFormat))
  # Stopped before the click, or the next dose past the end of the plot
  expect_null(skipNextOralDose(skipTable(c(0, 600), c(200, 0), c("mg PO tid", "mg PO")),
                               "carbamazepine", 700, MINS_PER_DAY, minutesFormat))
  expect_null(skipNextOralDose(skipTable(0, 200, "mg PO bid"), "carbamazepine", 1500, 1800, minutesFormat))
})

test_that("the 0 dose falls back to the scheduled unit when the base unit is not offered", {
  skip <- skipNextOralDose(skipTable(0, 500, "mg PO bid", "levetiracetam"), "levetiracetam", 100,
                           2 * MINS_PER_DAY, minutesFormat, units = c("mg PO bid"))
  expect_equal(skip$table$Units[2], "mg PO bid")
  expect_equal(skip$table$Dose[2], "0")
})

test_that("skip next dose writes times in the table's format and they read back exactly", {
  # Days: a tid repeat at a third of a day
  days <- timeFormat("days", "relative")
  dt <- skipTable(0, 200, "mg PO tid")
  skip <- skipNextOralDose(dt, "carbamazepine", 0, 2 * MINS_PER_DAY, days, units = cbzUnits)
  expect_equal(skip$time, 0)
  skip <- skipNextOralDose(dt, "carbamazepine", 400, 2 * MINS_PER_DAY, days, units = cbzUnits)
  expect_equal(skip$time, 480)
  back <- skip$table
  back$Time[nzchar(back$Drug)] <- displayTimeToMinutes(back$Time[nzchar(back$Drug)], unit = "days")
  expect_equal(simulatedTimes(back, 2 * MINS_PER_DAY),
               setdiff(seq(0, 2 * MINS_PER_DAY - 1, by = 480), 480))
  # Clock: the dialogs show the procedure start's clock
  clock <- timeFormat("hours", "clock")
  dt <- skipTable("08:00", 200, "mg PO bid")
  skip <- skipNextOralDose(dt, "carbamazepine", 60, 2 * MINS_PER_DAY, clock, reference = "08:00", units = cbzUnits)
  expect_equal(skip$time, 720)
  expect_equal(skip$table$Time[2:3], c("20:00", "24"))
})
