# "Stop" in the plot's dose dialogs (R/stop-administration.R).  Drafted by
# Claude Code, 2026-10-24, at the request of Steven L. Shafer.

minutes <- timeFormat("minutes", "relative")

table <- function(...) {
  rows <- list(...)
  dt <- data.frame(
    Drug  = vapply(rows, `[[`, "", 1),
    Time  = vapply(rows, `[[`, "", 2),
    Dose  = vapply(rows, `[[`, "", 3),
    Units = vapply(rows, `[[`, "", 4)
  )
  rbind(dt, doseTableNewRow, doseTableNewRow)
}

running <- function(dt, drug, time, maximum = 600) {
  ongoingAdministration(dt, drug, time, maximum, minutes)
}

test_that("a running infusion is found, and a 0 rate row stops it", {
  dt <- table(c("propofol", "0", "100", "mcg/kg/min"))
  r <- running(dt, "propofol", 30)
  expect_equal(r$kind, "infusion")
  expect_equal(r$units, "mcg/kg/min")

  s <- stopAdministration(dt, "propofol", 30, 600, minutes)
  added <- s$table[s$table$Drug == "propofol" & s$table$Time == "30", ]
  expect_equal(nrow(added), 1)
  expect_equal(added$Dose, "0")
  expect_equal(added$Units, "mcg/kg/min")
  # the table still ends in a blank row, and nothing runs after the stop
  expect_equal(s$table$Drug[nrow(s$table)], "")
  expect_equal(nrow(running(s$table, "propofol", 31)), 0)
  # before the stop it was still running
  expect_equal(nrow(running(s$table, "propofol", 29)), 1)
})

test_that("the stop is what the engine reads: the rate is zero after it", {
  dt <- table(c("propofol", "0", "100", "mcg/kg/min"))
  s <- stopAdministration(dt, "propofol", 30, 120, minutes)$table
  d <- s[s$Drug == "propofol", ]
  d <- data.frame(Time = as.numeric(d$Time), Dose = as.numeric(d$Dose), Bolus = FALSE)
  lines <- doseLines(d, c(0, 10, 29, 30, 31, 60))
  expect_equal(lines$infusion, c(100, 100, 100, 0, 0, 0))
})

test_that("nothing to stop: no rows, stopped rates, a bolus, the time before a start", {
  expect_equal(nrow(running(table(c("fentanyl", "0", "100", "mcg")), "fentanyl", 30)), 0)
  stopped <- table(c("propofol", "0", "100", "mcg/kg/min"), c("propofol", "20", "0", "mcg/kg/min"))
  expect_equal(nrow(running(stopped, "propofol", 30)), 0)
  expect_equal(nrow(running(table(c("propofol", "40", "100", "mcg/kg/min")), "propofol", 30)), 0)
  expect_equal(nrow(running(table(c("propofol", "0", "100", "mcg/kg/min")), "fentanyl", 30)), 0)
  # at or past the end of the plot there is nothing left to stop
  expect_equal(nrow(running(table(c("propofol", "0", "100", "mcg/kg/min")), "propofol", 600)), 0)
  expect_null(stopAdministration(stopped, "propofol", 30, 600, minutes))
})

test_that("rates at one time are summed, and the latest time is what runs", {
  restarted <- table(c("propofol", "0", "100", "mcg/kg/min"), c("propofol", "20", "0", "mcg/kg/min"),
                     c("propofol", "25", "5", "mg/hr"))
  r <- running(restarted, "propofol", 30)
  expect_equal(r$units, "mg/hr")
})

test_that("a TCI target is stopped with a target of 0, and replaces a manual rate", {
  dt <- table(c("propofol", "0", "100", "mcg/kg/min"), c("propofol", "10", "3", TCI_UNIT_EFFECT))
  r <- running(dt, "propofol", 30)
  expect_equal(r$kind, "tci")
  expect_equal(r$units, TCI_UNIT_EFFECT)
  s <- stopAdministration(dt, "propofol", 30, 600, minutes)$table
  expect_true(any(s$Units == TCI_UNIT_EFFECT & s$Dose == "0" & s$Time == "30"))
  # a target of 0 already entered: nothing runs
  off <- table(c("propofol", "10", "3", TCI_UNIT_PLASMA), c("propofol", "20", "0", TCI_UNIT_PLASMA))
  expect_equal(nrow(running(off, "propofol", 30)), 0)
  # a manual rate after the target replaces it
  manual <- table(c("propofol", "10", "3", TCI_UNIT_PLASMA), c("propofol", "20", "50", "mcg/kg/min"))
  expect_equal(running(manual, "propofol", 30)$kind, "infusion")
})

test_that("a repeating dose is stopped with a scheduled 0 in its own unit", {
  unit <- "mg PO qid"
  dt <- table(c("morphine", "0", "10", unit))
  r <- running(dt, "morphine", 400, maximum = 2880)
  expect_equal(r$kind, "schedule")
  expect_equal(r$units, unit)
  s <- stopAdministration(dt, "morphine", 400, 2880, minutes)$table
  m <- s[s$Drug == "morphine", ]
  expect_equal(m$Dose[m$Time == "400"], "0")
  # the repeats after the stop are gone
  d <- data.frame(Drug = "morphine", Time = as.numeric(m$Time), Dose = as.numeric(m$Dose), Units = m$Units)
  x <- expandScheduledDoses(d, 2880)
  expect_true(all(x$scheduled$Time < 400))
  # before the stop the repeats were given
  expect_equal(nrow(running(s, "morphine", 300, maximum = 2880)), 1)
  # a schedule whose repeats all fall after the end of the plot: nothing left
  expect_equal(nrow(running(dt, "morphine", 400, maximum = 401)), 0)
})

test_that("an intravenous scheduled dose is stopped on its own route", {
  unit <- "mg bid"
  dt <- table(c("cefazolin", "0", "2000", unit))
  r <- running(dt, "cefazolin", 100, maximum = 2880)
  expect_equal(r$units, unit)
})

test_that("an inhaled agent's setting is turned off with a 0", {
  dt <- table(c("sevoflurane", "0", "2", "%"))
  r <- running(dt, "sevoflurane", 30)
  expect_equal(r$kind, "setting")
  expect_equal(r$label, "the vaporizer")
  s <- stopAdministration(dt, "sevoflurane", 30, 600, minutes)$table
  expect_true(any(s$Drug == "sevoflurane" & s$Dose == "0" & s$Units == "%" & s$Time == "30"))
})

test_that("several things running are all stopped, in the table's format", {
  dt <- table(c("cefazolin", "0", "100", "mg/hr"), c("cefazolin", "0", "2000", "mg bid"))
  hours <- timeFormat("hours", "relative")
  s <- stopAdministration(dt, "cefazolin", 90, 2880, hours)
  expect_setequal(s$stopped$kind, c("infusion", "schedule"))
  expect_true(all(s$table$Time[s$table$Dose == "0"] == "1.5"))
})

test_that("stoppedText joins labels as a sentence", {
  expect_equal(stoppedText("the infusion (mg/hr)"), "the infusion (mg/hr)")
  expect_equal(stoppedText(c("a", "b")), "a and b")
  expect_equal(stoppedText(c("a", "b", "c")), "a, b and c")
})
