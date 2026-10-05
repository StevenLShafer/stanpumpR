# "Time until threshold" in the three intravenous engines, checked against the
# definition: stop all delivery at time t, simulate on, and see when the effect
# site finally comes down through the threshold.
#
# This is the test that the feature lacked.  It is how the defects fixed on
# 2026-10-05 were found: no time reported while the effect site was still
# climbing after a bolus or an oral dose, and the time-varying-PK engine timing
# the plasma instead of the effect site.
#
# (Claude Code, Claude Fable 5.1, 2026-10-05; run on R 4.6.1.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

pkFor <- function(drug, weight = 70, height = 170, age = 50, sex = "male") {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK(drug = drug, weight = weight, height = height, age = age,
                  sex = sex, drugDefaults = dd[dd$Drug == drug, ])
  PK$endCe <- dd$endCe[dd$Drug == drug]
  PK
}

# Stop every infusion at t, give nothing after it, and time the effect site.
bruteRecovery <- function(DT, ET, PK, t, horizon = 720) {
  d <- DT[DT$Time <= t, , drop = FALSE]
  for (u in unique(d$Units[grepl("min|hr", d$Units)]))
    d <- rbind(d, data.frame(Drug = d$Drug[1], Time = t, Dose = 0, Units = u))
  r <- simCpCe(d[order(d$Time), ], ET, PK, t + horizon, FALSE)$results
  ce <- r[r$Site == "Effect Site" & r$Time >= t, ]
  above <- which(ce$Y > PK$endCe)
  if (length(above) == 0) return(0)
  i <- max(above)
  ce$Time[i] + (PK$endCe - ce$Y[i]) * (ce$Time[i + 1] - ce$Time[i]) /
    (ce$Y[i + 1] - ce$Y[i]) - t
}

# The value the app would show at t.  Taken from the grid point at or just
# before t, since the curve jumps at every dose.
shown <- function(sim, t) {
  es <- sim$equiSpace
  es$Recovery[max(which(es$Time <= t + 1e-9))]
}

# `tolerance` is in MINUTES.  The plotted series is the engine's own time line
# interpolated onto 100 points, so where the curve has a corner -- reaching zero
# as the effect site crosses the threshold -- it can be out by up to the spacing
# of the points either side.
expectMatchesBrute <- function(DT, ET, PK, maximum, tolerance = 1) {
  sim <- simCpCe(DT, ET, PK, maximum, TRUE)
  grid <- sim$equiSpace$Time
  # Sample the plotted grid, skipping points within a grid step after a dose,
  # where the plotted line is joining two sides of a jump.
  step <- grid[2] - grid[1]
  ok <- vapply(grid, function(t) all(t - DT$Time < 0 | t - DT$Time > step), logical(1))
  for (t in grid[ok][seq(2, sum(ok), length.out = 12)]) {
    expect_lt(abs(shown(sim, t) - bruteRecovery(DT, ET, PK, t)), tolerance,
              label = paste(PK$drug, "at", round(t, 1), "min: difference in minutes"))
  }
}


test_that("bolus plus infusion: the plain intravenous engine matches stopping delivery", {
  DT <- data.frame(Drug = "propofol", Time = c(0, 0, 60), Dose = c(140, 100, 0),
                   Units = c("mg", "mcg/kg/min", "mcg/kg/min"))
  expectMatchesBrute(DT, noEvents, pkFor("propofol"), 120)

  DT <- data.frame(Drug = "fentanyl", Time = c(0, 30), Dose = c(100, 50), Units = "mcg")
  expectMatchesBrute(DT, noEvents, pkFor("fentanyl"), 90)
})


test_that("a time is reported while the effect site is still climbing after a bolus", {
  PK <- pkFor("fentanyl")
  DT <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  # A ten-minute simulation, so that the plotted grid is fine enough to catch
  # the first half minute.
  sim <- simCpCe(DT, noEvents, PK, 10, TRUE)
  es <- sim$equiSpace
  early <- es[es$Time > 0 & es$Time <= 1.5, ]
  # Still below the threshold on the way up for part of this window...
  expect_true(any(early$Ce < PK$endCe))
  # ...but it is going above it, so the time until threshold is not zero.
  expect_true(all(early$Recovery > 10))
})


test_that("an oral dose: time is reported through the absorption phase", {
  PK <- pkFor("oxycodone")
  DT <- data.frame(Drug = "oxycodone", Time = c(0, 240), Dose = c(10, 10), Units = "mg PO")
  expectMatchesBrute(DT, noEvents, PK, 480, tolerance = 6)   # 4.8 min grid

  sim <- simCpCe(DT, noEvents, PK, 480, TRUE)
  es <- sim$equiSpace
  rising <- es[es$Time > 0 & es$Ce < PK$endCe & es$Time < 25, ]
  expect_gt(nrow(rising), 0)
  expect_true(all(rising$Recovery > 100))
})


test_that("time-varying PK: the effect site is timed, not the plasma", {
  # Dexmedetomidine in an infant (age <= 1) switches PK on cardiopulmonary bypass, which
  # routes it through advanceClosedForm1().
  PK <- pkFor("dexmedetomidine", weight = 7, height = 65, age = 0.5)
  skip_if(length(PK$pkEvents) < 2, "dexmedetomidine has no PK events for this patient")
  ET <- data.frame(Time = 30, Event = "CPB Start")
  DT <- data.frame(Drug = "dexmedetomidine", Time = c(0, 0), Dose = c(1, 0.7),
                   Units = c("mcg/kg", "mcg/kg/hr"))
  sim <- simCpCe(DT, ET, PK, 120, TRUE)
  expect_gt(max(sim$equiSpace$Recovery), 0)
  # After the last change in PK, so that "the PK in force now" is also the PK
  # the brute-force run goes on using.
  for (t in c(45, 60, 90, 110)) {
    # Within 3%, or three quarters of a minute where the time itself is short:
    # early on it is a minute or two and the resolution of the brute-force run
    # is most of the difference; on bypass, with clearance a fraction of what it
    # was, it runs to hours.
    want <- bruteRecovery(DT, ET, PK, t, horizon = 1440)
    expect_lt(abs(shown(sim, t) - want), max(0.75, 0.03 * want),
              label = paste("at", t, "min: difference in minutes"))
  }
})
