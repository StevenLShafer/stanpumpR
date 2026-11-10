# The Visualizer (3D viewer): the run file's test
#
# Author:    Dr Charles Minto
# Assisted by: Claude.ai (Anthropic)
# Extended for stanpumpR's pipeline by Claude Code (Anthropic), 2026-11-05.
#
# Copyright (c) 2026 Charles Minto and Thomas Schnider
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# viewerRun() reads the simulation's $wide (Plasma, Effect Site),
# $compartments (V2, V3, Depot), $tci$rates and $DT by name; if they change
# it draws a wrong picture rather than failing, so this test fails instead.
# The drug given is checked against the doses themselves, and the drug
# eliminated (given less the drug in the body) against an independent
# integral of CL1 x Cp, for the linear drugs where that is the elimination.

NO_EVENTS <- data.frame(Time = double(), Event = character())

# As the app does it: the plot's simulation (processdoseTable(), as drugs()
# runs it), then the drug's entry from it, re-solved with the plot's own PK
# only where its points are too far apart (viewerPlotEntry()).
plotSimulation <- function(doses, maximum, minimum = 0) {
  doses$Drug <- as.character(doses$Drug)
  doses$Units <- as.character(doses$Units)
  D <- recalculatePK(NULL, getDrugDefaultsGlobal(), doses,
                     age = 40, weight = 70, height = 170, sex = "male")
  processdoseTable(doses, NO_EVENTS, D, maximum, FALSE, plotMinimum = minimum)
}
viewerEntry <- function(doses, maximum, minimum = 0, drug = doses$Drug[1]) {
  doses$Drug <- as.character(doses$Drug)
  doses$Units <- as.character(doses$Units)
  plot <- plotSimulation(doses, maximum, minimum)
  viewerPlotEntry(drug, plot[[drug]], doses, NO_EVENTS, plot, minimum, maximum)
}
vr <- function(doses, maximum, minimum = 0) {
  viewerRun(viewerEntry(doses, maximum, minimum), minimum, maximum)
}

# Eliminated as the viewer computes it (given less the drug in the body), and
# as CL1 x Cp integrated by the trapezoid, at the end of the run.
eliminated <- function(r) {
  n <- length(r$t)
  body <- r$amountScale * (r$cp[n] * r$pk$v1 + (if (is.null(r$c2)) 0 else r$c2[n] * r$pk$v2) +
          (if (is.null(r$c3)) 0 else r$c3[n] * r$pk$v3))
  c(viewer = r$given[n] - body,
    integral = r$amountScale * r$pk$cl1 * sum((r$cp[-1] + r$cp[-n]) / 2 * diff(r$t) / 60))
}

TOL <- 1e-3

test_that("propofol TCI-E: every series is there and the drug adds up", {
  doses <- data.frame(Drug = "propofol", Time = c(0, 20, 40), Dose = c(3, 4, 2),
                      Units = "Effect site target")
  r <- vr(doses, 60)
  expect_equal(r$version, 2)
  expect_false(is.null(r$ce))
  expect_false(is.null(r$c2))
  expect_false(is.null(r$c3))
  expect_true(r$inputKnown)
  for (k in c("cp", "ce", "c2", "c3", "rate", "target", "mode", "given"))
    expect_length(r[[k]], length(r$t))
  expect_equal(r$mode[1], "effect")
  expect_gt(r$rate[1], 0)
  # Given is the TCI pump's own schedule, integrated.
  n <- length(r$t)
  expect_equal(r$given[n], sum(r$rate[-n] * diff(r$t) / 60), tolerance = 1e-9)
  e <- eliminated(r)
  expect_lt(abs(e[["viewer"]] - e[["integral"]]) / r$given[n], TOL)
})

test_that("a manual infusion takes over from TCI at 7 mg/min", {
  doses <- data.frame(Drug = "propofol", Time = c(0, 20), Dose = c(3, 6),
                      Units = c("Effect site target", "mg/kg/hr"))
  r <- vr(doses, 40)
  after <- r$t >= 1200
  expect_true(all(r$mode[after] == "manual"))
  expect_true(all(abs(r$rate[after] - 7) < 1e-9))
  e <- eliminated(r)
  expect_lt(abs(e[["viewer"]] - e[["integral"]]) / utils::tail(r$given, 1), TOL)
})

test_that("boluses and an infusion add up to the doses", {
  doses <- data.frame(Drug = "fentanyl", Time = c(0, 0, 30), Dose = c(100, 1, 50),
                      Units = c("mcg", "mcg/kg/hr", "mcg"))
  r <- vr(doses, 60)
  expect_length(r$boluses, 2)
  expect_equal(utils::tail(r$given, 1), 100 + 70 + 50, tolerance = 1e-9)
  e <- eliminated(r)
  expect_lt(abs(e[["viewer"]] - e[["integral"]]) / 220, TOL)
})

test_that("the run spans the plot's window, Min time to Max time", {
  doses <- data.frame(Drug = "propofol", Time = c(0, 0), Dose = c(2, 100),
                      Units = c("mg/kg", "mcg/kg/min"))
  r <- vr(doses, 780)
  expect_equal(r$t[1], 0)
  expect_equal(utils::tail(r$t, 1), 46800)
  expect_true(all(diff(r$t) > 0))
  expect_lte(length(r$t), VIEWER_MAX_TIMES + 2)

  # From Min time: the drug given before the window is already counted, and
  # the bolus at 0 is not shown as an event inside it.
  w <- vr(doses, 120, minimum = 30)
  expect_equal(w$t[1], 1800)
  expect_equal(utils::tail(w$t, 1), 7200)
  expect_equal(w$given[1], 140 + 7 * 30, tolerance = 1e-9)
  expect_length(w$boluses, 0)
  # The same solution as a run from 0, sampled in the window.
  full <- vr(doses, 120)
  expect_equal(w$cp, stats::approx(full$t, full$cp, w$t)$y, tolerance = 1e-6)
})

test_that("an oral dose enters through the depot and adds up", {
  doses <- data.frame(Drug = "morphine", Time = 0, Dose = 10, Units = "mg PO tablet")
  r <- vr(doses, 240)
  P <- getDrugPK("morphine", 70, 170, 40, "male", getDrugDefaults("morphine"))$PK[[1]]
  n <- length(r$t)
  expect_false(is.null(r$depot))
  expect_true(r$inputKnown)
  # Absorbed plus still in the depot is the bioavailable dose, after the lag.
  after <- r$t / 60 > P$tlag_PO + 0.1
  expect_equal(r$absorbed[after] + r$depot[after],
               rep(10 * P$bioavailability_PO, sum(after)), tolerance = 1e-3)
  # No intravenous rate, and the given is what was absorbed.
  expect_true(all(r$rate == 0))
  expect_equal(r$given, r$absorbed)
  e <- eliminated(r)
  expect_lt(abs(e[["viewer"]] - e[["integral"]]) / r$given[n], 5e-3)
})

test_that("scheduled intravenous doses count every repeat", {
  doses <- data.frame(Drug = "morphine", Time = 0, Dose = 4, Units = "mg q4h")
  r <- vr(doses, 600)
  expect_equal(utils::tail(r$given, 1), 4 * 3, tolerance = 1e-9)   # 0, 240, 480 min
  expect_length(r$boluses, 3)
})

test_that("a saturable drug: given is the doses, eliminated never falls", {
  doses <- data.frame(Drug = "phenytoin", Time = 0, Dose = 1000, Units = "mg")
  r <- vr(doses, 240)
  expect_true(r$inputKnown)
  # 1000 mg of phenytoin sodium enters as the acid (saltFactor$IV).
  salt <- getDrugPK("phenytoin", 70, 170, 40, "male",
                    getDrugDefaults("phenytoin"))$michaelisMenten$saltFactor$IV
  expect_equal(utils::tail(r$given, 1), 1000 * salt, tolerance = 1e-9)
  body <- r$cp * r$pk$v1 + (if (is.null(r$c2)) 0 else r$c2 * r$pk$v2)
  elim <- r$given - body
  expect_gte(min(elim), -1e-6 * 1000)
  expect_gte(min(diff(elim)), -1e-6 * 1000)
})

test_that("drugs the viewer cannot draw say why", {
  D <- recalculatePK(NULL, getDrugDefaultsGlobal(),
                     data.frame(Drug = c("ketorolac", "fluindione", "mannitol"), Time = 0,
                                Dose = c(30, 20, 50), Units = c("mg", "mg PO", "g")),
                     age = 40, weight = 70, height = 170, sex = "male")
  expect_match(viewerProblem(D$ketorolac), "separate systems")
  expect_match(viewerProblem(D$fluindione), "INR")
  expect_match(viewerProblem(D$mannitol), "osmolality")
  expect_match(viewerProblem(NULL), "no simulation")
  expect_null(viewerProblem(recalculatePK(NULL, getDrugDefaultsGlobal(),
    data.frame(Drug = "propofol", Time = 0, Dose = 1, Units = "mg/kg"),
    age = 40, weight = 70, height = 170, sex = "male")$propofol))
})

test_that("a target and a manual infusion at one time: the target wins", {
  # R/tci.R: when both are entered at the same time the target wins, and the
  # simulator zeroes the manual infusion (test-tci.R). The run must say TCI,
  # with its target, as its pump rate already does.
  both <- data.frame(Drug = "propofol", Time = c(0, 0), Dose = c(100, 3),
                     Units = c("mcg/kg/min", "Effect site target"))
  alone <- data.frame(Drug = "propofol", Time = 0, Dose = 3, Units = "Effect site target")
  r <- vr(both, 30)
  a <- vr(alone, 30)
  expect_true(all(r$mode == "effect"))
  expect_true(all(r$target == 3))
  expect_equal(r$rate, a$rate)
  expect_equal(r$given, a$given)
})

test_that("the plot's own points are used when they are close enough", {
  doses <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  plot <- plotSimulation(doses, 60)
  # The plot's time line has gaps of minutes: too coarse, so re-solved.
  expect_false(viewerGridFine(plot$fentanyl, 0, 60))
  expect_true(viewerEntry(doses, 60)$viewerResolved)
  # A plot dense enough is used as it is, nothing re-solved, and the run is
  # the plot's own numbers.
  fine <- viewerEntry(doses, 60)
  fine$viewerResolved <- NULL
  expect_true(viewerGridFine(fine, 0, 60))
  e <- viewerPlotEntry("fentanyl", fine, doses, NO_EVENTS, list(fentanyl = fine), 0, 60)
  expect_false(e$viewerResolved)
  expect_identical(e$wide, fine$wide)
})

test_that("an ACT drug is drawn as the anti-Xa activity its ACT comes from", {
  doses <- data.frame(Drug = "heparinCPB", Time = 0, Dose = 30000, Units = "IU")
  plot <- plotSimulation(doses, 120)
  r <- vr(doses, 120)
  expect_equal(r$units$conc, "IU/mL")
  expect_equal(r$units$amount, "IU")
  expect_equal(r$amountScale, 1000)
  expect_false(is.null(r$c2))
  # The plot's ACT, put back through its Emax link, is the run's activity.
  act <- plot$heparinCPB$act
  w <- plot$heparinCPB$wide
  w <- w[w$Time > 1 & w$Time < 119, ]
  x <- w$Plasma - act$act0
  cp <- act$c50 * x / (act$emax - x)
  expect_equal(stats::approx(r$t / 60, r$cp, w$Time)$y, cp, tolerance = 1e-3)
  # The drug given is the dose, in IU, and the body holds what the
  # concentrations and volumes say.
  expect_equal(utils::tail(r$given, 1), 30000, tolerance = 1e-9)
  n <- length(r$t)
  body <- r$amountScale * (r$cp[n] * r$pk$v1 + r$c2[n] * r$pk$v2)
  expect_gt(body, 0)
  expect_lt(body, 30000)
})

test_that("drugs in IU, U and mmol: the amounts are in IU, U and mmol", {
  r <- vr(data.frame(Drug = "heparin", Time = 0, Dose = 5000, Units = "IU"), 60)
  expect_equal(r$units$amount, "IU")
  expect_equal(utils::tail(r$given, 1), 5000, tolerance = 1e-9)
  e <- eliminated(r)
  expect_lt(abs(e[["viewer"]] - e[["integral"]]) / 5000, 1e-3)
  v <- vr(data.frame(Drug = "vasopressin", Time = 0, Dose = 0.04, Units = "U/min"), 60)
  expect_equal(v$units$amount, "U")
  expect_equal(utils::tail(v$given, 1), 0.04 * 60, tolerance = 1e-9)
  # The vasopressin in the body, in U, is its concentration (mcU/mL = mU/L)
  # times the volume, over 1000.
  n <- length(v$t)
  expect_equal(v$amountScale, 1e-3)
  expect_lt(v$amountScale * v$cp[n] * v$pk$v1, 0.04 * 60)
  # Calcium is dosed as a salt: drawn, with the drug given unknown.
  ca <- vr(data.frame(Drug = "calcium", Time = 0, Dose = 1, Units = "g chloride"), 60)
  expect_equal(ca$units$conc, "mmol/L")
  expect_false(ca$inputKnown)
  expect_null(ca$given)
})

test_that("phenytoin sodium IV: the salt's mass is not counted as eliminated", {
  # advanceMichaelisMenten() enters an IV dose of the sodium salt as the acid,
  # x saltFactor$IV; the drug given must be on the same basis.
  doses <- data.frame(Drug = "phenytoin", Time = 0, Dose = 1000, Units = "mg")
  r <- vr(doses, 60)
  salt <- getDrugPK("phenytoin", 70, 170, 40, "male",
                    getDrugDefaults("phenytoin"))$michaelisMenten$saltFactor$IV
  expect_lt(salt, 1)
  expect_true(r$inputKnown)
  expect_equal(r$given[1], 1000 * salt, tolerance = 1e-9)
  body0 <- r$amountScale * (r$cp[1] * r$pk$v1 + (if (is.null(r$c2)) 0 else r$c2[1] * r$pk$v2))
  # At the dose, nothing is eliminated yet.
  expect_lt(abs(r$given[1] - body0) / r$given[1], 1e-3)
})

test_that("fosphenytoin (mg PE): the prodrug being converted leaves the drug given unknown", {
  # A PE dose waits in a conversion compartment that is neither the body nor
  # the Depot column, so the viewer cannot balance it; it says so instead of
  # showing the whole dose as eliminated.
  doses <- data.frame(Drug = "phenytoin", Time = 0, Dose = 1000, Units = "mg PE")
  r <- vr(doses, 60)
  expect_false(r$inputKnown)
  expect_null(r$given)
})

# Gemini's audit of 293e168 (2026-11-06): each case reproduced before the fix.

test_that("heparinCPB's height scale is the anti-Xa activity its typical ACT stands for", {
  r <- vr(data.frame(Drug = "heparinCPB", Time = 0, Dose = 30000, Units = "IU"), 120)
  act <- getDrugPK("heparinCPB", 70, 170, 40, "male", getDrugDefaults("heparinCPB"))$act
  # Not the ACT in seconds (480 before the fix).
  expect_lt(r$typical, 20)
  expect_equal(act$act0 + act$emax * r$typical / (act$c50 + r$typical),
               getDrugDefaults("heparinCPB")$Typical, tolerance = 1e-9)
})

test_that("a target with a manual infusion and a bolus at one time: the pump is the TCI's", {
  doses <- data.frame(Drug = "propofol", Time = 0, Dose = c(200, 100, 2),
                      Units = c("mg", "mcg/kg/min", "Effect site target"))
  r <- vr(doses, 40)
  e <- eliminated(r)
  # The manual 7 mg/min ran for the controller's first, zero-rate step before.
  expect_lt(abs(e[["viewer"]] - e[["integral"]]) / utils::tail(r$given, 1), 1e-3)
})

test_that("target rows entered out of time order are read in time order", {
  doses <- data.frame(Drug = "propofol", Time = c(20, 0), Dose = c(4, 2),
                      Units = c("Effect site target", "Plasma target"))
  r <- vr(doses, 40)
  at30 <- which(r$t == 1800)
  expect_equal(r$mode[at30], "effect")
  expect_equal(r$target[at30], 4)
  expect_equal(r$mode[which(r$t == 600)], "plasma")
})

test_that("a rate into a tissue depot leaves the drug given unknown", {
  r <- vr(data.frame(Drug = "ropivacaine", Time = 0, Dose = 20, Units = "mg/hr RA"), 120)
  expect_false(r$inputKnown)
})

test_that("GHB's peripheral compartment has the engine's volume, not a placeholder", {
  units <- getDrugDefaults("GHB")$Units[[1]][1]
  r <- vr(data.frame(Drug = "GHB", Time = 0, Dose = 2, Units = units), 240)
  mm <- getDrugPK("GHB", 70, 170, 40, "male", getDrugDefaults("GHB"))$michaelisMenten
  expect_equal(r$pk$v2, r$pk$v1 * mm$k23 / mm$k32)
  expect_false(is.null(r$c2))
})

test_that("a bolus at Max time is not counted: the simulation has not taken it in", {
  r <- vr(data.frame(Drug = "propofol", Time = c(0, 60), Dose = c(100, 50), Units = "mg"), 60)
  expect_equal(utils::tail(r$given, 1), 100, tolerance = 1e-9)
  expect_length(r$boluses, 1)
})

test_that("a metabolite also formed from a parent in the table is drawn as the plot draws it", {
  doses <- data.frame(Drug = c("morphine", "codeine"), Time = 0, Dose = c(5, 60),
                      Units = c("mg", "mg PO tablet"))
  plot <- plotSimulation(doses, 240)
  r <- vr(doses, 240)
  w <- plot$morphine$wide
  w <- w[w$Time > 1 & w$Time < 239, ]
  expect_equal(stats::approx(r$t / 60, r$cp, w$Time)$y, w$Plasma, tolerance = 1e-3)
  # Its input includes what codeine forms, which the dose rows do not show.
  expect_false(r$inputKnown)
})

# ChatGPT's audit of ff7e2f0 (2026-11-06).

test_that("fosphenytoin (mg PE) is not drawn as phenytoin entering V1", {
  # The prodrug goes to a conversion compartment (advanceMichaelisMenten()),
  # so it is neither a bolus nor a pump rate into V1.
  r <- vr(data.frame(Drug = "phenytoin", Time = 0, Dose = 1000, Units = "mg PE"), 60)
  expect_length(r$boluses, 0)
  expect_true(all(r$rate == 0))
  r <- vr(data.frame(Drug = "phenytoin", Time = 0, Dose = 50, Units = "mg PE/min"), 30)
  expect_true(all(r$rate == 0))
  expect_false(r$inputKnown)
  # Plain phenytoin sodium is still a bolus, after its salt factor.
  r <- vr(data.frame(Drug = "phenytoin", Time = 0, Dose = 1000, Units = "mg"), 60)
  expect_length(r$boluses, 1)
})

test_that("a saturable drug says its elimination is not CL1 x Cp; a linear one that it is", {
  expect_false(vr(data.frame(Drug = "phenytoin", Time = 0, Dose = 1000, Units = "mg PE"), 60)$eliminationLinear)
  expect_true(vr(data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg"), 30)$eliminationLinear)
})

test_that("viewerActActivity() refuses what is not one finite number", {
  act <- list(act0 = 116, emax = 720, c50 = 3.49)
  for (x in list("480", NULL, c(480, 500), NA_real_, Inf, -Inf, 116, 836))
    expect_identical(viewerActActivity(x, act), 0)
  expect_gt(viewerActActivity(480, act), 0)
})
