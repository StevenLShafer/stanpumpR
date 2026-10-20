# The peripheral compartments and the depot (R/compartments.R), shown by
# Graph Options -> Peripheral compartments.
#
# The reference is an independent solution of the full compartmental system
# by matrix exponential, sharing no code with the engines: absorption depots
# feeding a three-compartment model, every state an AMOUNT, amounts carried
# across a change in PK.  The engines read the peripheral concentrations off
# their plasma states (k21 sum(s / (k21 - mu))), so agreement to rounding
# checks that relation for each engine.  The metabolite and Michaelis-Menten
# engines are checked against the compartment's own differential equation,
# driven by the engine's plasma on a fine grid.
# (Claude Code, 2026-10-19, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

compartmentPK <- function(drug, weight = 70) {
  dd <- getDrugDefaultsGlobal()
  getDrugPK(drug, weight, 170, 50, "male", dd[dd$Drug == drug, ])
}

expmTaylor <- function(M) {
  s <- max(0, ceiling(log2(max(abs(M)) * 4 + 1e-300)))
  A <- M / 2^s
  E <- term <- diag(nrow(M))
  for (k in 1:20) { term <- term %*% A / k; E <- E + term }
  for (i in seq_len(s)) E <- E %*% E
  E
}

# State: depots (PO, IM, IN), central, second, third, and a constant 1 that
# carries an intravenous rate.  `sets` is one PK set, or two with the second
# in force from `switchAt`; doses in the engine's amount unit.
refRoutes <- c("PO", "IM", "IN")
refRun <- function(DT, sets, times, switchAt = Inf, weight = 70) {
  nD <- length(refRoutes); iC <- nD + 1; i2 <- nD + 2; i3 <- nD + 3; i1 <- nD + 4
  setAt <- function(t) if (t < switchAt) sets[[1]] else sets[[length(sets)]]
  field <- function(s, x) if (is.null(s[[x]]) || is.na(s[[x]])) 0 else s[[x]]
  system <- function(s, R) {
    A <- matrix(0, i1, i1)
    for (j in seq_len(nD)) {
      ka <- field(s, paste0("ka_", refRoutes[j]))
      A[j, j] <- -ka; A[iC, j] <- ka
    }
    A[iC, iC] <- -(s$k10 + s$k12 + s$k13)
    if (s$k12 > 0) { A[iC, i2] <- s$k21; A[i2, iC] <- s$k12; A[i2, i2] <- -s$k21 }
    if (s$k13 > 0) { A[iC, i3] <- s$k31; A[i3, iC] <- s$k13; A[i3, i3] <- -s$k31 }
    A[iC, i1] <- R
    A
  }
  perKg <- ifelse(grepl("kg", DT$Units), weight, 1)
  isRate <- grepl("/min|/hr", DT$Units)
  perMin <- ifelse(grepl("/hr", DT$Units), 1 / 60, 1)
  route <- vapply(DT$Units, function(u) {
    r <- refRoutes[vapply(refRoutes, grepl, logical(1), x = u)]
    if (length(r) == 0) "" else r
  }, character(1), USE.NAMES = FALSE)
  lands <- DT$Time
  for (d in which(route != ""))
    lands[d] <- DT$Time[d] + field(setAt(DT$Time[d]), paste0("tlag_", route[d]))
  rateAt <- function(t) {
    i <- which(isRate & DT$Time <= t)
    if (length(i) == 0) 0 else DT$Dose[max(i)] * perKg[max(i)] * perMin[max(i)]
  }
  breaks <- sort(unique(c(0, DT$Time, lands, if (is.finite(switchAt)) switchAt, times)))
  x <- c(rep(0, i1 - 1), 1)
  out <- matrix(NA_real_, length(times), 4,
                dimnames = list(NULL, c("C1", "C2", "C3", "Depot")))
  for (k in seq_along(breaks)) {
    t <- breaks[k]
    s <- setAt(t)
    for (d in which(lands == t & !isRate)) {
      amount <- DT$Dose[d] * perKg[d]
      if (route[d] == "") x[iC] <- x[iC] + amount
      else {
        j <- match(route[d], refRoutes)
        x[j] <- x[j] + amount * field(s, paste0("bioavailability_", route[d]))
      }
    }
    for (i in which(times == t))
      out[i, ] <- c(x[iC] / s$v1,
                    if (s$k12 > 0) x[i2] / s$v2 else 0,
                    if (s$k13 > 0) x[i3] / s$v3 else 0,
                    sum(x[seq_len(nD)]))
    if (k < length(breaks))
      x <- as.vector(expmTaylor(system(s, rateAt(t)) * (breaks[k + 1] - t)) %*% x)
  }
  out
}

relErr <- function(got, ref, floor = 1e-6) {
  keep <- abs(ref) > max(abs(ref)) * floor
  max(abs(got[keep] / ref[keep] - 1))
}


test_that("V2 and V3 of an intravenous drug are exact (advanceClosedForm0)", {
  PK <- compartmentPK("propofol")
  DT <- data.frame(Drug = "propofol", Time = c(0, 0, 60), Dose = c(2, 100, 0),
                   Units = c("mg/kg", "mcg/kg/min", "mcg/kg/min"))
  sim <- simCpCe(DT, noEvents, PK, 240, FALSE)
  cm <- sim$compartments
  expect_named(cm, c("Time", "V2", "V3"))
  # mcg/kg/min is 0.001 mg/kg/min; the reference doses are mg
  ref <- refRun(transform(DT, Dose = c(2, 0.1, 0)), list(PK$PK$default), cm$Time)
  expect_lt(relErr(sim$wide$Plasma, ref[, "C1"]), 1e-9)
  expect_lt(relErr(cm$V2, ref[, "C2"]), 1e-9)
  expect_lt(relErr(cm$V3, ref[, "C3"]), 1e-9)
  # The plot's grid ends at the window and starts empty
  expect_equal(max(cm$Time), 240)
  expect_lt(abs(cm$V2[1]) + abs(cm$V3[1]), 1e-12)
})


test_that("the absorbed routes' compartments and depot are exact (advanceClosedFormPO_IM_IN)", {
  PK <- compartmentPK("hydromorphone")
  DT <- data.frame(Drug = "hydromorphone", Time = c(0, 0, 50, 20), Dose = c(4, 1, 2, 0.5),
                   Units = c("mg PO", "mg IM", "mg IN", "mg"))
  sim <- simCpCe(DT, noEvents, PK, 300, FALSE)
  cm <- sim$compartments
  expect_named(cm, c("Time", "V2", "V3", "Depot"))
  # hydromorphone is in ng/mL: the engine's amounts are mcg
  ref <- refRun(transform(DT, Dose = Dose * 1000), list(PK$PK$default), cm$Time)
  expect_lt(relErr(sim$wide$Plasma, ref[, "C1"]), 1e-8)
  expect_lt(relErr(cm$V2, ref[, "C2"]), 1e-8)
  expect_lt(relErr(cm$V3, ref[, "C3"]), 1e-8)
  expect_lt(relErr(cm$Depot, ref[, "Depot"]), 1e-8)
  # The depot holds the bioavailable dose once the lag has passed
  s <- PK$PK$default
  landed <- which(cm$Time == s$tlag_PO)
  if (length(landed) == 1 && s$tlag_IM == 0)
    expect_equal(cm$Depot[landed],
                 4000 * s$bioavailability_PO +
                   1000 * s$bioavailability_IM * exp(-s$ka_IM * s$tlag_PO),
                 tolerance = 1e-9)
})


test_that("the compartments are exact across a change in PK (advanceClosedForm1)", {
  PK <- compartmentPK("hydromorphone")
  PK$PK$Switch <- compartmentPK("hydromorphone", 130)$PK$default
  PK$pkEvents <- c(PK$pkEvents, "Switch")
  DT <- data.frame(Drug = "hydromorphone", Time = c(0, 0, 20, 50), Dose = c(4, 1, 1, 2),
                   Units = c("mg PO", "mg IM", "mg", "mg IN"))
  sim <- simCpCe(DT, data.frame(Time = 50, Event = "Switch"), PK, 300, FALSE)
  cm <- sim$compartments
  ref <- refRun(transform(DT, Dose = Dose * 1000), list(PK$PK$default, PK$PK$Switch),
                cm$Time, switchAt = 50)
  expect_lt(relErr(sim$wide$Plasma, ref[, "C1"]), 1e-8)
  expect_lt(relErr(cm$V2, ref[, "C2"]), 1e-8)
  expect_lt(relErr(cm$V3, ref[, "C3"]), 1e-8)
  expect_lt(relErr(cm$Depot, ref[, "Depot"]), 1e-8)
})


test_that("a tissue depot carries its slow share, its lag and a continuous infusion", {
  PK <- compartmentPK("bupivacaine")
  s <- PK$PK$default
  DT <- data.frame(Drug = "bupivacaine", Time = c(0, 30, 90), Dose = c(100, 10, 0),
                   Units = c("mg RA", "mg/hr RA", "mg/hr RA"))
  cm <- simCpCe(DT, noEvents, PK, 240, FALSE)$compartments
  t <- cm$Time
  # The bolus, through each depot it has, plus a constant rate from 30 to 90
  # minutes: F R / ka (1 - exp(-ka u)) while it runs, decaying after.
  bolus <- function(ka, F, lag) ifelse(t >= lag, 100 * F * exp(-ka * (t - lag)), 0)
  infusion <- function(ka, F, lag) {
    on <- pmin(pmax(t - 30 - lag, 0), 60)
    off <- pmax(t - 90 - lag, 0)
    F * (10 / 60) / ka * (1 - exp(-ka * on)) * exp(-ka * off)
  }
  expected <- bolus(s$ka_RA, s$bioavailability_RA, s$tlag_RA) +
    infusion(s$ka_RA, s$bioavailability_RA, s$tlag_RA)
  if (isTRUE(s$ka_RAslow > 0))
    expected <- expected + bolus(s$ka_RAslow, s$bioavailability_RAslow, s$tlag_RAslow) +
      infusion(s$ka_RAslow, s$bioavailability_RAslow, s$tlag_RAslow)
  expect_lt(relErr(cm$Depot, expected), 1e-9)
})


# A compartment's own equation, dC/dt = k (Cplasma - C), integrated by RK4 on
# a fine grid with the plasma interpolated linearly between the engine's own
# points, which withOutputTimes() makes dense.
odeCompartment <- function(time, plasma, k, h = 0.01) {
  grid <- seq(0, max(time), by = h)
  cp <- stats::approx(time, plasma, grid, rule = 2)$y
  mid <- stats::approx(time, plasma, grid + h / 2, rule = 2)$y
  C <- numeric(length(grid))
  for (i in seq_along(grid)[-1]) {
    f <- function(c, p) k * (p - c)
    c0 <- C[i - 1]
    k1 <- f(c0, cp[i - 1]); k2 <- f(c0 + h / 2 * k1, mid[i - 1])
    k3 <- f(c0 + h / 2 * k2, mid[i - 1]); k4 <- f(c0 + h * k3, cp[i])
    C[i] <- c0 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  function(t) stats::approx(grid, C, t)$y
}


test_that("a formed metabolite fills its own drug's compartments, and adds to a direct dose", {
  # Dense output, because the reference interpolates the plasma linearly
  # between the engine's points; on a 0.5-minute grid that alone is 1e-3.
  run <- function(dose) withOutputTimes(seq(0, 480, by = 0.05), simulateDrugsWithCovariates(
    dose, noEvents, weight = 70, height = 170, age = 40, sex = "male",
    maximum = 480, plotRecovery = FALSE))
  x <- run(data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg PO"))
  # Codeine's own model has no peripheral compartment; its depot is its own
  expect_named(x$codeine$compartments, c("Time", "Depot"))
  cm <- x$morphine$compartments
  expect_named(cm, c("Time", "V2", "V3"))
  s <- compartmentPK("morphine")$PK$default
  w <- x$morphine$wide
  check <- cm$Time[cm$Time >= 5]
  for (k in c("V2", "V3")) {
    rate <- if (k == "V2") s$k21 else s$k31
    ode <- odeCompartment(w$Time, w$Plasma, rate)
    expect_lt(relErr(stats::approx(cm$Time, cm[[k]], check)$y, ode(check), 1e-3), 1e-4,
              label = paste("formed morphine", k, "against its own equation"))
  }
  # Given as well, morphine's compartments are the sum of the two
  direct <- data.frame(Drug = "morphine", Time = 120, Dose = 5, Units = "mg")
  both <- run(rbind(data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg PO"), direct))
  own <- run(direct)$morphine$compartments
  sum <- both$morphine$compartments
  at <- own$Time[own$Time > 0]
  for (k in c("V2", "V3"))
    expect_lt(relErr(stats::approx(sum$Time, sum[[k]], at)$y,
                     own[[k]][own$Time > 0] + stats::approx(cm$Time, cm[[k]], at)$y), 1e-4,
              label = paste("morphine", k, "given and formed"))
})


test_that("the Michaelis-Menten engine reports its peripheral compartment and depot (GHB)", {
  PK <- compartmentPK("GHB")
  DT <- data.frame(Drug = "GHB", Time = 0, Dose = 4.5, Units = "g PO oxybate")
  sim <- withOutputTimes(seq(0, 360, by = 0.5), simCpCe(DT, noEvents, PK, 360, FALSE))
  cm <- sim$compartments
  expect_true(all(c("V2", "Depot") %in% names(cm)))
  mm <- PK$michaelisMenten
  ode <- odeCompartment(sim$wide$Time, sim$wide$Plasma, mm$k32)
  check <- cm$Time[cm$Time >= 5]
  expect_lt(relErr(stats::approx(cm$Time, cm$V2, check)$y, ode(check), 1e-3), 2e-3)
  expect_equal(max(cm$Depot), max(cm$Depot[cm$Time <= 1]))
  expect_lt(utils::tail(cm$Depot, 1), max(cm$Depot) / 100)
})


test_that("a drug shows only the compartments it has", {
  # One compartment, intravenous only: nothing to show
  PK <- compartmentPK("codeine")
  expect_null(simCpCe(data.frame(Drug = "codeine", Time = 0, Dose = 1, Units = "mg/hr"),
                      noEvents, PK, 60, FALSE)$compartments)
  # Intravenous propofol has no depot
  PK <- compartmentPK("propofol")
  cm <- simCpCe(data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg"),
                noEvents, PK, 60, FALSE)$compartments
  expect_false("Depot" %in% names(cm))
  # Warfarin's plotted curve is the INR, and mannitol's the serum osmolality
  for (drug in c("warfarin", "mannitol")) {
    dd <- getDrugDefaultsGlobal()
    units <- strsplit(as.character(dd$Units[dd$Drug == drug]), ",")[[1]][1]
    PK <- compartmentPK(drug)
    expect_null(simCpCe(data.frame(Drug = drug, Time = 0, Dose = 5, Units = units),
                        noEvents, PK, 600, FALSE)$compartments, label = drug)
  }
})


test_that("the rate constant is nudged off a tie, and an unused state adds nothing", {
  # A mode equal to k: the state carries the factor (k - mu), here zero
  out <- peripheralConcentration(list(c(0, 1), c(0, 0)), c(0.1, 0.05), 0.05)
  expect_true(all(is.finite(out)))
  expect_equal(out, c(0, 0.05 * 1 / (0.05 - 0.1)), tolerance = 1e-6)
  # No such compartment
  expect_equal(peripheralConcentration(list(1:3), 0.1, NaN), rep(0, 3))
  # One rate per point
  expect_equal(peripheralConcentration(cbind(c(1, 1)), cbind(c(0.1, 0.2)), c(0.05, 0.05)),
               c(0.05 / -0.05, 0.05 / -0.15))
})


test_that("an inhaled agent's muscle and fat groups come from its gas states", {
  DT <- data.frame(Drug = c("sevoflurane", "oxygen", "ventilation"), Time = 0,
                   Dose = c(2, 6, 6), Units = c("%", "L/min", "L/min"))
  sim <- simulateGases(DT, maximum = 120)
  entries <- gasDrugEntries(sim, DT, getDrugDefaultsGlobal(), 120)
  cm <- entries$sevoflurane$compartments
  expect_named(cm, c("Time", "MRG", "FG"))
  expect_equal(cm$MRG, sim$state$sevoflurane[, 4])
  expect_equal(cm$FG, sim$state$sevoflurane[, 5])
  expect_true(cm$FG[nrow(cm)] < cm$MRG[nrow(cm)])
  expect_null(entries$oxygen$compartments)
  expect_null(entries$MAC$compartments)
})


test_that("the compartment panels go directly under their drug's, before its TCI panel", {
  dd <- getDrugDefaultsGlobal()
  DT <- data.frame(Drug = c("propofol", "morphine", "remifentanil"), Time = 0,
                   Dose = c(3, 30, 0.1), Units = c("Plasma target", "mg PO tablet", "mcg/kg/min"))
  drugs <- recalculatePK(NULL, dd, DT, age = 40, weight = 70, height = 170, sex = "male")
  drugs <- processdoseTable(DT, noEvents, drugs, 120, FALSE)
  plotWith <- function(...) simulationPlot(drugs, noEvents, dd, data.frame(Event = character(0)),
                                           xBreaks = seq(0, 120, 30), xLabels = seq(0, 120, 30),
                                           xMaximum = 120, plasmaLinetype = "solid", ...)
  off <- plotWith()
  expect_false(any(grepl(" V2\n| depot", levels(off$plotResults$Wrap), ignore.case = TRUE)))
  on <- plotWith(plotCompartments = TRUE)
  expect_equal(levels(on$plotResults$Wrap), c(
    "propofol\n(mcg/ml)", "propofol V2\n(mcg/ml)", "propofol V3\n(mcg/ml)",
    levels(off$plotResults$Wrap)[2],
    "remifentanil\n(ng/ml)", "remifentanil V2\n(ng/ml)", "remifentanil V3\n(ng/ml)",
    "morphine\n(mcg/ml)", "morphine V2\n(mcg/ml)", "morphine V3\n(mcg/ml)",
    "morphine Depot\n(mg)"
  ))
  expect_match(levels(off$plotResults$Wrap)[2], "^propofol TCI")
  expect_equal(on$plotHeight - off$plotHeight, 7 * 150)
  # The strips name the compartment alone; the values stay unique to the drug
  expect_equal(compartmentPanelLabel(c("propofol V2\n(mcg/ml)", "morphine Depot\n(mg)",
                                       "propofol\n(mcg/ml)", "sevoflurane FG\n(%)")),
               c("V2\n(mcg/ml)", "depot\n(mg)", "propofol\n(mcg/ml)", "FG\n(%)"))
  # The compartment series join no legend
  built <- ggplot2::ggplot_build(on$plotObject)
  expect_silent(ggplot2::ggplot_gtable(built))
  # Normalization has no peak to scale a compartment by
  norm <- plotWith(plotCompartments = TRUE, normalization = "Peak plasma")
  expect_false(any(grepl(" V2\n", levels(norm$plotResults$Wrap))))
  # A click on a compartment panel's series finds its drug
  expect_equal(compartmentSeries(c("morphine Depot", "propofol V3", "propofol TCI"))$drug,
               c("morphine", "propofol", NA))
})


test_that("a scenario may show the compartments only without normalization", {
  drugDefaults <- getDrugDefaultsGlobal()
  s <- helpScenario("compartments-normalized", "Bad", "Intravenous basics", "x",
                    doses = helpDoses(c("propofol", 0, 100, "mg")),
                    showCompartments = TRUE, normalization = "Peak plasma")
  expect_match(helpScenarioCheck(s, drugDefaults), "showCompartments needs", all = FALSE)
  for (id in c("propofol-peripheral-compartments", "oral-morphine-depot"))
    expect_true(helpScenarioById(id)$options$showCompartments, label = id)
})


test_that("each oral formulation's run adds its depot (morphine tablet and liquid)", {
  PK <- compartmentPK("morphine")
  one <- function(units) simCpCe(data.frame(Drug = "morphine", Time = 0, Dose = 10, Units = units),
                                 noEvents, PK, 240, FALSE)$compartments
  tablet <- one("mg PO tablet")
  liquid <- one("mg PO liquid")
  both <- simCpCe(data.frame(Drug = "morphine", Time = 0, Dose = 10,
                             Units = c("mg PO tablet", "mg PO liquid")),
                  noEvents, PK, 240, FALSE)$compartments
  expect_equal(both$Depot, tablet$Depot + liquid$Depot, tolerance = 1e-12)
  expect_equal(both$V3, tablet$V3 + liquid$V3, tolerance = 1e-12)
  # The liquid is absorbed faster
  expect_lt(stats::approx(liquid$Time, liquid$Depot, 60)$y,
            stats::approx(tablet$Time, tablet$Depot, 60)$y)
})
