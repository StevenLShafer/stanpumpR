# Subcutaneous dosing: the SC route on the tissue depot, insulin units, the
# pump rate, weekly schedules and zero-order input (R/subcutaneous.R).  Every
# curve is checked against its closed-form solution, not against an earlier
# run of the same code.  Drafted by Claude Code, 2026-10-17.

noEvents <- data.frame(Time = numeric(0), Event = character(0))
doseRows <- function(drug, time, dose, units)
  data.frame(Drug = drug, Time = time, Dose = dose, Units = units)
cpAt <- function(sim, t) stats::approx(sim$wide$Time, sim$wide$Plasma, xout = t)$y
# The engine's own time points, where its concentrations are exact (between
# them the plot interpolates), within [from, to]
gridOf <- function(sim, from = 0, to = Inf) {
  t <- sim$wide$Time
  keep <- t >= from & t <= to
  list(t = t[keep], cp = sim$wide$Plasma[keep])
}

# A one-compartment SC test model; `extra` adds fields to the PK set and
# `block` to the model.
scModel <- function(v = 10, cl = 1, ka = 0.05, f = 0.8, tlag = 0, extra = list(),
                    block = list())
  c(list(PK = list(default = c(list(v1 = v, v2 = 1, v3 = 1, cl1 = cl, cl2 = 0, cl3 = 0,
                                    ka_SC = ka, bioavailability_SC = f, tlag_SC = tlag),
                               extra)),
         tPeak = 0, MEAC = 0, typical = 0, upperTypical = 0, lowerTypical = 0,
         reference = "test"), block)
mockPK <- function(model, drug = "semaglutide")
  getDrugPK(drug, 70, 170, 40, "male", model = model)

test_that("SC units read as the SC route, rates as rates", {
  expect_equal(doseRoute(c("mg SC", "U SC", "U/kg SC", "U/hr SC", "mg SC qwk", "U SC tid")),
               rep(ROUTE_SC, 6))
  expect_true(isRateUnit("U/hr SC"))
  expect_false(any(isRateUnit(c("U SC", "mg SC qwk"))))
  expect_true(all(c(scUnits, insulinUnits, insulinRateUnits) %in% allUnits))
  expect_true(all(c("mg SC qwk", "U SC bid", "U/kg SC qd") %in% scheduledUnits))
  # qwk is offered on the subcutaneous units only
  expect_false(any(grepl(" qwk$", setdiff(scheduledUnits,
    as.vector(outer(c(scUnits, insulinUnits), names(ALL_SCHEDULE_INTERVALS), paste))))))
  expect_true(all(nchar(scheduledUnits) <= MAX_UNIT_STRING_LENGTH))
})

test_that("SC absorption moves onto the tissue depot, and SC with RA is refused", {
  pk <- mockPK(scModel(ka = 0.05, f = 0.8, tlag = 3))$PK$default
  expect_equal(pk$ka_RA, 0.05)
  expect_equal(pk$bioavailability_RA, 0.8)
  expect_equal(pk$tlag_RA, 3)
  expect_null(pk$ka_SC)
  expect_equal(mockPK(scModel())$tissueRoute, ROUTE_SC)
  expect_equal(getDrugPK("bupivacaine", 70, 170, 40, "male")$tissueRoute, ROUTE_RA)
  expect_error(mockPK(scModel(extra = list(ka_RA = 0.1))), "not both")
})

test_that("an SC dose gives the one-compartment Bateman curve, lag included", {
  v <- 10; cl <- 1; ka <- 0.05; f <- 0.8; lag <- 7; D <- 2
  pk <- mockPK(scModel(v, cl, ka, f, lag))
  sim <- simCpCe(doseRows("semaglutide", 0, D, "mg SC"), noEvents, pk, 600, FALSE)
  k <- cl / v
  g <- gridOf(sim, lag)
  s <- g$t - lag
  # semaglutide reports ng/mL, so mg doses are carried as mcg: x1000
  expected <- 1000 * f * D * ka / (v * (ka - k)) * (exp(-k * s) - exp(-ka * s))
  expect_equal(g$cp, expected, tolerance = 1e-9)
  expect_equal(cpAt(sim, 5), 0)
})

test_that("one-compartment mass balance: AUC is F x dose / CL, and IV C(0) is dose / V", {
  v <- 10; cl <- 1; f <- 0.8
  pk <- mockPK(scModel(v, cl, 0.05, f))
  sim <- simCpCe(doseRows("semaglutide", 0, 1, "mg SC"), noEvents, pk, 20000, FALSE)
  w <- sim$wide
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
  expect_equal(auc, 1000 * f / cl, tolerance = 0.01)   # trapezoids
  # The one-compartment bolus coefficient is 1/V (getDrugPK(); it once also
  # divided by lambda_1)
  expect_equal(pk$PK$default$p_coef_bolus_l1, 1 / v)
})

test_that("a pump rate into the SC depot reaches F x rate / CL, exactly", {
  v <- 10; cl <- 1; ka <- 0.05; f <- 0.8; r <- 0.06   # mg/min = 3.6 mg/hr
  pk <- mockPK(scModel(v, cl, ka, f))
  # the test drug has no "mg/hr SC" unit; the pump rows are flagged as the
  # rate route, as "U/hr SC" is
  expect_true(isRateUnit("U/hr SC"))
  pkI <- getDrugPK("insulinRegular", 70, 170, 40, "male",
                   model = scModel(v, cl, ka, f))
  sim <- simCpCe(doseRows("insulinRegular", 0, r * 60, "U/hr SC"), noEvents, pkI, 3000, FALSE)
  k <- cl / v
  g <- gridOf(sim)
  t <- g$t
  # U/hr: x1000 mU per U, /60 per minute
  rate <- 1000 * r
  expected <- f * rate / cl * (1 - (ka * exp(-k * t) - k * exp(-ka * t)) / (ka - k))
  expect_equal(g$cp, expected, tolerance = 1e-9)
})

test_that("insulin doses are in units: U to mU, mass refused, U refused elsewhere", {
  pk <- getDrugPK("insulinRegular", 70, 170, 40, "male")
  expect_error(simCpCe(doseRows("insulinRegular", 0, 1, "mg SC"), noEvents, pk, 600, FALSE),
               "units \\(U\\)")
  pkS <- getDrugPK("semaglutide", 70, 170, 40, "male")
  expect_error(simCpCe(doseRows("semaglutide", 0, 1, "U SC"), noEvents, pkS, 600, FALSE),
               "mcU/mL")
  # U/kg SC is the per-kilogram dose
  a <- simCpCe(doseRows("insulinRegular", 0, 7, "U SC"), noEvents, pk, 600, FALSE)
  b <- simCpCe(doseRows("insulinRegular", 0, 0.1, "U/kg SC"), noEvents, pk, 600, FALSE)
  expect_equal(a$wide$Plasma, b$wide$Plasma)
})

test_that("a weekly SC dose repeats every 7 days to the end of the plot", {
  x <- expandScheduledDoses(doseRows("semaglutide", 0, 1, "mg SC qwk"), 5 * MINS_PER_WEEK)
  expect_equal(x$scheduled$Time, (1:4) * MINS_PER_WEEK)
  expect_true(all(x$dose$Units == "mg SC"))
})

test_that("a parallel zero-order share enters the circulation as a box", {
  v <- 10; cl <- 1; ka <- 0.05; lag <- 5; T0 <- 60; phi <- 0.3; D <- 4
  model <- scModel(v, cl, ka, 1, lag, block = list(
    zeroOrderSC = list(fraction = phi, duration = T0, into = "central")))
  pk <- getDrugPK("insulinRegular", 70, 170, 40, "male", model = model)
  expect_equal(pk$PK$default$bioavailability_RA, 1 - phi)
  expect_equal(pk$zeroOrderSC$share, phi)
  sim <- simCpCe(doseRows("insulinRegular", 0, D, "U SC"), noEvents, pk, 900, FALSE)
  k <- cl / v
  g <- gridOf(sim)
  t <- g$t
  amount <- 1000 * D
  depot <- (1 - phi) * amount * ka / (v * (ka - k)) *
    (exp(-k * pmax(t - lag, 0)) - exp(-ka * pmax(t - lag, 0)))
  R <- phi * amount / T0
  box <- ifelse(t <= T0, R / cl * (1 - exp(-k * t)),
                R / cl * (1 - exp(-k * T0)) * exp(-k * (t - T0)))
  expect_equal(g$cp, depot + box, tolerance = 1e-9)
  # Overlapping doses add: two at once equal one of twice the size
  two <- simCpCe(doseRows("insulinRegular", c(0, 0), c(D, D), "U SC"), noEvents, pk, 900, FALSE)
  one <- simCpCe(doseRows("insulinRegular", 0, 2 * D, "U SC"), noEvents, pk, 900, FALSE)
  expect_equal(two$wide$Plasma, one$wide$Plasma, tolerance = 1e-12)
})

test_that("a sequential zero-order release feeds the depot", {
  v <- 10; cl <- 1; ka <- 0.05; f <- 0.9; T0 <- 120; D <- 2
  model <- scModel(v, cl, ka, f, block = list(
    zeroOrderSC = list(fraction = 1, duration = T0, into = "depot")))
  pk <- mockPK(model)
  sim <- simCpCe(doseRows("semaglutide", 0, D, "mg SC"), noEvents, pk, 1000, FALSE)
  k <- cl / v
  r <- 1000 * D / T0
  up <- function(t) f * r / cl * (1 - (ka * exp(-k * t) - k * exp(-ka * t)) / (ka - k))
  g <- gridOf(sim)
  t <- g$t
  # superposition: the rate on from 0, the same rate off from T0
  expected <- up(t) - ifelse(t > T0, up(pmax(t - T0, 0)), 0)
  expect_equal(g$cp, expected, tolerance = 1e-9)
  expect_error(validateZeroOrderSC(list(fraction = 0.5, duration = 60, into = "depot"),
                                   pk$PK, "x"), "whole dose")
})

test_that("zero-order input refuses an intravenous infusion and, into the depot, a pump", {
  central <- scModel(block = list(zeroOrderSC = list(fraction = 0.3, duration = 60, into = "central")))
  pk <- getDrugPK("insulinRegular", 70, 170, 40, "male", model = central)
  flagged <- data.frame(Time = c(0, 10), Dose = c(1, 1), Units = c("U SC", "x"),
                        Bolus = FALSE, PO = FALSE, IM = FALSE, IN = FALSE, SL = FALSE,
                        RA = c(TRUE, FALSE), RAslow = FALSE, RArate = FALSE, PO2 = FALSE)
  expect_error(expandZeroOrderSC(flagged, pk$zeroOrderSC, 600), "intravenous infusion")
  depot <- list(fraction = 1, duration = 60, into = "depot", share = 1)
  flagged$RA <- c(FALSE, FALSE); flagged$RArate <- c(TRUE, FALSE)
  flagged <- flagged[1, ]
  expect_error(expandZeroOrderSC(flagged, depot, 600), "pump rate")
})

test_that("insulin lispro by pump: the plateau is rate / CL, the zero-order share included", {
  pk <- getDrugPK("insulinLispro", 70, 170, 40, "male", adjustToFFM = FALSE)
  sim <- simCpCe(doseRows("insulinLispro", c(0, 1440), c(1, 0), "U/hr SC"), noEvents, pk, 2000, FALSE)
  # 1 U/hr = 1000 mU/hr over CL/F 30.5 L/h
  expect_equal(cpAt(sim, 1400), 1000 / 30.5, tolerance = 1e-4)
  # and back to (almost) nothing after the pump stops
  expect_lt(cpAt(sim, 1990), 0.01 * 1000 / 30.5)
  # an injection and the same amount by a 2-minute pump burst nearly agree
  inj <- simCpCe(doseRows("insulinLispro", 0, 1, "U SC"), noEvents, pk, 600, FALSE)
  burst <- simCpCe(doseRows("insulinLispro", c(0, 2), c(30, 0), "U/hr SC"), noEvents, pk, 600, FALSE)
  expect_equal(cpAt(burst, c(120, 300)), cpAt(inj, c(120, 300)), tolerance = 0.02)
})

test_that("a slow SC depot can carry a lag of its own", {
  model <- scModel(v = 10, cl = 1, ka = 0.1, f = 1, tlag = 0,
                   extra = list(ka_SC_slow = 0.01, fraction_SC_slow = 0.5, tlag_SC_slow = 20))
  pk <- mockPK(model)$PK$default
  expect_equal(pk$tlag_RAslow, 20)
  expect_equal(pk$tlag_RA, 0)
  expect_equal(pk$bioavailability_RA + pk$bioavailability_RAslow, 1)
})

test_that("help pages name the SC route", {
  html <- helpPageHTML("drugs/insulinLispro")
  expect_match(html, "Subcutaneous (SC)", fixed = TRUE)
  expect_match(html, "insulin pump", fixed = TRUE)
  expect_match(html, "mcU/mL", fixed = TRUE)
  expect_false(grepl("Regional anesthesia (RA)", html, fixed = TRUE))
})
