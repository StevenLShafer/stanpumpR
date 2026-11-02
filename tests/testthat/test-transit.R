# One transit compartment before a depot, and an enteral infusion through it:
# levodopa, its intestinal gel and opicapone, which declare a transit chain of
# two compartments (R/advanceTransitChain.R, exact by matrix exponential), and
# Lecigon, whose enteral infusion rides on the tissue depot (R/enteral.R).
# These tests measure the drugs' curves against an independent numerical
# solution of the chain itself.  Drafted by Claude Code, 2026-10-19; moved
# from R/transit.R's two-depot limit (good to 1e-5) to the exact engine
# 2026-10-31, so the tolerances are now the Runge-Kutta solution's own.

# The levodopa model (Othman 2014) with its effect site (Mao 2013), solved by
# fourth-order Runge-Kutta at a 0.01 min step: dosing compartment, transit
# compartment, central, peripheral, effect site.  Doses go into the dosing
# compartment; rates are list(c(start, end, mg/min)).
transitODE <- function(times, ktr, F, doses = list(), rates = list(),
                       ke0 = 1.80 / 60, weight = 70)
{
  CL <- 24.8 / 60; V1 <- 58.5 * weight / 70; Q <- 6.8 / 60; V2 <- 72.9
  f <- function(y, R) c(-ktr * y[1] + R,
                        ktr * y[1] - ktr * y[2],
                        ktr * y[2] - (CL + Q) / V1 * y[3] + Q / V2 * y[4],
                        Q / V1 * y[3] - Q / V2 * y[4],
                        ke0 * (y[3] / V1 - y[5]))
  rate <- function(t) sum(vapply(rates, function(r) if (t >= r[1] && t < r[2]) r[3] * F else 0, 0))
  y <- c(0, 0, 0, 0, 0); t <- 0; h <- 0.01
  cp <- ce <- numeric(length(times))
  for (i in seq_along(times))
  {
    while (t < times[i] - 1e-12)
    {
      for (d in doses) if (abs(t - d[1]) < 1e-9) y[1] <- y[1] + d[2] * F
      hh <- min(h, times[i] - t); R <- rate(t + hh / 2)
      k1 <- f(y, R); k2 <- f(y + hh / 2 * k1, R); k3 <- f(y + hh / 2 * k2, R); k4 <- f(y + hh * k3, R)
      y <- y + hh / 6 * (k1 + 2 * k2 + 2 * k3 + k4); t <- t + hh
    }
    cp[i] <- y[3] / V1; ce[i] <- y[5]
  }
  list(cp = cp, ce = ce)
}

test_that("an oral dose through the transit compartment matches the numerical chain", {
  res <- pdSimulate(data.frame(Drug = "levodopa", Time = 0, Dose = 200, Units = "mg PO"),
                    maximum = 480, age = 64)
  p <- pdSite(res); e <- pdSite(res, "Effect Site")
  t <- p$Time[p$Time > 0]
  ode <- transitODE(t, 2.4 / 60, 1, doses = list(c(0, 200)))
  expect_lt(max(abs(p$Y[p$Time > 0] - ode$cp)) / max(ode$cp), 1e-8)
  expect_lt(max(abs(e$Y[e$Time > 0] - ode$ce)) / max(ode$ce), 1e-8)
})

test_that("an enteral infusion and a dose through the chain match the numerical chain", {
  # 200 mg over 15 minutes, then 60 mg/h
  dt <- data.frame(Drug = "levodopaIntestinalGel", Time = c(0, 15),
                   Dose = c(800, 60), Units = "mg/hr PO")
  p <- pdSite(pdSimulate(dt, maximum = 480, age = 64))
  t <- p$Time[p$Time > 0]
  ode <- transitODE(t, 9.2 / 60, 0.97, rates = list(c(0, 15, 800 / 60), c(15, 1e9, 1)))
  expect_lt(max(abs(p$Y[p$Time > 0] - ode$cp)) / max(ode$cp), 1e-8)
  # and a dose of the gel through the tube
  p <- pdSite(pdSimulate(data.frame(Drug = "levodopaIntestinalGel", Time = 0, Dose = 100,
                                    Units = "mg PO"), maximum = 360, age = 64))
  t <- p$Time[p$Time > 0]
  ode <- transitODE(t, 9.2 / 60, 0.97, doses = list(c(0, 100)))
  expect_lt(max(abs(p$Y[p$Time > 0] - ode$cp)) / max(ode$cp), 1e-8)
})

test_that("the transit chain delays the peak as two steps do, not one", {
  # A single first-order step at 2.4/h would peak at about 48 minutes; the
  # chain, at about 81 (Othman 2014's simulation: 75).
  res <- pdSimulate(data.frame(Drug = "levodopa", Time = 0, Dose = 200, Units = "mg PO"),
                    maximum = 240, age = 64)
  expect_gt(pdPeak(res)[["t"]] * 60, 70)
  expect_lt(pdPeak(res)[["t"]] * 60, 90)
})

test_that("the time until threshold runs on a transit drug", {
  res <- pdSimulate(data.frame(Drug = "levodopa", Time = 0, Dose = 200, Units = "mg PO"),
                    maximum = 480, age = 64, plotRecovery = TRUE)
  rec <- pdSite(res, "Recovery")
  expect_true(any(is.finite(rec$Y) & rec$Y > 0))
})

test_that("the transit drugs run on the chain engine, Lecigon on the tissue depot", {
  for (d in c("levodopa", "levodopaIntestinalGel", "opicapone"))
  {
    pk <- getDrugPK(d, 70, 170, 60, "male")
    expect_identical(pk$transitChain$n, 2L, info = d)
    expect_equal(pk$transitChain$ktr, pk$PK$default$ka_PO, tolerance = 1e-6, info = d)
    # the oral absorption stays oral: nothing is moved onto the tissue depot
    expect_false(isTRUE(pk$PK$default$ka_RA > 0), info = d)
  }
  lecig <- getDrugPK("levodopaEntacaponeIntestinalGel", 70, 170, 60, "male")
  expect_null(lecig$transitChain)
  expect_true(lecig$PK$default$ka_RA > 0)
})

test_that("enteral declarations are validated", {
  base <- list(v1 = 10, v2 = 1, v3 = 1, cl1 = 1, cl2 = 0, cl3 = 0)
  m <- function(set, ...) c(list(PK = list(default = set), tPeak = 0, MEAC = 0,
                                 typical = 0, upperTypical = 0, lowerTypical = 0,
                                 reference = "test"), list(...))
  expect_error(validateEnteralInfusion(TRUE, m(base), "x"), "absorbed from the gut")
  expect_error(validateEnteralInfusion(TRUE, m(c(base, ka_PO = 0.01, ka_RA = 0.01)), "x"),
               "no tissue route")
  expect_error(validateEnteralInfusion(TRUE, m(c(base, ka_PO = 0.01, ka_PO2 = 0.02)), "x"),
               "second oral depot")
  expect_error(validateEnteralInfusion(TRUE, m(c(base, ka_PO = 0.01),
                                               oralSaturation = list(form = "power")), "x"),
               "dose-dependent")
  expect_false(validateEnteralInfusion(NULL, m(base), "x"))
})

test_that("only an enteral drug takes the enteral rate", {
  expect_error(pdSimulate(data.frame(Drug = "levodopa", Time = 0, Dose = 50, Units = "mg/hr PO"),
                          maximum = 120), "not given by enteral infusion")
  dd <- getDrugDefaultsGlobal()
  offers <- dd$Drug[vapply(dd$Units, function(u) any(u %in% enteralRateUnits), logical(1))]
  expect_setequal(offers, c("levodopaIntestinalGel", "levodopaEntacaponeIntestinalGel"))
  for (d in offers) expect_true(isTRUE(getDrugPK(d, 70, 170, 60, "male")$enteralInfusion), info = d)
})

test_that("a zero-order oral share into the circulation is exact", {
  # Amantadine ER (R/drugs_amantadineER.R): 79.9% first order at 0.344/h,
  # 20.1% at a constant rate over 8.79 h, one compartment, no transit.
  pk <- amantadineER(70, 170, 60, "male", FALSE)$PK$default
  V <- pk$v1; k <- pk$cl1 / pk$v1; ka <- pk$ka_PO; D <- 100; Tz <- 8.79 * 60
  res <- pdSite(pdSimulate(data.frame(Drug = "amantadineER", Time = 0, Dose = D,
                                      Units = "mg PO"), maximum = 2880))
  t <- res$Time[res$Time > 0]
  first <- 0.799 * D * ka / (V * (ka - k)) * (exp(-k * t) - exp(-ka * t))
  zero <- 0.201 * D / Tz / (V * k) *
    ifelse(t <= Tz, 1 - exp(-k * t), (1 - exp(-k * Tz)) * exp(-k * (t - Tz)))
  expect_lt(max(abs(res$Y[res$Time > 0] - (first + zero) * 1000)) / max((first + zero) * 1000), 1e-8)
})

test_that("the depot panel shows what is left in the dosing and transit compartments together", {
  # The chain's contents, F D exp(-k t) (1 + k t), which Graph Options ->
  # Peripheral compartments draws as the depot.
  noE <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  r <- simulateDrugsWithCovariates(data.frame(Drug = "levodopa", Time = 0, Dose = 200,
                                              Units = "mg PO"),
                                   noE, 70, 170, 60, "male", maximum = 480,
                                   adjustToFFM = FALSE, plotRecovery = FALSE)
  cmp <- r$levodopa$compartments
  k <- LEVODOPA_KTR_ORAL / 60
  expect_equal(cmp$Depot, 200 * exp(-k * cmp$Time) * (1 + k * cmp$Time), tolerance = 1e-9)
  expect_true("V2" %in% names(cmp))
})
