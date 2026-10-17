# The numerical Michaelis-Menten engine, R/advanceMichaelisMenten.R.
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.

mmPK <- function(v = 50, vmax = 0.5, km = 5, ka = 0.01, F = 1, tlag = 0,
                 kConversion = NULL, ka_IM = NULL, saltFactor = list(),
                 oralFormulations = NULL, k23 = NULL, k32 = NULL)
{
  list(
    PK = list(default = list(v1 = v, ka_PO = ka, bioavailability_PO = F,
                             tlag_PO = tlag)),
    oralFormulations = oralFormulations,
    michaelisMenten = list(vmax = vmax, km = km, kConversion = kConversion,
                           ka_IM = ka_IM, saltFactor = saltFactor,
                           k23 = k23, k32 = k32)
  )
}

mmDose <- function(Time, Dose, Units)
{
  route <- doseRoute(Units)
  rate <- isRateUnit(Units)
  data.frame(Time = Time, Dose = Dose, Units = Units,
             Bolus = route == ROUTE_IV & !rate, PO = route == ROUTE_PO & !rate,
             IM = route == ROUTE_IM & !rate, stringsAsFactors = FALSE)
}

test_that("with Km far above the concentrations it is the linear one-compartment model", {
  # CL = Vmax / Km when C << Km
  V <- 40; CL <- 0.05; km <- 1e7
  PK <- mmPK(v = V, vmax = CL * km, km = km, ka = 0.02, F = 0.8)
  dose <- mmDose(c(0, 300, 600), c(100, 50, 0.5), c("mg", "mg PO", "mg/min"))
  out <- advanceMichaelisMenten(dose, PK, 1440)
  k <- CL / V; ka <- 0.02
  exact <- function(t) {
    iv <- 100 / V * exp(-k * t)
    po <- ifelse(t >= 300, 0.8 * 50 * ka / (V * (ka - k)) *
                   (exp(-k * (t - 300)) - exp(-ka * (t - 300))), 0)
    inf <- ifelse(t >= 600, 0.5 / CL * (1 - exp(-k * (t - 600))), 0)
    iv + po + inf
  }
  expect_equal(out$Cp, exact(out$Time), tolerance = 1e-6)
  expect_true(all(is.na(out$Ce)))
  expect_true(all(is.na(out$Recovery)))
})

test_that("an intravenous bolus follows the implicit analytical solution", {
  # t = (C0 - C) V / Vmax + Km V / Vmax ln(C0 / C)
  V <- 50; vmax <- 0.35; km <- 5
  PK <- mmPK(v = V, vmax = vmax, km = km)
  out <- advanceMichaelisMenten(mmDose(0, 1000, "mg"), PK, 7 * 1440)
  C0 <- 1000 / V
  keep <- out$Cp > 0.05
  tExact <- (C0 - out$Cp[keep]) * V / vmax + km * V / vmax * log(C0 / out$Cp[keep])
  expect_equal(out$Time[keep], tExact, tolerance = 1e-5)
})

test_that("a constant infusion approaches Css = Km R / (Vmax - R)", {
  V <- 50; vmax <- 0.35; km <- 5; R <- 0.25
  PK <- mmPK(v = V, vmax = vmax, km = km)
  out <- advanceMichaelisMenten(mmDose(0, R, "mg/min"), PK, 60 * 1440)
  expect_equal(tail(out$Cp, 1), km * R / (vmax - R), tolerance = 1e-4)
  # disproportionate: 20% more rate gives far more than 20% more concentration
  out2 <- advanceMichaelisMenten(mmDose(0, 1.2 * R, "mg/min"), PK, 60 * 1440)
  expect_gt(tail(out2$Cp, 1) / tail(out$Cp, 1), 2)
})

test_that("a prodrug in equivalents converts first order and keeps its mass", {
  V <- 50; km <- 1e7; CL <- 1e-12
  kc <- log(2) / 15
  PK <- mmPK(v = V, vmax = CL * km, km = km, kConversion = kc, ka_IM = 0.03,
             saltFactor = list(PE = 0.92, IV = 0.92))
  out <- advanceMichaelisMenten(mmDose(0, 1000, "mg PE"), PK, 600)
  # no elimination: central amount = 0.92 x 1000 x (1 - exp(-kc t))
  expect_equal(out$Cp * V, 920 * (1 - exp(-kc * out$Time)), tolerance = 1e-6)
  im <- advanceMichaelisMenten(mmDose(0, 1000, "mg PE IM"), PK, 2000)
  expect_equal(tail(im$Cp, 1) * V, 920, tolerance = 1e-4)
  inf <- advanceMichaelisMenten(mmDose(c(0, 10), c(100, 0), c("mg PE/min", "mg PE/min")),
                                PK, 2000)
  expect_equal(tail(inf$Cp, 1) * V, 920, tolerance = 1e-4)
})

test_that("each oral formulation has its own depot, salt factor and absorption", {
  V <- 50; km <- 1e7; CL <- 1e-12
  PK <- mmPK(v = V, vmax = CL * km, km = km, ka = 0.02, F = 0.9,
             saltFactor = list(default = 0.92, liquid = 1),
             oralFormulations = list(liquid = list(default = list(
               ka_PO = 0.05, bioavailability_PO = 1, tlag_PO = 0))))
  out <- advanceMichaelisMenten(mmDose(c(0, 0), c(100, 100), c("mg PO", "mg PO liquid")),
                                PK, 3000)
  expect_equal(tail(out$Cp, 1) * V, 100 * 0.9 * 0.92 + 100, tolerance = 1e-4)
  t <- out$Time
  expect_equal(out$Cp * V,
               82.8 * (1 - exp(-0.02 * t)) + 100 * (1 - exp(-0.05 * t)),
               tolerance = 1e-6)
})

test_that("units it cannot represent are refused", {
  PK <- mmPK()
  expect_error(advanceMichaelisMenten(mmDose(0, 10, "mg IN"), PK, 100), "cannot simulate")
})

# -----------------------------------------------------------------------------
# The optional peripheral compartment (k23/k32), added for GHB.  Every drug
# that declares neither must be unchanged, which the tests above still pin;
# these check that the new states are right when a drug does declare them.
# Claude Code, 2026-10-17, at the request of Steven L. Shafer.
# -----------------------------------------------------------------------------

test_that("declaring neither rate constant leaves the peripheral compartment empty", {
  # The same model with and without the fields present but NULL: identical.
  V <- 40; CL <- 0.05; km <- 1e7
  dose <- mmDose(c(0, 300), c(100, 50), c("mg", "mg PO"))
  bare <- advanceMichaelisMenten(dose, mmPK(v = V, vmax = CL * km, km = km), 900)
  nulls <- advanceMichaelisMenten(
    dose, mmPK(v = V, vmax = CL * km, km = km, k23 = NULL, k32 = NULL), 900)
  expect_identical(bare$Cp, nulls$Cp)
})

test_that("with Km far above the concentrations it is the linear TWO-compartment model", {
  # The exact bi-exponential solution of an intravenous bolus, written here
  # from the micro constants rather than taken from the engine.
  V <- 30; CL <- 1.2; km <- 1e9; k12 <- 0.0016; k21 <- 0.0089; D <- 2000
  k10 <- CL / V
  s <- k10 + k12 + k21
  alpha <- (s + sqrt(s^2 - 4 * k10 * k21)) / 2
  beta  <- (s - sqrt(s^2 - 4 * k10 * k21)) / 2
  exact <- function(t) D / V * ((k21 - alpha) / (beta - alpha) * exp(-alpha * t) +
                                (k21 - beta) / (alpha - beta) * exp(-beta * t))

  r <- advanceMichaelisMenten(mmDose(0, D, "mg"),
                              mmPK(v = V, vmax = CL * km, km = km,
                                   k23 = k12, k32 = k21), 600)
  keep <- r$Time > 0
  expect_equal(r$Cp[keep], exact(r$Time[keep]), tolerance = 1e-4)
  # And it is genuinely two-compartment: dropping the peripheral compartment
  # changes the curve, most visibly late, when drug returns from it.
  one <- advanceMichaelisMenten(mmDose(0, D, "mg"),
                                mmPK(v = V, vmax = CL * km, km = km), 600)
  late <- r$Time > 120
  expect_gt(max(abs(one$Cp[late] - r$Cp[late]) / r$Cp[late]), 0.2)
})

test_that("the peripheral compartment conserves mass and equilibrates at k23/k32", {
  # With no elimination at all, the whole dose must end up distributed in the
  # ratio the rate constants set, and none of it may vanish.
  V <- 25; k23 <- 0.01; k32 <- 0.04; D <- 500
  r <- advanceMichaelisMenten(mmDose(0, D, "mg"),
                              mmPK(v = V, vmax = 1e-12, km = 1e6,
                                   k23 = k23, k32 = k32), 3000)
  central <- r$Cp[nrow(r)] * V
  # Ap/Ac -> k23/k32 at equilibrium, so Ac -> D * k32 / (k23 + k32)
  expect_equal(central, D * k32 / (k23 + k32), tolerance = 1e-3)
})

test_that("a peripheral compartment needs both rate constants, both positive", {
  expect_error(validateMichaelisMenten(list(vmax = 1, km = 1, k23 = 0.01), "x"),
               "needs both k23 and k32")
  expect_error(validateMichaelisMenten(list(vmax = 1, km = 1, k32 = 0.01), "x"),
               "needs both k23 and k32")
  expect_error(
    validateMichaelisMenten(list(vmax = 1, km = 1, k23 = 0, k32 = 0.01), "x"),
    "k23 must be positive")
  expect_error(
    validateMichaelisMenten(list(vmax = 1, km = 1, k23 = 0.01, k32 = -1), "x"),
    "k32 must be positive")
  expect_silent(
    validateMichaelisMenten(list(vmax = 1, km = 1, k23 = 0.01, k32 = 0.04), "x"))
})
