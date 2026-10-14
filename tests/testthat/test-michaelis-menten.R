# The numerical Michaelis-Menten engine, R/advanceMichaelisMenten.R.
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.

mmPK <- function(v = 50, vmax = 0.5, km = 5, ka = 0.01, F = 1, tlag = 0,
                 kConversion = NULL, ka_IM = NULL, saltFactor = list(),
                 oralFormulations = NULL)
{
  list(
    PK = list(default = list(v1 = v, ka_PO = ka, bioavailability_PO = F,
                             tlag_PO = tlag)),
    oralFormulations = oralFormulations,
    michaelisMenten = list(vmax = vmax, km = km, kConversion = kConversion,
                           ka_IM = ka_IM, saltFactor = saltFactor)
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
