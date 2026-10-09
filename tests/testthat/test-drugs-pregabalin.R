# pregabalin: see the header of R/drugs_pregabalin.R for the model, how Chan's
# Table 2 was read, and where the effect site comes from.  Pins were worked out
# from the published numbers by hand (plain R, with the fat-free mass and the
# Cockcroft-Gault and Du Bois formulas written out again), not from the code
# under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

pregabalinDose <- function(time, dose, units = "mg PO") {
  data.frame(Drug = "pregabalin", Time = time, Dose = dose, Units = units)
}

oralPregabalin <- function(dt, maximum = 1440, age = 35, adjustToFFM = FALSE,
                           creatinine = NULL) {
  simulateDrugsWithCovariates(
    dt, noEvents, 70, 170, age, "male", maximum, FALSE,
    adjustToFFM = adjustToFFM, creatinine = creatinine
  )$pregabalin$wide
}

# The one-compartment oral solution and its effect site, written out
# independently of the engine.  t and lag in minutes, k, ka and ke0 per minute.
oneCompOral <- function(t, D, V, k, ka, lag) {
  s <- pmax(t - lag, 0)
  D / V * ka / (ka - k) * (exp(-k * s) - exp(-ka * s))
}
oneCompOralCe <- function(t, D, V, k, ka, lag, ke0) {
  s <- pmax(t - lag, 0)
  A <- D / V * ka / (ka - k)
  A * ke0 * (exp(-k * s) / (ke0 - k) - exp(-ka * s) / (ke0 - ka) -
               (1 / (ke0 - k) - 1 / (ke0 - ka)) * exp(-ke0 * s))
}


test_that("the reference patient receives Chan's typical parameters", {
  # 70 kg, 170 cm, 35 y man at the assumed creatinine of 1.0 mg/dL:
  # Cockcroft-Gault 102.08 mL/min, Du Bois 1.8097 m^2, NCLcr 97.59 mL/min/1.73
  # m^2, above the breakpoint of 96.4, so CL/F is 4.96 L/h and V/F 39.8 L.
  # His pharmacokinetic weight is 70 kg, so the switch changes nothing.
  for (adjust in c(TRUE, FALSE)) {
    actual <- pregabalin(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 39.8, v2 = 1, v3 = 1,
        cl1 = 4.96 / 60, cl2 = 0, cl3 = 0,
        ka_PO = 10 / 60,
        bioavailability_PO = 1,
        tlag_PO = 19.2
      )),
      tPeak = 283,
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = 2.7,
      upperTypical = 5.4,
      lowerTypical = 1.3,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
})


test_that("Chan's weight terms run on the pharmacokinetic weight with the switch on", {
  # 120 kg, 170 cm, 50 y man.  Switch off, at total weight: Cockcroft-Gault
  # 150 mL/min, NCLcr 114.0 (above the breakpoint), so CL/F = 4.96 x
  # (120/70)^0.52 and V/F = 39.8 x (120/70)^0.70.
  off <- pregabalin(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off[c("v1", "cl1")],
                       list(v1 = 58.0418593288, cl1 = 0.1094091624))

  # Switch on, at the pharmacokinetic weight 91.343 kg: Cockcroft-Gault
  # 114.18 mL/min, NCLcr 97.48, still above the breakpoint.
  on <- pregabalin(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "cl1")],
                       list(v1 = 47.9500078081, cl1 = 0.0949361764))
})


test_that("a woman takes Chan's sex terms on clearance and volume", {
  # 60 kg, 160 cm, 40 y woman at the assumed creatinine of 0.8 mg/dL.  Switch
  # off: Cockcroft-Gault 88.54 mL/min, NCLcr 94.43, below the breakpoint.
  off <- pregabalin(60, 160, 40, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off[c("v1", "cl1")],
                       list(v1 = 29.6550330039, cl1 = 0.0687630438))

  # Switch on, at the pharmacokinetic weight 49.999 kg (Al-Sallami's female
  # maturation term is 1.014 at 40 y, not 1).
  on <- pregabalin(60, 160, 40, "female")$PK$default
  expect_equal_rounded(on[c("v1", "cl1")],
                       list(v1 = 26.1015390612, cl1 = 0.0563174710))
})


test_that("clearance is proportional to creatinine clearance up to the breakpoint", {
  # 40 y man: NCLcr 92.94 at 1.0 mg/dL, below the breakpoint, so doubling the
  # creatinine halves clearance and doubles the half-life.
  normal <- pregabalin(70, 170, 40, "male", adjustToFFM = FALSE, creatinine = 1)
  raised <- pregabalin(70, 170, 40, "male", adjustToFFM = FALSE, creatinine = 2)
  expect_equal(normal$PK$default$cl1, 0.0796996805, tolerance = 1e-8)
  expect_equal(raised$PK$default$cl1, normal$PK$default$cl1 / 2)

  # Above the breakpoint clearance stops rising: a creatinine of 0.6 gives
  # the 35 y man an NCLcr of 163, and his clearance stays at 4.96 L/h.
  low <- pregabalin(70, 170, 35, "male", adjustToFFM = FALSE, creatinine = 0.6)
  expect_equal(low$PK$default$cl1, 4.96 / 60)
})


test_that("pregabalin is offered orally only, in the startup menu under Oral analgesics", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "pregabalin"], ",")[[1]]
  expect_equal(units, c("mg PO", paste("mg PO", names(SCHEDULE_INTERVALS))))
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "pregabalin"]))
  expect_true(is.na(dd$Infusion.Units[dd$Drug == "pregabalin"]))
  expect_equal(dd$Category[dd$Drug == "pregabalin"], "Oral analgesics")
})


test_that("the CSV row agrees with the drug function", {
  # The plot reads the band and MEAC from the CSV, not from the function.
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "pregabalin", ]
  x   <- pregabalin(70, 170, 35, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  expect_equal(row$MEAC, 0)          # not an opioid: off the MEAC panel
  expect_equal(row$endCe, 0)
  expect_equal(row$Concentration.Units, "mcg")
  expect_equal(x$tPeak, PREGABALIN_TPEAK)
})


test_that("absorption is linear: twice the dose, twice the concentration", {
  # No saturation, unlike gabapentin: no oralSaturation block, and 300 mg is
  # exactly twice 150 mg at every time, plasma and effect site.
  expect_null(getDrugPK("pregabalin", 70, 170, 35, "male")$oralSaturation)
  a <- oralPregabalin(pregabalinDose(0, 150))
  b <- oralPregabalin(pregabalinDose(0, 300))
  expect_equal(a$Time, b$Time)
  expect_equal(b$Plasma, 2 * a$Plasma)
  expect_equal(b$"Effect Site", 2 * a$"Effect Site")
})


test_that("the simulation is the one-compartment oral solution, with its effect site", {
  # 150 mg, then a 75 mg twice-daily schedule from 12 h (repeats at 24 and
  # 36 h), checked at the simulation's own time points against the solution
  # written out above.  ke0 is the one getDrugPK() solved; everything else is
  # Chan's typical value.
  w  <- oralPregabalin(
    rbind(pregabalinDose(0, 150), pregabalinDose(720, 75, "mg PO bid")),
    maximum = 2880
  )
  ke0 <- getDrugPK("pregabalin", 70, 170, 35, "male", adjustToFFM = FALSE)$PK$default$ke0
  V   <- 39.8
  k   <- 4.96 / 60 / V
  ka  <- 10 / 60
  lag <- 19.2
  doses <- data.frame(t0 = c(0, 720, 1440, 2160), D = c(150, 75, 75, 75))
  cp <- ce <- 0
  for (i in seq_len(nrow(doses))) {
    t <- w$Time - doses$t0[i]
    cp <- cp + ifelse(t > 0, oneCompOral(t, doses$D[i], V, k, ka, lag), 0)
    ce <- ce + ifelse(t > 0, oneCompOralCe(t, doses$D[i], V, k, ka, lag, ke0), 0)
  }
  expect_equal(w$Plasma, cp, tolerance = 1e-8)
  expect_equal(w$"Effect Site", ce, tolerance = 1e-8)
})


test_that("the effect site peaks 283 min after an oral dose, lag included", {
  # ke0 is solved against the oral curve with the 19.2 min lag counted, which
  # gives van Esdonk's 0.39 /h back for the reference patient.  Without the
  # lag in the solve the peak would fall at 302 min.
  pk  <- getDrugPK("pregabalin", 70, 170, 35, "male")
  d   <- pk$PK$default
  expect_equal(d$ke0 * 60, 0.390778, tolerance = 1e-4)

  k <- d$cl1 / d$v1
  ce <- function(t) oneCompOralCe(t, 150, d$v1, k, d$ka_PO, d$tlag_PO, d$ke0)
  peak <- stats::optimize(ce, c(0, 1000), maximum = TRUE)$maximum
  expect_equal(peak, 283, tolerance = 1e-4)

  # The same holds for any patient: ke0 changes so that the peak does not.
  # A 40 y man with a creatinine of 2.0 mg/dL clears pregabalin at half the
  # rate.
  pk2 <- getDrugPK("pregabalin", 70, 170, 40, "male", creatinine = 2)
  d2  <- pk2$PK$default
  k2  <- d2$cl1 / d2$v1
  peak2 <- stats::optimize(
    function(t) oneCompOralCe(t, 150, d2$v1, k2, d2$ka_PO, d2$tlag_PO, d2$ke0),
    c(0, 1000), maximum = TRUE
  )$maximum
  expect_equal(peak2, 283, tolerance = 1e-4)
})


test_that("the model reproduces the single-dose data it is checked against", {
  # The reference patient (Chan's typical subject): 150 mg peaks at 3.57
  # mcg/mL at 0.764 h (observed 3.85-4.65 at about 1 h), AUC 30.2 mcg.h/mL
  # (observed 23.7-29.8), half-life 5.56 h (label 6.3).  Analytically, off
  # the parameters rather than the plotted grid.
  d   <- getDrugPK("pregabalin", 70, 170, 35, "male")$PK$default
  k   <- d$cl1 / d$v1
  cp  <- function(t) oneCompOral(t, 150, d$v1, k, d$ka_PO, d$tlag_PO)
  top <- stats::optimize(cp, c(0, 300), maximum = TRUE)
  expect_equal(top$maximum / 60, 0.764035, tolerance = 1e-5)
  expect_equal(top$objective, 3.565952, tolerance = 1e-5)
  expect_equal(150 / (d$cl1 * 60), 30.24194, tolerance = 1e-6)
  expect_equal(log(2) / k / 60, 5.561947, tolerance = 1e-6)

  # Mueller 2026 gave 300 mg two hours before induction to women of about
  # 65 kg, 162 cm and 49 y, and measured 7.67 +/- 3.00 mcg/mL at incision.
  # Total weight, as in the source.
  w  <- getDrugPK("pregabalin", 65, 161.9, 49, "female", adjustToFFM = FALSE)$PK$default
  kw <- w$cl1 / w$v1
  expect_equal(oneCompOral(120, 300, w$v1, kw, w$ka_PO, w$tlag_PO), 7.795168,
               tolerance = 1e-5)
})


test_that("the linear-absorption scenario says what the model does", {
  # inst/help/scenarios/pregabalin-linear-absorption.md quotes these numbers;
  # if the model changes, the narrative has to change with it.  Worked out by
  # hand: 40 y, 70 kg man, NCLcr 92.9 (below the breakpoint), CL/F 4.78 L/h,
  # half-life 5.77 h.
  s <- helpScenarioById("pregabalin-linear-absorption")
  p <- s$patient
  pk <- getDrugPK("pregabalin", p$weight, p$height, p$age, p$sex)
  d  <- pk$PK$default
  k  <- d$cl1 / d$v1
  expect_equal(log(2) / k / 60, 5.769, tolerance = 1e-3)

  cp <- function(t, D) oneCompOral(t, D, d$v1, k, d$ka_PO, d$tlag_PO)
  ce <- function(t, D) oneCompOralCe(t, D, d$v1, k, d$ka_PO, d$tlag_PO, d$ke0)
  both   <- function(f) function(t) f(t, 300) + f(t + 2160, 150)
  first  <- stats::optimize(function(t) cp(t, 150), c(0, 300), maximum = TRUE)
  second <- stats::optimize(both(cp), c(0, 300), maximum = TRUE)
  expect_equal(c(first$objective, second$objective), c(3.5715, 7.1909), tolerance = 1e-4)
  expect_equal(second$objective / first$objective, 2.013, tolerance = 1e-3)
  expect_equal(first$maximum, 46.05, tolerance = 1e-3)           # minutes

  # The effect site: 4.7 h after each dose, 2.2 and 4.5 mcg/mL
  e1 <- stats::optimize(function(t) ce(t, 150), c(0, 1000), maximum = TRUE)
  e2 <- stats::optimize(both(ce), c(0, 1000), maximum = TRUE)
  expect_equal(e1$maximum, 283, tolerance = 1e-4)
  expect_equal(c(e1$objective, e2$objective), c(2.2492, 4.5411), tolerance = 1e-4)

  # One and two hours after 300 mg: plasma 7.0, effect site a third and 71%
  # of its own peak
  expect_equal(cp(60, 300), 7.0223, tolerance = 1e-4)
  expect_equal(ce(c(60, 120), 300) / ce(283, 300), c(0.3338, 0.7077), tolerance = 1e-3)

  # The simulation agrees with the analytic curves at its own time points
  out <- simulateDrugsWithCovariates(
    s$doses, noEvents, p$weight, p$height, p$age, p$sex,
    s$options$maximum, FALSE, adjustToFFM = p$adjustToFFM
  )$pregabalin$wide
  expect_equal(out$Plasma, cp(out$Time, 150) + cp(out$Time - 2160, 300), tolerance = 1e-8)

  # Try next: a creatinine of 2.0 mg/dL doubles the half-life and leaves 0.44
  # mcg/mL at 36 h; 150 mg twice daily peaks at 4.68, 1.31 times the first.
  d2 <- getDrugPK("pregabalin", p$weight, p$height, p$age, p$sex, creatinine = 2)$PK$default
  k2 <- d2$cl1 / d2$v1
  expect_equal(log(2) / k2 / 60, 11.538, tolerance = 1e-4)
  expect_equal(oneCompOral(2160, 150, d2$v1, k2, d2$ka_PO, d2$tlag_PO), 0.4446, tolerance = 1e-3)
  bid <- function(t) cp(t, 150) + cp(t - 720, 150) + cp(t - 1440, 150) + cp(t - 2160, 150)
  fourth <- stats::optimize(bid, c(2160, 2400), maximum = TRUE)$objective
  expect_equal(fourth, 4.6784, tolerance = 1e-4)
  expect_equal(fourth / first$objective, 1.31, tolerance = 1e-2)
})
