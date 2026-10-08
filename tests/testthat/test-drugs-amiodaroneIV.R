# amiodaroneIV: acute intravenous amiodarone.  See the header of
# R/drugs_amiodaroneIV.R for what the model is and what it leaves out.
#
# Every expectation is derived independently of the code under test: the pins
# are Korth-Bradley's per-kilogram parameters multiplied out by hand (L/kg and
# L/h/kg times 70 kg, hours to minutes), the fat-free-mass factors are those
# worked out by hand for the other drug tests, and the half-lives and the
# concentrations come from the two-compartment rate equations written out
# below, solved with a matrix exponential (eigendecomposition), sharing no
# code with the package's closed form.  Engine values are read at the end of
# a run, where every grid has a point, or compared point for point with the
# independent solution at the engine's own times.
#
# (Claude Code, 2026-10-08, at the request of Steven L. Shafer; run on
# R 4.3.3.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

KORTH_BRADLEY_REFERENCE <- paste0(
  "Korth-Bradley JM, Rose GM, de Vane PJ, Peters J, Chiang ST. Population ",
  "pharmacokinetics of intravenous amiodarone in patients with refractory ",
  "ventricular tachycardia/fibrillation. J Clin Pharmacol 1996;36:715-719. ",
  "https://pubmed.ncbi.nlm.nih.gov/8877675/"
)

# --- Independent reference: the two-compartment rate equations --------------
#
# Amounts (mg) in the central and peripheral compartments, from the abstract's
# per-kilogram values at `weight` (L/kg, L/h/kg), scaled per kilogram as the
# published model is.  Input is a constant rate into the central compartment.
kbSystem <- function(weight = 70) {
  v1 <- 0.30 * weight; v2 <- 10.0 * weight
  cl <- 0.22 * weight / 60; q <- 0.71 * weight / 60
  K <- matrix(0, 2, 2)
  K[1, 1] <- -(cl + q) / v1; K[1, 2] <- q / v2
  K[2, 1] <- q / v1;         K[2, 2] <- -q / v2
  e <- eigen(K)
  list(K = K, V = e$vectors, Vi = solve(e$vectors), lambda = e$values,
       v1 = v1, cl = cl)
}

# Amounts after h minutes at a constant input `rate` (mg/min), starting at x:
# x(h) = e^(Kh) x + K^-1 (e^(Kh) - I) b rate, with b the central compartment.
kbStep <- function(S, x, rate, h) {
  E <- Re(S$V %*% diag(exp(S$lambda * h)) %*% S$Vi)
  as.vector(E %*% x + solve(S$K, (E - diag(2)) %*% c(1, 0)) * rate)
}

# The amounts at time `at` (min) after a sequence of rate changes: rates[i]
# mg/min from times[i], plus boluses (mg) at bolusTimes.
kbState <- function(S, times, rates, at, bolusTimes = numeric(0), boluses = numeric(0)) {
  knots <- sort(unique(c(0, times, bolusTimes)))
  knots <- knots[knots < at]
  x <- c(0, 0)
  for (i in seq_along(knots)) {
    x[1] <- x[1] + sum(boluses[bolusTimes == knots[i]])
    running <- times <= knots[i]
    rate <- if (any(running)) rates[max(which(running))] else 0
    end <- if (i < length(knots)) knots[i + 1] else at
    x <- kbStep(S, x, rate, end - knots[i])
  }
  x
}

kbConcentration <- function(S, times, rates, at, ...) {
  vapply(at, function(a) kbState(S, times, rates, a, ...)[1] / S$v1, numeric(1))
}

# The package's answer at the end of a run, with only the rows given before
# it: the end of the run is always a grid point.
engineAt <- function(DT, at, weight = 70, adjustToFFM = FALSE) {
  o <- simulateDrugsWithCovariates(DT[DT$Time < at, ], noEvents, weight, 170, 50, "male",
                                   at, FALSE, adjustToFFM = adjustToFFM)
  w <- o$amiodaroneIV$wide
  w$Plasma[nrow(w)]
}

# The label's first 24 hours: 150 mg over 10 minutes, 360 mg over 6 hours,
# then 0.5 mg/min (540 mg over the remaining 18 hours)
LABEL_TIMES <- c(0, 10, 370)
LABEL_RATES <- c(15, 1, 0.5)
labelRegimen <- data.frame(Drug = "amiodaroneIV", Time = LABEL_TIMES, Dose = LABEL_RATES,
                           Units = "mg/min")


# --- The model ----------------------------------------------------------------

test_that("amiodaroneIV returns the published per-kilogram model with the switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- amiodaroneIV(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 21,                # 0.30 L/kg
        v2 = 700,               # 10.0 L/kg
        v3 = 1,
        cl1 = 0.2566666667,     # 0.22 L/h/kg = 15.4 L/h
        cl2 = 0.8283333333,     # 0.71 L/h/kg = 49.7 L/h
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = KORTH_BRADLEY_REFERENCE
  )
  expect_equal_rounded(actual, expected)

  # Per kilogram, and nothing else: with the switch off only weight matters
  expect_equal(amiodaroneIV(70, 150, 80, "female", adjustToFFM = FALSE), actual)
  expect_equal_rounded(amiodaroneIV(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default,
                       list(v1 = 36, v2 = 1200, v3 = 1, cl1 = 0.44, cl2 = 1.42, cl3 = 0))
})

test_that("the reference man receives the published values with the switch on", {
  # 70 kg, 170 cm, 35 y male: fat-free-mass factors exactly 1
  expect_equal_rounded(amiodaroneIV(70, 170, 35, "male")$PK$default, list(
    v1 = 21, v2 = 700, v3 = 1, cl1 = 0.2566666667, cl2 = 0.8283333333, cl3 = 0
  ))
})

test_that("amiodaroneIV scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the Al-Sallami formula by hand)
  actual <- amiodaroneIV(120, 170, 50, "male")
  expect_equal_rounded(actual$PK$default, list(
    v1 = 27.4030407, v2 = 913.43469, v3 = 1,
    cl1 = 0.3133675673, cl2 = 1.011322604, cl3 = 0
  ))
})

test_that("the citation is Korth-Bradley's, distinct from the long-term entry's", {
  expect_identical(amiodaroneIV(70, 170, 50, "male")$reference, KORTH_BRADLEY_REFERENCE)
  expect_match(KORTH_BRADLEY_REFERENCE, "https://pubmed.ncbi.nlm.nih.gov/8877675/$")
  expect_false(identical(KORTH_BRADLEY_REFERENCE, amiodarone(70, 170, 50, "male")$reference))
})


# --- Half-lives, and what separates it from the long-term entry -------------

test_that("the half-lives are 13.2 minutes and 42.0 hours", {
  # From the quadratic for a two-compartment model, by hand
  PK <- getDrugPK("amiodaroneIV", 70, 170, 50, "male", getDrugDefaults("amiodaroneIV"),
                  adjustToFFM = FALSE)
  d <- PK$PK$default
  k10 <- 0.22 / 0.30 / 60; k12 <- 0.71 / 0.30 / 60; k21 <- 0.71 / 10 / 60
  a <- k10 + k12 + k21
  lambda <- (a + c(1, -1) * sqrt(a^2 - 4 * k10 * k21)) / 2
  expect_equal(c(d$lambda_1, d$lambda_2), lambda, tolerance = 1e-10)
  expect_equal(log(2) / d$lambda_1, 13.18399239, tolerance = 1e-8)
  expect_equal(log(2) / d$lambda_2 / 60, 41.99479387, tolerance = 1e-8)
  expect_equal(d$lambda_3, 0)
  # The independent system has the same eigenvalues
  expect_equal(sort(-kbSystem()$lambda), sort(lambda), tolerance = 1e-10)

  # No effect site and no metabolite
  expect_equal(d$ke0, 0)
  expect_null(PK$metaboliteName)
  expect_null(d$metabolite)
})

test_that("no bioavailability reconciles it with the long-term oral entry", {
  # The oral model's parameters are apparent, CL1/F = 229 L/day, so its true
  # clearance is 229 x F, at most 229 L/day.  This one's is 369.6 L/day at
  # 70 kg, and its central volume 21 L against 882 x F.
  iv <- amiodaroneIV(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  po <- amiodarone(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(iv$cl1 * MINS_PER_DAY, 369.6, tolerance = 1e-10)
  expect_equal(po$cl1 * MINS_PER_DAY, 229, tolerance = 1e-10)
  for (F in c(0.3, 0.5, 0.8, 1)) expect_gt(iv$cl1, po$cl1 * F)
  # Not a long-term drug: no offer to switch the plot to a year
  expect_false("amiodaroneIV" %in% LONG_TERM_DRUGS)
})


# --- The label regimen through the engine -----------------------------------

test_that("the label regimen follows the rate equations at every engine time", {
  o <- simulateDrugsWithCovariates(labelRegimen, noEvents, 70, 170, 50, "male",
                                   1440, FALSE, adjustToFFM = FALSE)
  w <- o$amiodaroneIV$wide
  expected <- kbConcentration(kbSystem(), LABEL_TIMES, LABEL_RATES, w$Time)
  expect_equal(w$Plasma, expected, tolerance = 1e-8)
  # and at the end of each step
  for (at in c(10, 370, 1440)) {
    expect_equal(engineAt(labelRegimen, at),
                 kbConcentration(kbSystem(), LABEL_TIMES, LABEL_RATES, at),
                 tolerance = 1e-8, label = paste("minute", at))
  }
})

test_that("the label regimen gives the values the help pages quote", {
  S <- kbSystem()
  cp <- function(t) kbConcentration(S, LABEL_TIMES, LABEL_RATES, t)
  # 5.6 mg/L at the end of the rapid load, 1.38 at an hour, 1.11 at its low
  # on 1 mg/min, 1.29 when the rate is halved, 0.94 at 12 h, 1.12 at 24 h
  expect_equal(round(cp(c(10, 60, 120, 370, 720, 1440)), 2),
               c(5.58, 1.38, 1.11, 1.29, 0.94, 1.12))
  low <- stats::optimize(cp, c(60, 370))
  expect_equal(round(low$objective, 2), 1.11)
  expect_equal(round(low$minimum / 60), 2)
  # The dip after the rate is halved: 0.87 mg/L at about 7 h 33 min, below
  # 1.0 mg/L from 6.5 to 15.6 h
  trough <- stats::optimize(cp, c(380, 700))
  expect_equal(round(trough$objective, 2), 0.87)
  expect_equal(round(trough$minimum), 453)
  below <- c(stats::uniroot(function(t) cp(t) - 1, c(370, 453), tol = 1e-8)$root,
             stats::uniroot(function(t) cp(t) - 1, c(453, 1440), tol = 1e-8)$root)
  expect_equal(round(below / 60, 1), c(6.5, 15.6))
  # From the first hour to the 24th: 0.87 to 1.38 mg/L
  hourly <- cp(seq(60, 1440, by = 1))
  expect_equal(round(range(hourly), 2), c(0.87, 1.38))
  # First below 2.5 mg/L at about half an hour
  expect_equal(round(stats::uniroot(function(t) cp(t) - 2.5, c(10, 60))$root), 32)

  # Where the 1045 mg given by 24 h is: 24 mg central, 611 mg peripheral,
  # 410 mg eliminated
  x <- kbState(S, LABEL_TIMES, LABEL_RATES, 1440)
  given <- 150 + 360 + 0.5 * (1440 - 370)
  expect_equal(given, 1045)
  expect_equal(round(c(x, given - sum(x))), c(24, 611, 410))

  # Run on at 0.5 mg/min: 1.39 at 48 h, 1.57 at 72 h, towards 30 mg/h over
  # 15.4 L/h = 1.95 mg/L, 90% of which is reached within five days
  expect_equal(round(cp(c(2880, 4320)), 2), c(1.39, 1.57))
  expect_equal(round(30 / 15.4, 2), 1.95)
  expect_gt(cp(5 * MINS_PER_DAY), 0.9 * 30 / 15.4)
  expect_lt(cp(4.5 * MINS_PER_DAY), 0.9 * 30 / 15.4)
  # And through the engine
  expect_equal(round(engineAt(labelRegimen, 4320), 2), 1.57)
})

test_that("the scenario's Try next values are the model's", {
  S <- kbSystem()
  # A supplemental 150 mg over 10 minutes at 720 (15 mg/min on top of 0.5):
  # peak 6.5 mg/L at 730, 1.5 mg/L at 780, 1.22 at 24 h
  times <- c(LABEL_TIMES, 720, 730); rates <- c(LABEL_RATES, 15.5, 0.5)
  expect_equal(round(kbConcentration(S, times, rates, c(730, 780, 1440)), c(1, 1, 2)),
               c(6.5, 1.5, 1.22))
  # Entered as two rows at 720, which add up, and one at 730, through the engine
  extra <- rbind(labelRegimen,
                 data.frame(Drug = "amiodaroneIV", Time = c(720, 720, 730),
                            Dose = c(15, 0.5, 0.5), Units = "mg/min"))
  expect_equal(engineAt(extra, 730), kbConcentration(S, times, rates, 730), tolerance = 1e-8)
  expect_equal(engineAt(extra, 1440), kbConcentration(S, times, rates, 1440), tolerance = 1e-8)
  # As a 150 mg bolus at 720 instead: 8.1 mg/L
  bolus <- rbind(labelRegimen, data.frame(Drug = "amiodaroneIV", Time = 720, Dose = 150,
                                          Units = "mg"))
  afterBolus <- kbState(S, LABEL_TIMES, LABEL_RATES, 720)[1] / S$v1 + 150 / S$v1
  expect_equal(round(afterBolus, 1), 8.1)
  o <- simulateDrugsWithCovariates(bolus, noEvents, 70, 170, 50, "male", 1440, FALSE,
                                   adjustToFFM = FALSE)
  w <- o$amiodaroneIV$wide
  expect_equal(w$Plasma[w$Time == 720], afterBolus, tolerance = 1e-8)

  # A 120 kg man: peak, dip and 24 h, with fat-free-mass scaling (factors
  # 1.3049067 and 1.2209126) and per kilogram
  heavy <- function(volume, clearance) {
    v1 <- 21 * volume; v2 <- 700 * volume
    cl <- 15.4 / 60 * clearance; q <- 49.7 / 60 * clearance
    K <- matrix(c(-(cl + q) / v1, q / v1, q / v2, -q / v2), 2, 2)
    e <- eigen(K)
    list(K = K, V = e$vectors, Vi = solve(e$vectors), lambda = e$values, v1 = v1)
  }
  for (case in list(list(S = heavy(1.3049067, 1.2209126), ffm = TRUE, want = c(4.3, 0.69, 0.89)),
                    list(S = heavy(120 / 70, 120 / 70),   ffm = FALSE, want = c(3.3, 0.51, 0.66)))) {
    cp <- function(t) kbConcentration(case$S, LABEL_TIMES, LABEL_RATES, t)
    got <- c(cp(10), stats::optimize(cp, c(380, 700))$objective, cp(1440))
    expect_equal(round(got, c(1, 2, 2)), case$want, label = paste("ffm", case$ffm))
    expect_equal(engineAt(labelRegimen, 1440, weight = 120, adjustToFFM = case$ffm), cp(1440),
                 tolerance = 1e-6, label = paste("engine, ffm", case$ffm))
  }
})

test_that("the published cross-checks the help page quotes are the model's", {
  S <- kbSystem()
  # Watt 1986: 175 mg/h for 2 h, then 50 mg/h.  The typical patient is at
  # 1.10 to 1.48 mg/L from 3 to 16 h.
  watt <- kbConcentration(S, c(0, 120), c(175 / 60, 50 / 60), seq(180, 960, by = 1))
  expect_equal(round(range(watt), 2), c(1.10, 1.48))

  # Shiga 2011: single 15-minute infusions of 1.25, 2.5 and 5 mg/kg.  Peaks
  # at the end of the infusion, 2.90, 5.81 and 11.62 mg/L, inside the reported
  # 2.92 +/- 0.61, 7.14 +/- 1.48 and 13.66 +/- 3.41; areas to 96 h 4.78, 9.56
  # and 19.12 mg.h/L against 3.6, 8.1 and 16.6.
  mgPerKg <- c(1.25, 2.5, 5)
  peak <- vapply(mgPerKg, function(D) kbConcentration(S, c(0, 15), c(D * 70 / 15, 0), 15),
                 numeric(1))
  expect_equal(round(peak, 2), c(2.90, 5.81, 11.62))
  expect_true(all(abs(peak - c(2.92, 7.14, 13.66)) <= c(0.61, 1.48, 3.41)))
  auc <- vapply(mgPerKg, function(D) {
    f <- function(t) kbConcentration(S, c(0, 15), c(D * 70 / 15, 0), t)
    (stats::integrate(f, 0, 15, rel.tol = 1e-10)$value +
       stats::integrate(f, 15, 96 * 60, rel.tol = 1e-10, subdivisions = 1000)$value) / 60
  }, numeric(1))
  expect_equal(round(auc, 2), c(4.78, 9.56, 19.12))
  expect_equal(round(auc / c(3.6, 8.1, 16.6) - 1, 2), c(0.33, 0.18, 0.15))

  # Through the engine: 5 mg/kg for a 70 kg man as 350 mg over 15 minutes
  shiga <- data.frame(Drug = "amiodaroneIV", Time = c(0, 15), Dose = c(350 / 15, 0),
                      Units = "mg/min")
  expect_equal(engineAt(shiga, 15), peak[3], tolerance = 1e-8)
})


# --- Units, and the library row ---------------------------------------------

test_that("the library offers amiodaroneIV as intravenous boluses and infusions only", {
  dd <- getDrugDefaultsGlobal()
  a <- dd[dd$Drug == "amiodaroneIV", ]
  expect_equal(nrow(a), 1)
  units <- a$Units[[1]]
  expect_setequal(units, c("mg", "mg/kg", "mg/min", "mg/hr"))
  expect_true(all(units %in% allUnits))
  expect_true(all(doseRoute(units) == ROUTE_IV))
  expect_true(all(c("mg", "mg/kg") %in% bolusUnits))
  expect_false(any(isRateUnit(c("mg", "mg/kg"))))
  expect_true(all(c("mg/min", "mg/hr") %in% infusionUnits))
  expect_true(all(isRateUnit(c("mg/min", "mg/hr"))))
  expect_false(any(units %in% c(poUnits, poRateUnits, imUnits, inUnits, tciUnits, scheduledUnits)))
  expect_equal(c(a$Bolus.Units, a$Infusion.Units, a$Default.Units), c("mg", "mg/min", "mg"))
  expect_true(all(validateDoseTableInput(data.frame(
    Drug = "amiodaroneIV", Time = c("0", "10", "370", "720"),
    Dose = c("15", "1", "30", "5"), Units = c("mg/min", "mg/min", "mg/hr", "mg/kg")
  ))))
  # No band, no MEAC, no threshold; the model mirrors the band
  m <- amiodaroneIV(70, 170, 50, "male")
  expect_equal(c(a$Lower, a$Upper, a$Typical), c(m$lowerTypical, m$upperTypical, m$typical))
  expect_equal(c(a$Lower, a$Upper, a$Typical, a$MEAC, a$endCe), rep(0, 5))
  expect_equal(a$Concentration.Units, "mcg")
  expect_equal(a$Class, "IV")
  # A colour of its own
  others <- dd$Color[dd$Drug != "amiodaroneIV"]
  expect_false(toupper(a$Color) %in% toupper(others))
})

test_that("each unit converts as it should", {
  PK <- getDrugPK("amiodaroneIV", 70, 170, 50, "male", getDrugDefaults("amiodaroneIV"))
  PK$endCe <- 0
  perMin <- data.frame(Drug = "amiodaroneIV", Time = c(0, 360), Dose = c(1, 0), Units = "mg/min")
  perHr <- transform(perMin, Dose = c(60, 0), Units = "mg/hr")
  expect_equal(simCpCe(perMin, noEvents, PK, 720, FALSE)$wide,
               simCpCe(perHr, noEvents, PK, 720, FALSE)$wide)
  # 5 mg/kg for 70 kg is 350 mg
  perKg <- data.frame(Drug = "amiodaroneIV", Time = 0, Dose = 5, Units = "mg/kg")
  mg <- transform(perKg, Dose = 350, Units = "mg")
  expect_equal(simCpCe(perKg, noEvents, PK, 720, FALSE)$wide,
               simCpCe(mg, noEvents, PK, 720, FALSE)$wide)
  # A bolus lands in V1 at once: 350 mg / 21 L
  w <- simCpCe(mg, noEvents, PK, 720, FALSE)$wide
  expect_equal(w$Plasma[w$Time == 0], 350 / PK$PK$default$v1, tolerance = 1e-10)
})

test_that("it simulates as plasma only, with no metabolite row and no threshold", {
  o <- simulateDrugsWithCovariates(labelRegimen, noEvents, 70, 170, 50, "male", 1440, TRUE)
  expect_identical(names(o), "amiodaroneIV")
  w <- o$amiodaroneIV$wide
  expect_true(all(is.na(w$"Effect Site")))
  expect_true(all(w$Recovery == 0))
  expect_equal(max(o$amiodaroneIV$equiSpace$MEAC), 0)
  expect_null(o$amiodaroneIV$metaboliteSeries)
})

test_that("the teaching scenario is the label regimen, in hours, over 24 hours", {
  s <- helpScenarioById("amiodarone-iv-loading")
  expect_equal(s$group, "Intravenous basics")
  expect_equal(s$options$timeUnits, "hours")
  expect_equal(s$options$maximum, 1440)
  expect_equal(s$doses, labelRegimen)
  expect_length(helpScenarioCheck(s), 0)
})

test_that("the two amiodarone pages link to each other", {
  iv <- helpDrugPageHTML("amiodaroneIV")
  po <- helpDrugPageHTML("amiodarone")
  expect_match(iv, 'data-help-page="drugs/amiodarone"', fixed = TRUE)
  expect_match(po, 'data-help-page="drugs/amiodaroneIV"', fixed = TRUE)
  expect_match(iv, "first one to three days", fixed = TRUE)
  # Generated parts: intravenous, no band, no effect site, no threshold
  expect_match(iv, "<td>Intravenous</td>", fixed = TRUE)
  expect_match(iv, "None: no range applies to this model", fixed = TRUE)
  expect_match(iv, "This model has <strong>no effect site</strong>", fixed = TRUE)
  expect_match(iv, "None by default; a threshold set under Drug Thresholds is timed on the plasma",
               fixed = TRUE)
  expect_false(grepl("prodrug|no effect site of its own", iv))
  expect_equal(helpDrugTitle("amiodaroneIV"), "Amiodarone IV")
  # The citation under Model source is labelled with the title, not the name
  expect_match(iv, '<strong style="color:#C2185B;">Amiodarone IV</strong>', fixed = TRUE)
  expect_false(grepl(">amiodaroneIV<", iv, fixed = TRUE))
})
