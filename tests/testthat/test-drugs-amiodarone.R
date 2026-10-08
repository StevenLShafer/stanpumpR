# amiodarone and desethylamiodarone: see the headers of R/drugs_amiodarone.R
# and R/drugs_desethylamiodarone.R for what the models are and what they
# leave out.
#
# Every expectation is derived independently of the code under test: the pins
# are Pollak, Bouillon and Shafer's Table II converted by hand from L/day to
# L/min, the fat-free-mass factors are those worked out by hand for the other
# drug tests, and the half-lives, steady states, regimen and decrement times
# come from the compartmental rate equations written out below, solved with a
# matrix exponential (eigendecomposition), sharing no code with the package's
# closed form.  Engine values are read at the end of a run, where every grid
# has a point, or compared point for point between runs that share a grid,
# so these hold whatever the spacing of the evaluation grid.
#
# (Claude Code, 2026-10-07, at the request of Steven L.
# Shafer; run on R 4.3.3.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

POLLAK_REFERENCE <- paste0(
  "Pollak PT, Bouillon T, Shafer SL. Population pharmacokinetics of long-term ",
  "oral amiodarone therapy. Clin Pharmacol Ther 2000;67:642-652. ",
  "https://pubmed.ncbi.nlm.nih.gov/10872646/"
)

# --- Independent reference: the four-compartment rate equations -------------
#
# Amounts (mg) in amiodarone's central and peripheral compartments and in
# desethylamiodarone's, from Table II (L and L/day).  All of the parent's
# elimination is metabolite input, mass for mass (Pollak, Methods).  Input is
# a constant rate into the parent's central compartment.
pollakSystem <- function() {
  perMin <- function(x) x / 1440
  v1 <- 882;  v2 <- 12700; cl1 <- perMin(229); cl2 <- perMin(588)
  m1 <- 2790; m2 <- 7830;  mc1 <- perMin(254); mc2 <- perMin(151)
  K <- matrix(0, 4, 4)
  K[1, 1] <- -(cl1 + cl2) / v1; K[1, 2] <- cl2 / v2
  K[2, 1] <- cl2 / v1;          K[2, 2] <- -cl2 / v2
  K[3, 1] <- cl1 / v1;          K[3, 3] <- -(mc1 + mc2) / m1; K[3, 4] <- mc2 / m2
  K[4, 3] <- mc2 / m1;          K[4, 4] <- -mc2 / m2
  e <- eigen(K)
  list(K = K, V = e$vectors, Vi = solve(e$vectors), lambda = e$values,
       v1 = v1, m1 = m1)
}

# Amounts after h minutes at a constant input `rate` (mg/min), starting at x:
# x(h) = e^(Kh) x + K^-1 (e^(Kh) - I) b rate, with b the parent's central.
pollakStep <- function(S, x, rate, h) {
  E <- Re(S$V %*% diag(exp(S$lambda * h)) %*% S$Vi)
  as.vector(E %*% x + solve(S$K, (E - diag(4)) %*% c(1, 0, 0, 0)) * rate)
}

# Serum amiodarone and desethylamiodarone (mg/L) at time `at` (min) after a
# sequence of rate changes: rates[i] mg/min from times[i].
pollakConcentrations <- function(times, rates, at) {
  S <- pollakSystem()
  x <- rep(0, 4)
  for (i in seq_along(times)) {
    if (times[i] >= at) break
    end <- if (i < length(times)) min(times[i + 1], at) else at
    x <- pollakStep(S, x, rates[i], end - times[i])
  }
  c(parent = x[1] / S$v1, metabolite = x[3] / S$m1)
}

# The package's answer at the end of a run, with only the rows given before
# it: the end of the run is always a grid point.
engineAt <- function(DT, at, adjustToFFM = FALSE) {
  o <- simulateDrugsWithCovariates(DT[DT$Time < at, ], noEvents, 70, 170, 50, "male",
                                   at, FALSE, adjustToFFM = adjustToFFM)
  a <- o$amiodarone$wide
  m <- o$desethylamiodarone$wide
  c(parent = a$Plasma[nrow(a)], metabolite = m$Plasma[nrow(m)])
}

# Pollak's proposed regimen (Results): 1600 mg/d for 2 days, 1200 for 5, 1000
# for 7, 800 for 7, 600 for 7, 400 for 62, then 343 mg/d.
POLLAK_DAYS <- c(0, 2, 7, 14, 21, 28, 90)
POLLAK_MG_PER_DAY <- c(1600, 1200, 1000, 800, 600, 400, 343)
pollakRegimen <- data.frame(Drug = "amiodarone", Time = POLLAK_DAYS * MINS_PER_DAY,
                            Dose = POLLAK_MG_PER_DAY, Units = "mg/day PO")


# --- The models ---------------------------------------------------------------

test_that("amiodarone returns Pollak's Table II with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- amiodarone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 882,
        v2 = 12700,
        v3 = 1,
        cl1 = 0.1590277778,     # 229 L/day
        cl2 = 0.4083333333,     # 588 L/day
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 1.5, upperTypical = 2.5, lowerTypical = 1,
    reference = POLLAK_REFERENCE,
    prodrug = FALSE,
    metabolite = list(
      name              = "desethylamiodarone",
      kFormation        = 1.803036029e-4,   # CL1/F over V1/F, per minute
      firstPassFraction = 0,
      mwRatio           = 1
    )
  )
  expect_equal_rounded(actual, expected)

  # No covariate was significant, so with the switch off the patient's size
  # changes nothing at all.
  expect_equal(amiodarone(120, 160, 80, "female", adjustToFFM = FALSE), actual)
})

test_that("desethylamiodarone returns Pollak's Table II with the fat-free-mass switch off", {
  actual <- desethylamiodarone(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 2790,
        v2 = 7830,
        v3 = 1,
        cl1 = 0.1763888889,     # 254 L/day
        cl2 = 0.1048611111,     # 151 L/day
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = POLLAK_REFERENCE
  )
  expect_equal_rounded(actual, expected)
  expect_equal(desethylamiodarone(120, 160, 80, "female", adjustToFFM = FALSE), actual)
})

test_that("the pair scales to fat-free mass identically for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the Al-Sallami formula by hand), the same factors for
  # both members, so that the apparent metabolite scale stays consistent.
  a <- amiodarone(120, 170, 50, "male")
  d <- desethylamiodarone(120, 170, 50, "male")
  expect_equal_rounded(a$PK$default, list(
    v1 = 1150.927662, v2 = 16572.31441, v3 = 1,
    cl1 = 0.1941590206, cl2 = 0.4985393192, cl3 = 0
  ))
  expect_equal_rounded(d$PK$default, list(
    v1 = 3640.689543, v2 = 10217.41904, v3 = 1,
    cl1 = 0.2153554202, cl2 = 0.1280262537, cl3 = 0
  ))
  # Formation is the scaled parent's own k10: (FFM ratio)^-0.25 x 1.803e-4
  expect_equal_rounded(a$metabolite$kFormation, 1.686978487e-4)
})

test_that("both drugs give the same, single citation", {
  expect_identical(amiodarone(70, 170, 50, "male")$reference, POLLAK_REFERENCE)
  expect_identical(desethylamiodarone(70, 170, 50, "male")$reference, POLLAK_REFERENCE)
  expect_match(POLLAK_REFERENCE, "https://pubmed.ncbi.nlm.nih.gov/10872646/$")
})


# --- Half-lives, formation and steady state ---------------------------------

test_that("the half-lives are those of Table II's parameters", {
  # From the quadratic for a two-compartment model on Table II, by hand.  The
  # paper reports 17 h and 55 days for the parent, and 62 days for the
  # metabolite, which Table II's own values do not reproduce (60.39 days).
  PK <- getDrugPK("amiodarone", 70, 170, 50, "male", getDrugDefaults("amiodarone"),
                  adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / PK$lambda_1 / 60, 17.32719464, tolerance = 1e-6)
  expect_equal(log(2) / PK$lambda_2 / MINS_PER_DAY, 55.35965915, tolerance = 1e-6)
  expect_equal(PK$lambda_3, 0)
  expect_equal(PK$ke0, 0)

  M <- getDrugPK("desethylamiodarone", 70, 170, 50, "male",
                 getDrugDefaults("desethylamiodarone"), adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / M$lambda_1 / 60, 108.7511948, tolerance = 1e-6)
  expect_equal(log(2) / M$lambda_2 / MINS_PER_DAY, 60.39255841, tolerance = 1e-6)
  expect_equal(M$ke0, 0)

  # The independent system has the same four eigenvalues as the package's
  # convolution: the parent's two and the metabolite's two.
  S <- pollakSystem()
  expect_equal(sort(-S$lambda), sort(PK$metabolite$coefs$lambda), tolerance = 1e-10)
})

test_that("formation is the parent's own elimination, mass for mass", {
  PK <- getDrugPK("amiodarone", 70, 170, 50, "male", getDrugDefaults("amiodarone"),
                  adjustToFFM = FALSE)$PK$default
  expect_equal(PK$metabolite$name, "desethylamiodarone")
  # kFormation is k10, so the formation flux kFormation x V1 x Cp is CL1 x Cp
  expect_equal(PK$k10, 1.803036029e-4, tolerance = 1e-9)
  expect_equal(PK$metabolite$coefs$K, 229 / 1440, tolerance = 1e-12)
  # Neither drug carries an absorption model: oral input is a constant rate
  expect_equal(PK$ka_PO, 0)
  expect_equal(PK$metabolite$coefs$PO, rep(0, 4))
})

test_that("steady state is the dosing rate over clearance for both drugs", {
  # At steady state every milligram given is eliminated, so Css = rate / CL1;
  # and since all of it becomes metabolite, the metabolite's Css is the rate
  # over ITS clearance.  343 mg/day is Pollak's maintenance dose, 1.5 mg/L x
  # 229 L/day.
  PK <- getDrugPK("amiodarone", 70, 170, 50, "male", getDrugDefaults("amiodarone"),
                  adjustToFFM = FALSE)$PK$default
  parentInfusion <- PK$p_coef_infusion_l1 + PK$p_coef_infusion_l2 + PK$p_coef_infusion_l3
  expect_equal(343 / 1440 * parentInfusion, 1.497816594, tolerance = 1e-9)
  expect_equal(343 / 1440 * sum(PK$metabolite$coefs$infusion), 1.350393701, tolerance = 1e-9)

  # And through the engine: 400 mg/day for ten years, by when the slowest
  # exponential has decayed by e^-42.
  tenYears <- 3650 * MINS_PER_DAY
  at <- engineAt(data.frame(Drug = "amiodarone", Time = 0, Dose = 400, Units = "mg/day PO"),
                 tenYears)
  expect_equal(unname(at["parent"]), 1.746724891, tolerance = 1e-8)
  expect_equal(unname(at["metabolite"]), 1.57480315, tolerance = 1e-8)
  # The ratio the paper's clearances imply, 229 / 254
  expect_equal(unname(at["metabolite"] / at["parent"]), 229 / 254, tolerance = 1e-8)
})


# --- The constant-rate oral input --------------------------------------------

test_that("mg/day PO is a constant rate: 1440 mg/day equals 1 mg/min", {
  PK <- getDrugPK("amiodarone", 70, 170, 50, "male", getDrugDefaults("amiodarone"))
  PK$endCe <- 1
  oral <- data.frame(Drug = "amiodarone", Time = c(0, 4320), Dose = c(1440, 0),
                     Units = "mg/day PO")
  perMin <- transform(oral, Dose = c(1, 0), Units = "mg/min")
  perHr  <- transform(oral, Dose = c(60, 0), Units = "mg/hr")
  a <- simCpCe(oral, noEvents, PK, 7200, TRUE)
  b <- simCpCe(perMin, noEvents, PK, 7200, TRUE)
  h <- simCpCe(perHr, noEvents, PK, 7200, TRUE)
  expect_equal(a$wide, b$wide)
  expect_equal(a$wide, h$wide)
  expect_equal(a$metaboliteSeries, b$metaboliteSeries)
  # A rate, not an amount: the concentration rises from zero rather than
  # jumping, and nothing is lost to a bioavailability of zero.
  expect_equal(a$wide$Plasma[1], 0)
  expect_gt(max(a$wide$Plasma), 0.5)
  # A row of 0 mg/day PO stops it: from then on the curve only falls
  w <- a$wide[a$wide$Time >= 4320, ]
  expect_true(all(diff(w$Plasma) < 0))
})

test_that("a dosing regimen follows the rate equations, row by row", {
  # Pollak's proposed regimen, read at the start of each step and at a few
  # later days, against the independent solution.  Each later row replaces
  # the running rate.
  for (day in c(1, 2, 7, 14, 21, 28, 60, 90, 180, 365)) {
    at <- day * MINS_PER_DAY
    expected <- pollakConcentrations(POLLAK_DAYS * MINS_PER_DAY, POLLAK_MG_PER_DAY / MINS_PER_DAY, at)
    expect_equal(engineAt(pollakRegimen, at), expected, tolerance = 1e-8,
                 label = paste("day", day))
  }
})

test_that("Pollak's regimen holds serum amiodarone in the therapeutic window", {
  # The model's prediction, on a quarter-day grid of the independent
  # solution: within 1.0 to 2.5 mg/L from the end of the first day.
  days <- seq(1, 365, by = 0.25)
  cp <- vapply(days * MINS_PER_DAY, function(t)
    pollakConcentrations(POLLAK_DAYS * MINS_PER_DAY, POLLAK_MG_PER_DAY / MINS_PER_DAY, t)[["parent"]],
    numeric(1))
  expect_gte(min(cp), 1.0)
  expect_lte(max(cp), 2.5)

  # The engine at every point it computed after the first day, which are
  # exact values of the same function
  o <- simulateDrugsWithCovariates(pollakRegimen, noEvents, 70, 170, 50, "male",
                                   365 * MINS_PER_DAY, FALSE, adjustToFFM = FALSE)
  a <- o$amiodarone$wide
  late <- a$Plasma[a$Time >= MINS_PER_DAY]
  expect_true(all(late >= 1.0 & late <= 2.5))
  # Settling on the 1.5 mg/L target by a year
  expect_equal(a$Plasma[nrow(a)], 1.5, tolerance = 0.01)

  # The values the literature check recorded (to two decimals), as a
  # readable anchor: parent / metabolite at days 7, 28 and 365
  expect_equal(round(engineAt(pollakRegimen, 7 * MINS_PER_DAY), 2),
               c(parent = 1.72, metabolite = 0.56))
  expect_equal(round(engineAt(pollakRegimen, 28 * MINS_PER_DAY), 2),
               c(parent = 1.54, metabolite = 0.95))
  expect_equal(round(engineAt(pollakRegimen, 365 * MINS_PER_DAY), 2),
               c(parent = 1.50, metabolite = 1.34))
})

test_that("the context-sensitive decrement at steady state is the model's", {
  # Pollak's text and Fig 6 give 25% in 3 days, 50% in 36 days ("nearly 5
  # weeks") and 75% in 98 days at steady state.  Table II's parameters give
  # 2.2, 31.2 and 86.6 days, which is what is pinned.  The 12 to 15% gap is
  # not explained by the abstract's CL2/F of 599 L/day (2.3, 31.5 and 86.6
  # days), nor by the length of therapy before stopping: for a constant
  # concentration, which is what Pollak's definition assumes, the decrement
  # time grows with duration towards its steady-state value, so no shorter
  # course gives a longer one.  Solved here from the parent's own
  # two-compartment rate equations, independently of the package.
  k10 <- 229 / 1440 / 882; k12 <- 588 / 1440 / 882; k21 <- 588 / 1440 / 12700
  K <- matrix(c(-(k10 + k12), k12, k21, -k21), 2, 2)
  e <- eigen(K)
  steady <- c(1 / k10, k12 / k21 / k10)    # amounts per unit rate
  remaining <- function(t)
    (Re(e$vectors %*% diag(exp(e$values * t)) %*% solve(e$vectors)) %*% steady)[1] / steady[1]
  decrement <- vapply(c(0.75, 0.5, 0.25), function(f)
    stats::uniroot(function(t) remaining(t) - f, c(1, 1e7), tol = 1e-8)$root, numeric(1))
  expect_equal(decrement / MINS_PER_DAY, c(2.217235, 31.22575, 86.58541), tolerance = 1e-5)

  # The engine agrees: 400 mg/day for ten years (steady state to e^-42), then
  # stopped with a row of 0 mg/day PO, read at each decrement time.
  stop <- 3650 * MINS_PER_DAY
  DT <- data.frame(Drug = "amiodarone", Time = c(0, stop), Dose = c(400, 0),
                   Units = "mg/day PO")
  atStop <- engineAt(DT, stop)[["parent"]]
  for (i in 1:3) {
    expect_equal(engineAt(DT, stop + decrement[i])[["parent"]] / atStop,
                 c(0.75, 0.5, 0.25)[i], tolerance = 1e-6)
  }
})

test_that("the time until threshold is the time for serum amiodarone to leave the window", {
  # endCe 1.0 mg/L, timed on the plasma because there is no effect site.  One
  # day of 1600 mg/day leaves 1.19 mg/L; stopping there, the reported time is
  # when the serum concentration reaches 1.0, well inside the one-week
  # plasma horizon.  Nothing antibiotic-specific applies: there is no MIC.
  expect_null(antibioticMic("amiodarone"))
  dd <- getDrugDefaultsGlobal()
  expect_equal(dd$endCe[dd$Drug == "amiodarone"], 1)
  load <- data.frame(Drug = "amiodarone", Time = 0, Dose = 1600, Units = "mg/day PO")
  o <- simulateDrugsWithCovariates(load, noEvents, 70, 170, 50, "male",
                                   MINS_PER_DAY, TRUE, adjustToFFM = FALSE)
  w <- o$amiodarone$wide
  rec <- w$Recovery[nrow(w)]
  expect_gt(rec, 0)
  expect_lt(rec, MINS_PER_WEEK)
  stopped <- rbind(load, data.frame(Drug = "amiodarone", Time = MINS_PER_DAY, Dose = 0,
                                    Units = "mg/day PO"))
  expect_equal(engineAt(stopped, MINS_PER_DAY + rec)[["parent"]], 1, tolerance = 1e-6)
  # The metabolite has no threshold
  expect_true(all(o$desethylamiodarone$wide$Recovery == 0))
})

test_that("neither drug has an effect site, and the metabolite row is formed", {
  o <- simulateDrugsWithCovariates(pollakRegimen, noEvents, 70, 170, 50, "male",
                                   28 * MINS_PER_DAY, FALSE)
  expect_setequal(names(o), c("amiodarone", "desethylamiodarone"))
  expect_equal(o$desethylamiodarone$formedFrom, "amiodarone")
  expect_true(all(is.na(o$amiodarone$wide$"Effect Site")))
  expect_true(all(is.na(o$desethylamiodarone$wide$"Effect Site")))
  # Not opioids: nothing on the MEAC panel
  expect_equal(max(o$amiodarone$equiSpace$MEAC), 0)
  expect_equal(max(o$desethylamiodarone$equiSpace$MEAC), 0)
})


# --- The library rows ---------------------------------------------------------

test_that("the library offers amiodarone as a constant daily oral rate only", {
  dd <- getDrugDefaultsGlobal()
  a <- dd[dd$Drug == "amiodarone", ]
  units <- a$Units[[1]]
  # Oral by route, a rate by kind; no intravenous unit (the parameters are
  # apparent, divided by F) and no first-order oral unit (no ka was
  # identified)
  expect_equal(units, poRateUnits)
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_true(all(isRateUnit(units)))
  expect_false(any(units %in% c(bolusUnits, infusionUnits, poUnits)))
  expect_equal(a$Default.Units, "mg/day PO")
  expect_true(is.na(a$Bolus.Units) || !nzchar(a$Bolus.Units))
  expect_true(is.na(a$Infusion.Units) || !nzchar(a$Infusion.Units))
  # The therapeutic window and target, mirrored by the model function
  m <- amiodarone(70, 170, 50, "male")
  expect_equal(c(a$Lower, a$Upper, a$Typical), c(m$lowerTypical, m$upperTypical, m$typical))
  expect_equal(c(a$Lower, a$Upper, a$Typical), c(1, 2.5, 1.5))
  expect_equal(a$MEAC, 0)
  expect_equal(a$Concentration.Units, "mcg")
  expect_equal(a$Class, "IV")

  # Desethylamiodarone: no unit (formed only), no band, no threshold
  d <- dd[dd$Drug == "desethylamiodarone", ]
  expect_length(helpDrugUnits(d), 0)
  expect_equal(c(d$Lower, d$Upper, d$Typical, d$MEAC, d$endCe), rep(0, 5))
  expect_equal(d$Concentration.Units, "mcg")

  # Colours of their own
  others <- dd$Color[!dd$Drug %in% c("amiodarone", "desethylamiodarone")]
  expect_false(any(toupper(c(a$Color, d$Color)) %in% toupper(others)))
  expect_false(identical(a$Color, d$Color))
})

test_that("a dose table of mg/day PO rows validates", {
  rows <- data.frame(Drug = "amiodarone", Time = c("0", "2880", "129600"),
                     Dose = c("1600", "1200", "343"), Units = "mg/day PO")
  expect_true(validateDoseTableInput(rows))
})
