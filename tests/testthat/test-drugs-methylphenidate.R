# Methylphenidate: d-methylphenidate after racemic Ritalin IR (Lyauk 2016).
#
# What is guarded: the published parameters, the dose basis (half the tablet
# is d-MPH, checked against the AUC Stage 2017 observed in the same subjects),
# the lag-time reduction of the transit absorption against the published
# transit model itself, and that the apparent parameters stay oral only.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

# Lyauk's published model: dose into T1, three transit compartments, depot,
# two-compartment disposition.  Integrated by fourth-order Runge-Kutta, hours,
# so the reduction is checked against the source structure rather than
# against itself.  Returns d-MPH in ng/mL at times `tt` (h).
lyaukTransit <- function(dose_dMPH, tt, MTT = 0.505, ka = 0.418, CL = 233,
                         V1 = 97.6, Q = 70.1, V2 = 252, dt = 0.001) {
  ktr <- 4 / MTT
  deriv <- function(x) c(
    -ktr * x[1],
    ktr * (x[1] - x[2]),
    ktr * (x[2] - x[3]),
    ktr * x[3] - ka * x[4],
    ka * x[4] - (CL + Q) * x[5] / V1 + Q * x[6] / V2,
    Q * x[5] / V1 - Q * x[6] / V2
  )
  x <- c(dose_dMPH, 0, 0, 0, 0, 0)
  steps <- round(max(tt) / dt)
  out <- numeric(steps + 1)
  out[1] <- 0
  for (i in seq_len(steps)) {
    k1 <- deriv(x)
    k2 <- deriv(x + dt / 2 * k1)
    k3 <- deriv(x + dt / 2 * k2)
    k4 <- deriv(x + dt * k3)
    x <- x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    out[i + 1] <- x[5]
  }
  out[round(tt / dt) + 1] * 1000 / V1
}

# The same disposition with the engine's lag-time input, closed form via the
# package's own PK resolution.
reducedCurve <- function(sex, tt_h, mg = 10) {
  PK <- getDrugPK("methylphenidate", 70, 170, 35, sex,
                  getDrugDefaults("methylphenidate"), adjustToFFM = FALSE)
  pk <- PK$PK$default
  lam <- c(pk$lambda_1, pk$lambda_2)
  a   <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2) * pk$ka_PO / (pk$ka_PO - lam)
  coef   <- c(a, -sum(a))
  lambda <- c(lam, pk$ka_PO)
  s <- pmax(tt_h * 60 - pk$tlag_PO, 0)
  vapply(s, function(t) sum(coef * exp(-lambda * t)), numeric(1)) *
    mg * pk$bioavailability_PO * 1000      # mg/L to ng/mL
}


test_that("returns the correct calculations", {
  actual <- methylphenidate(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = 97.6, v2 = 252, v3 = 1,
      cl1 = 233 / 60, cl2 = 70.1 / 60, cl3 = 0,
      ka_PO = 0.39782 / 60,
      bioavailability_PO = 0.5,
      tlag_PO = 0.34871 * 60
    )),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = actual$reference,
    oralPulses = actual$oralPulses    # Concerta; tested on its own below
  )

  expect_equal_rounded(actual, expected)
})


test_that("female sex lengthens absorption only", {
  m <- methylphenidate(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  f <- methylphenidate(70, 170, 35, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(f$tlag_PO, 0.62925 * 60)
  expect_equal_rounded(f$ka_PO, 0.36607 / 60)
  expect_gt(f$tlag_PO, m$tlag_PO)
  # Lyauk put sex on MTT alone: disposition is unchanged
  expect_equal(f[c("v1", "v2", "cl1", "cl2")], m[c("v1", "v2", "cl1", "cl2")])
})


test_that("an unrecognised sex is refused rather than read as male", {
  expect_error(methylphenidate(70, 170, 35, "Female"), "Invalid sex")
  expect_error(methylphenidate(70, 170, 35, NA), "Invalid sex")
  expect_error(methylphenidate(70, 170, 35, c("male", "female")), "Invalid sex")
})


test_that("the switch off reproduces Lyauk's published allometry", {
  # 50 kg: CL/F and Q/F x (50/70)^0.75, volumes x 50/70
  x <- methylphenidate(50, 160, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(x$cl1 * 60, 233 * (50 / 70)^0.75)
  expect_equal_rounded(x$cl2 * 60, 70.1 * (50 / 70)^0.75)
  expect_equal_rounded(x$v1, 97.6 * 50 / 70)
  expect_equal_rounded(x$v2, 252 * 50 / 70)
})


test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  actual <- methylphenidate(120, 170, 50, "male")
  expected <- list(
    v1 = 97.6 * 1.3049067, v2 = 252 * 1.3049067, v3 = 1,
    cl1 = 233 / 60 * 1.2209126, cl2 = 70.1 / 60 * 1.2209126, cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})


test_that("the dose basis reproduces the d-MPH AUC observed after 10 mg racemic", {
  # Stage 2017 (Lyauk's Study I), CES1 control group: median d-MPH AUC
  # 21.4 ng.h/mL after a 10 mg Ritalin tablet.  On the racemic basis the
  # model would give twice that.
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "methylphenidate", Time = 0, Dose = 10, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 2880, FALSE, adjustToFFM = FALSE)
  w <- o$methylphenidate$wide
  auc_h <- trapz(w$Time, w$Plasma) / 60
  expect_equal(auc_h / 21.4, 1, tolerance = 0.03)
  # Analytically: 5 mg d-MPH / 233 L/h
  expect_equal(auc_h / (5000 / 233), 1, tolerance = 0.01)
})


test_that("the lag-time reduction tracks the published transit model", {
  tt <- seq(0, 24, by = 0.05)
  for (sex in c("male", "female")) {
    MTT <- if (sex == "female") 0.505 * 1.925 else 0.505
    ode <- lyaukTransit(5, tt, MTT = MTT)
    red <- reducedCurve(sex, tt)

    # Peak height within 2%, and the area exact (same disposition and dose)
    expect_equal(max(red) / max(ode), 1, tolerance = 0.02)
    expect_equal(trapz(tt, red) / trapz(tt, ode), 1, tolerance = 0.01)
    # The peak comes early, but by less than 20 minutes
    expect_lt(abs(tt[which.max(red)] - tt[which.max(ode)]), 1 / 3)
    # The documented worst point is on the rising limb, within a third of
    # the peak; from 4 h on the two curves agree within 3% of the peak
    expect_lt(max(abs(red - ode)) / max(ode), 0.32)
    late <- tt >= 4
    expect_lt(max(abs(red[late] - ode[late])) / max(ode), 0.03)
  }
})


test_that("methylphenidate is offered orally only, because the parameters are apparent", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "methylphenidate"], ",")[[1]]
  expect_equal(units, c("mg PO", paste("mg PO", names(SCHEDULE_INTERVALS)),
                        "mg PO XR", "mg PO XR qd"))
  expect_true(all(doseRoute(units) == "PO"))
  expect_equal(dd$Category[dd$Drug == "methylphenidate"], "Stimulants")
})


test_that("no effect site and no therapeutic band are claimed", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "methylphenidate", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC, row$endCe), rep(0, 5))
  expect_equal(methylphenidate(70, 170, 35, "male")$tPeak, 0)
})


# Concerta ("mg PO XR"): input fitted to the shape of Childress 2025's mean
# curves (OROS reference arms, fasted healthy adults).

concertaCurve <- function(mg, maximum = 2880) {
  simulateDrugsWithCovariates(
    data.frame(Drug = "methylphenidate", Time = 0, Dose = mg, Units = "mg PO XR"),
    noEvents, 70, 170, 35, "male", maximum, FALSE, adjustToFFM = FALSE
  )$methylphenidate$wide
}
aucBetween <- function(w, t1, t2) {
  i <- w$Time >= t1 * 60 & w$Time <= t2 * 60
  trapz(w$Time[i], w$Plasma[i]) / 60
}


test_that("Concerta's input is 22% at once and 78% over 2 to 15 h, falling", {
  p <- methylphenidate(70, 170, 35, "male")$oralPulses$XR
  expect_equal(sum(p$fraction), 1)
  expect_equal(p$fraction[1], 0.22)
  expect_equal(p$delay[1], 0)
  core <- p$delay[-1]
  expect_true(all(core > 2 * 60 & core < 15 * 60))
  expect_equal(length(core), 52)                  # every 15 min
  expect_true(all(diff(p$fraction[-1]) < 0))      # the rate falls
  expect_equal(sum(p$fraction[-1]), 0.78)
})


test_that("Concerta reproduces the shape of Childress's mean curves", {
  # Fractions of AUC0-inf in 0-3, 3-7, 7-12 h and after 12 h; observed after
  # 54 mg 0.103 / 0.257 / 0.329 / 0.311 and after 2 x 36 mg 0.113 / 0.298 /
  # 0.340 / 0.249.  The model is held between the two, within 15% of either.
  w <- concertaCurve(54, maximum = 4320)
  total <- trapz(w$Time, w$Plasma) / 60
  f <- c(aucBetween(w, 0, 3), aucBetween(w, 3, 7), aucBetween(w, 7, 12)) / total
  f <- c(f, 1 - sum(f))
  expect_equal(f, c(0.108, 0.278, 0.335, 0.280), tolerance = 0.15)
  # Tmax: median 7.0 h (54 mg) and 6.5 h (2 x 36 mg)
  tmax <- w$Time[which.max(w$Plasma)] / 60
  expect_gt(tmax, 6)
  expect_lt(tmax, 7.5)
  # The overcoat gives an early shoulder: well above zero by 1 h
  expect_gt(w$Plasma[which.min(abs(w$Time - 60))], 0.25 * max(w$Plasma))
})


test_that("Concerta delivers the whole dose, and its level is Lyauk's", {
  # AUC = d-MPH dose / CL/F, whatever the input: 27 mg / 233 L/h after 54 mg
  w <- concertaCurve(54, maximum = 4320)
  expect_equal(trapz(w$Time, w$Plasma) / 60 / (27000 / 233), 1, tolerance = 0.01)
  # The recorded shortfall against Childress (total MPH, AUC 173.8 and Cmax
  # 14.6 after 54 mg): about a third low.  If this changes, the help page and
  # the header must change with it.
  expect_equal(trapz(w$Time, w$Plasma) / 60 / 173.8, 0.67, tolerance = 0.05)
  expect_equal(max(w$Plasma) / 14.6, 0.61, tolerance = 0.08)
})


test_that("Concerta once daily equals its scheduled repeats", {
  one <- simulateDrugsWithCovariates(
    data.frame(Drug = "methylphenidate", Time = c(0, 1440), Dose = 36, Units = "mg PO XR"),
    noEvents, 70, 170, 35, "male", 2880, FALSE)$methylphenidate$wide
  qd <- simulateDrugsWithCovariates(
    data.frame(Drug = "methylphenidate", Time = 0, Dose = 36, Units = "mg PO XR qd"),
    noEvents, 70, 170, 35, "male", 2880, FALSE)$methylphenidate$wide
  expect_equal(qd$Plasma, approx(one$Time, one$Plasma, qd$Time)$y, tolerance = 1e-8)
})
