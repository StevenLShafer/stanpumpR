# Vincristine in children (R/drugs_vincristinePediatric.R): Nijstad 2022, two
# compartments with saturable binding to beta-tubulin, carried at
# quasi-equilibrium by the Michaelis-Menten engine's rapidBinding with
# plotted = "unbound".  Drafted by Claude Code, 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())

# The full three-state system of Nijstad's Table S1 (binding as a process,
# kon 1300 /h, koff 11.5 /h), in mg and hours, with Table II's values typed
# here: classical Runge-Kutta at 0.0002 h (0.012 min), where the fastest rate,
# about 1330 /h, gives a step product of 0.27.  An intravenous bolus at 0.
nijstadFull <- function(dose, weight, age, times, h = 0.0002) {
  w <- weight / 70
  CL <- 30.6 * w^0.75; Q <- 63.2 * w^0.75; Vc <- 5.39 * w; Vp <- 400 * w
  B <- 0.525 * w * (max(age, 0.04) / 18)^-0.199
  kon <- 1300; koff <- 11.5
  f <- function(x) {
    bind <- kon * x[1] * (1 - x[3] / B) - koff * x[3]
    c(-(CL + Q) / Vc * x[1] + Q / Vp * x[2] - bind,
      Q / Vc * x[1] - Q / Vp * x[2],
      bind)
  }
  x <- c(dose, 0, 0); t <- 0; out <- numeric(length(times))
  for (i in seq_along(times)) {
    n <- round((times[i] - t) / h)
    hh <- (times[i] - t) / n
    for (k in seq_len(n)) {
      k1 <- f(x); k2 <- f(x + hh / 2 * k1); k3 <- f(x + hh / 2 * k2); k4 <- f(x + hh * k3)
      x <- x + hh / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    }
    t <- times[i]
    out[i] <- x[1] / Vc * 1000                 # ng/mL
  }
  out
}

bolus <- function(dose, weight, age, hours = 80) {
  d <- data.frame(Drug = "vincristinePediatric", Time = 0, Dose = dose, Units = "mg")
  simulateDrugsWithCovariates(d, noEvents, weight, 130, age, "male", hours * 60, FALSE,
                              adjustToFFM = FALSE)$vincristinePediatric$wide
}

test_that("the model is Table II, allometric in weight, with the binding at equilibrium", {
  X <- vincristinePediatric(70, 175, 18, "male", adjustToFFM = FALSE)
  set <- X$PK$default
  mm <- X$michaelisMenten
  expect_equal_rounded(set$cl1 * 60, 30.6)
  expect_equal_rounded(mm$clLinear * 60, 30.6)
  expect_equal_rounded(set$cl2 * 60, 63.2)
  expect_equal_rounded(set$v1, 5.39)
  expect_equal_rounded(set$v2, 400)
  expect_equal(mm$vmax, 0)
  rb <- mm$rapidBinding
  expect_identical(rb$plotted, "unbound")
  expect_equal(rb$amaxPeripheral, 0)
  # Bmax 0.525 mg = 525 mcg in 5.39 L; KD = Bmax koff / kon
  expect_equal_rounded(rb$bmaxCentral, 525 / 5.39)
  expect_equal_rounded(rb$kd, 525 * 11.5 / 1300 / 5.39)
  expect_equal(round(rb$kd, 3), 0.862)                # ng/mL, the spec's 0.86

  # A 20-kg child: weight on everything but kon and koff
  C <- vincristinePediatric(20, 110, 18, "male", adjustToFFM = FALSE)
  expect_equal_rounded(C$PK$default$cl1 / set$cl1, (20 / 70)^0.75)
  expect_equal_rounded(C$PK$default$v1 / set$v1, 20 / 70)
  # KD/Vc and Bmax/Vc do not depend on weight
  expect_equal_rounded(C$michaelisMenten$rapidBinding$kd, rb$kd)
  expect_equal_rounded(C$michaelisMenten$rapidBinding$bmaxCentral, rb$bmaxCentral)
})

test_that("the age term on Bmax is floored at the youngest age studied, 0.04 years", {
  b <- function(age) vincristinePediatric(4, 55, age, "male",
                                          adjustToFFM = FALSE)$michaelisMenten$rapidBinding$bmaxCentral
  expect_equal(b(0), b(0.04))
  expect_gt(b(0.04), b(0.05))
  expect_equal_rounded(b(0.04) / b(18), (0.04 / 18)^-0.199)
  expect_equal(round((0.04 / 18)^-0.199, 2), 3.37)
  expect_true(is.finite(b(0)))
})

test_that("the quasi-equilibrium is within about 1% of the full binding ODE (bolus)", {
  # Typical child (8.3 y, 27.1 kg, 1.6 mg), adult (18 y, 70 kg, 2 mg) and
  # infant (0.5 y, 7.5 kg, 0.375 mg), compared at the engine's own time
  # points.  Measured 2026-11-06 (per cent, at 5 min, 30 min, 1 h, 6 h, 24 h
  # and 72 h): child 0.06, -0.48, -0.30, 0.01, 0.02, -0.08; adult 0.16, 0.18,
  # -0.51, 0.00, 0.01, -0.05; infant 1.07, -0.71, -0.80, 0.01, -0.01, -0.36.
  # The error is the reduction's, not the step's: a 0.1-min engine step
  # changes it by under 0.05%.
  for (case in list(c(1.6, 27.1, 8.3), c(2, 70, 18), c(0.375, 7.5, 0.5))) {
    wide <- bolus(case[1], case[2], case[3])
    pick <- vapply(c(5, 30, 60, 360, 1440, 4320),
                   function(t) which.min(abs(wide$Time - t)), integer(1))
    ref <- nijstadFull(case[1], case[2], case[3], wide$Time[pick] / 60)
    rel <- wide$Plasma[pick] / ref - 1
    expect_lt(abs(rel[1]), 0.012, label = paste(case, collapse = "/"))
    expect_lt(max(abs(rel[-1])), 0.009, label = paste(case, collapse = "/"))
    expect_lt(max(abs(rel[4:5])), 0.001, label = paste(case, collapse = "/"))
  }
})

test_that("the spec's typical-patient values", {
  child <- bolus(1.6, 27.1, 8.3)
  at <- function(w, t) approx(w$Time, w$Plasma, t)$y
  expect_equal(at(child, 60), 4.03, tolerance = 0.01)
  expect_equal(at(child, 1440), 1.04, tolerance = 0.01)
  expect_equal(at(child, 4320), 0.125, tolerance = 0.01)
  adult <- bolus(2, 70, 18)
  expect_equal(at(adult, 60), 2.31, tolerance = 0.01)
  expect_equal(at(adult, 1440), 0.684, tolerance = 0.01)
})

test_that("mass balance: the AUC of the unbound concentration is dose / CL", {
  # The binding is reversible and elimination is from the central
  # compartment only, so every microgram given is cleared at CL.
  wide <- bolus(2, 70, 18, hours = 24 * 21)
  auc <- sum(diff(wide$Time) * (head(wide$Plasma, -1) + tail(wide$Plasma, -1)) / 2) / 60
  expect_equal(auc, 2000 / 30.6, tolerance = 0.005)
})

test_that("the binding saturates: a larger dose is not a scaled curve", {
  small <- bolus(0.5, 70, 18, hours = 48)
  large <- bolus(2, 70, 18, hours = 48)
  ratio <- function(t) {
    i <- which.min(abs(small$Time - t))
    large$Plasma[i] / small$Plasma[i]
  }
  # Four times the dose: at 5 min the binding is saturated by the large dose
  # (about 16 times the unbound concentration), and by 1 h the extra free
  # drug has been cleared (about 2 times)
  expect_gt(ratio(5), 12)
  expect_lt(ratio(60), 2.5)
})

test_that("both switch positions are pinned", {
  on <- vincristinePediatric(20, 110, 5, "female")
  off <- vincristinePediatric(20, 110, 5, "female", adjustToFFM = FALSE)
  expect_equal_rounded(off$PK$default$v1, 1.54)
  expect_equal_rounded(on$PK$default$v1, 1.5254263)
  expect_equal_rounded(on$PK$default$cl1, 0.19788917)
})

test_that("vincristinePediatric is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "vincristinePediatric", ]
  expect_true(is.na(row$Category) || row$Category == "")
  expect_true(row$Drug %in% unlist(lapply(MODEL_GROUPS, function(g) g$model)))  # a candidate under its drug's one name
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("vincristinePediatric", 20, 110, 5, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Nijstad")
  expect_identical(therapeuticRange("vincristinePediatric")$band_status, "not_applicable")
})
