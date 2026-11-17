# The autoinduction block of the Michaelis-Menten engine
# (R/advanceMichaelisMenten.R, AUTOINDUCTION): ifosfamide (Kerbusch 2000) and
# cyclophosphamide in children (McCune 2009).  Drafted by Claude Code,
# 2026-11-10.

noEvents <- data.frame(Time = double(), Event = character())

# An independent integration of the published equations, written from the
# papers rather than from the engine: classical RK4 at a 0.05-min step with
# the infusion switched at exact step boundaries.
referenceAutoinduction <- function(V, clNon, clInd, kEnz, emax, ec50, imax, ic50,
                                   rateTable, times, h = 0.05)
{
  rateAt <- function(t) {
    r <- rateTable$rate[findInterval(t, rateTable$time)]
    if (length(r) == 0 || is.na(r)) 0 else r
  }
  f <- function(x, R) {
    C <- x[1] / V
    c(R - clNon * C - clInd * x[2] * C,
      kEnz * ((1 + emax * C / (ec50 + C)) - x[2] * (1 - imax * C / (ic50 + C))))
  }
  x <- c(0, 1)
  t <- 0
  out <- numeric(length(times))
  for (i in seq_along(times)) {
    while (t < times[i] - 1e-9) {
      R <- rateAt(t + 1e-9)
      k1 <- f(x, R); k2 <- f(x + h / 2 * k1, R)
      k3 <- f(x + h / 2 * k2, R); k4 <- f(x + h * k3, R)
      x <- x + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
      t <- t + h
    }
    out[i] <- x[1] / V
  }
  out
}

test_that("ifosfamide: the engine agrees with an independent integration of equations 1-4", {
  X <- ifosfamide(59, 170, 49, "male", adjustToFFM = FALSE)
  ai <- X$michaelisMenten$autoinduction
  rate <- 10000 / (72 * 60)                       # 10 g over 72 h, mg/min
  dose <- data.frame(Drug = "ifosfamide", Time = c(0, 72 * 60),
                     Dose = c(rate * 60, 0), Units = "mg/hr")
  w <- simulateDrugsWithCovariates(dose, noEvents, 59, 170, 49, "male", 96 * 60,
                                   FALSE, adjustToFFM = FALSE)$ifosfamide$wide
  at <- c(180, 600, 1200, 1440, 2880, 4320, 4380, 4800, 5520)
  engine <- approx(w$Time, w$Plasma, at)$y
  ref <- referenceAutoinduction(X$PK$default$v1, 0, ai$cl, ai$kEnz, 0, 1, 1, ai$ic50,
                                data.frame(time = c(0, 72 * 60), rate = c(rate, 0)), at)
  expect_equal(engine, ref, tolerance = 1e-4)
})

test_that("cyclophosphamide: the engine agrees with an independent integration of McCune's equations", {
  X <- cyclophosphamideMcCune(14.2, 95, 3.16, "male", adjustToFFM = FALSE)
  mm <- X$michaelisMenten
  bsa <- bsaMosteller(14.2, 95)
  starts <- (0:2) * 1440
  dose <- data.frame(Drug = "cyclophosphamideMcCune", Time = rep(starts, each = 2) + c(0, 30),
                     Dose = rep(c(400 * bsa * 2, 0), 3), Units = "mg/hr")
  w <- simulateDrugsWithCovariates(dose, noEvents, 14.2, 95, 3.16, "male", 3 * 1440,
                                   FALSE, adjustToFFM = FALSE)$cyclophosphamideMcCune$wide
  at <- c(30, 120, 600, 1470, 1800, 2910, 3600, 4000)
  engine <- approx(w$Time, w$Plasma, at)$y
  rt <- data.frame(time = sort(c(starts, starts + 30)),
                   rate = rep(c(400 * bsa / 30, 0), 3))
  ref <- referenceAutoinduction(X$PK$default$v1, mm$clLinear, mm$autoinduction$cl,
                                mm$autoinduction$kEnz, 5, mm$autoinduction$ec50, 0, 1,
                                rt, at)
  expect_equal(engine, ref, tolerance = 1e-4)
})

test_that("a long constant infusion reaches the steady state the equations imply", {
  # Kerbusch: at steady state E = 1 + C / IC50 and R = CL E C, so
  # R = CL C (1 + C / IC50), a quadratic in C.
  X <- ifosfamide(70, 170, 35, "male", adjustToFFM = FALSE)
  ai <- X$michaelisMenten$autoinduction
  R <- 2                                           # mg/min
  dose <- data.frame(Drug = "ifosfamide", Time = 0, Dose = R * 60, Units = "mg/hr")
  w <- simulateDrugsWithCovariates(dose, noEvents, 70, 170, 35, "male", 30 * 1440,
                                   FALSE, adjustToFFM = FALSE)$ifosfamide$wide
  css <- (-1 + sqrt(1 + 4 * R / (ai$cl * ai$ic50) )) / (2 / ai$ic50)
  expect_equal(tail(w$Plasma, 1), css, tolerance = 1e-4)
})

test_that("the reference integration itself: with the enzyme frozen it is the exact linear curve", {
  # A negligible kEnz leaves E at 1: clearance clLinear + cl, linear, whose
  # constant-rate solution is exact.  This checks the referee above.
  X <- cyclophosphamideMcCune(14.2, 95, 3.16, "male", adjustToFFM = FALSE)
  V <- X$PK$default$v1
  k <- (X$michaelisMenten$clLinear + X$michaelisMenten$autoinduction$cl) / V
  t <- c(15, 30, 120, 600)
  exact <- ifelse(t <= 30, 5 / (k * V) * (1 - exp(-k * t)),
                  5 / (k * V) * (1 - exp(-k * 30)) * exp(-k * (t - 30)))
  ref <- referenceAutoinduction(V, X$michaelisMenten$clLinear, X$michaelisMenten$autoinduction$cl,
                                1e-12, 5, X$michaelisMenten$autoinduction$ec50, 0, 1,
                                data.frame(time = c(0, 30), rate = c(5, 0)), t)
  expect_equal(ref, exact, tolerance = 1e-6)
})

test_that("an invalid autoinduction block is refused", {
  mm <- list(vmax = 0, km = 1, clLinear = 0,
             autoinduction = list(cl = 0.05, kEnz = 0.001, imax = 1, ic50 = 8))
  expect_identical(validateMichaelisMenten(mm, "x"), mm)
  bad <- mm; bad$autoinduction$imax <- 1.5
  expect_error(validateMichaelisMenten(bad, "x"), "autoinduction")
  bad <- mm; bad$autoinduction$ic50 <- NULL
  expect_error(validateMichaelisMenten(bad, "x"), "autoinduction")
  bad <- mm; bad$autoinduction$kEnz <- 0
  expect_error(validateMichaelisMenten(bad, "x"), "autoinduction")
  bad <- list(vmax = 0, km = 1, clLinear = 0)
  expect_error(validateMichaelisMenten(bad, "x"), "vmax")
})
