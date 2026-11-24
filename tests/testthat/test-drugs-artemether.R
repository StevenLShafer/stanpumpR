# Artemether with dihydroartemisinin (R/drugs_artemether.R): Ding 2026,
# Table 2.  Drafted by Claude Code, 2026-11-24.  Expectations are computed
# here from the paper, not from the drug file.

ding2 <- function(w) list(
  cl = 79.3 * (w / 45)^0.75, q = 21.7 * (w / 45)^0.75,
  vc = 141 * w / 45, vp = 283 * w / 45,
  clD = 255 * (w / 45)^0.75, vD = 64.1 * w / 45)

test_that("Ding 2026, Table 2, switch off", {
  for (w in c(20, 45, 80)) {
    x <- artemether(w, 170, 30, "male", adjustToFFM = FALSE)
    set <- x$PK$default
    ref <- ding2(w)
    expect_equal(set$cl1, ref$cl / 60)
    expect_equal(set$cl2, ref$q / 60)
    expect_equal(set$v1, ref$vc)
    expect_equal(set$v2, ref$vp)
    expect_equal(set$cl3, 0)
    tc <- x$transitChain
    # The dose depot and two transit compartments, each at 3 / MTT
    expect_identical(tc$n, 3L)
    expect_equal(tc$ktr * 60, 3 / 1.55)
    expect_equal(set$ka_PO * 60, 3 / 1.55)
    expect_equal(set$bioavailability_PO, 1)
    # All of the elimination forms dihydroartemisinin
    expect_equal(tc$kMetabolic, set$cl1 / set$v1)
    expect_equal(tc$doseOccasionClearance, list(slope = 0.551, occasions = 6L))
    expect_identical(tc$metabolite$name, "dihydroartemisinin")
    expect_identical(tc$metabolite$n, 0L)
    expect_equal(tc$metabolite$v, ref$vD)
    expect_equal(tc$metabolite$kel * 60, ref$clD / ref$vD)
  }
})

test_that("switch on: weight at the pharmacokinetic weight", {
  pkW <- pkSizeFactors(90, 175, 40, "female")$pkWeight
  set <- artemether(90, 175, 40, "female")$PK$default
  expect_equal(set$cl1, ding2(pkW)$cl / 60)
  expect_equal(set$v1, ding2(pkW)$vc)
  dha <- dihydroartemisinin(90, 175, 40, "female")$PK$default
  expect_equal(dha$cl1, ding2(pkW)$clD / 60)
  expect_equal(dha$v1, ding2(pkW)$vD)
})

test_that("Table 2's terminal half-life of 12.1 h", {
  set <- artemether(45, 160, 30, "male", adjustToFFM = FALSE)$PK$default
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  a <- k10 + k12 + k21
  lambdaZ <- (a - sqrt(a^2 - 4 * k10 * k21)) / 2
  # the typical patient's first occasion; Table 2's 12.1 h is a median of
  # the post hoc estimates
  expect_equal(round(log(2) / lambdaZ / 60, 1), 11.8)
})

# An independent solution of Ding's equations: classical Runge-Kutta on a
# fine step, the clearance stepped at each dose, in hours, mg and litres.
dingReference <- function(w, doseMg, doseTimes, outTimes, dt = 0.005)
{
  p <- ding2(w)
  ktr <- 3 / 1.55
  x <- numeric(6)     # depot, transit 1, transit 2, central, peripheral, DHA
  occ <- function(t) min(6, max(1, sum(doseTimes <= t + 1e-9)))
  f <- function(x, cl) {
    c(-ktr * x[1],
      ktr * x[1] - ktr * x[2],
      ktr * x[2] - ktr * x[3],
      ktr * x[3] - (cl + p$q) / p$vc * x[4] + p$q / p$vp * x[5],
      p$q / p$vc * x[4] - p$q / p$vp * x[5],
      cl / p$vc * x[4] - p$clD / p$vD * x[6])
  }
  steps <- round(max(outTimes) / dt)
  out <- matrix(NA_real_, length(outTimes), 2)
  outStep <- round(outTimes / dt)
  doseStep <- round(doseTimes / dt)
  for (i in 0:steps) {
    if (i %in% doseStep) x[1] <- x[1] + doseMg * sum(doseStep == i)
    j <- which(outStep == i)
    if (length(j)) out[j, ] <- matrix(c(x[4] / p$vc, x[6] / p$vD) * 1000,
                                      length(j), 2, byrow = TRUE)
    if (i == steps) break
    cl <- p$cl * (1 + 0.551 * (occ(i * dt) - 1))
    k1 <- f(x, cl); k2 <- f(x + dt / 2 * k1, cl)
    k3 <- f(x + dt / 2 * k2, cl); k4 <- f(x + dt * k3, cl)
    x <- x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  out
}

test_that("the engine, dose occasions included, against an independent solution", {
  doseTimes <- c(0, 8, 24, 36, 48, 60)
  dt <- data.frame(Drug = "artemether", Time = doseTimes * 60, Dose = 80,
                   Units = "mg PO")
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  check <- c(1, 2, 7.9, 9.5, 25, 37, 49.5, 61, 62.5, 70, 90)
  # The engine's own points at the checked times, not an interpolation
  r <- withOutputTimes(check * 60, simulateDrugsWithCovariates(
    dt, events, 45, 160, 30, "male", 4 * 1440, FALSE, adjustToFFM = FALSE))
  a <- r$artemether$wide
  d <- r$dihydroartemisinin$wide
  ref <- dingReference(45, 80, doseTimes, check)
  at <- match(round(check * 60, 6), round(a$Time, 6))
  expect_false(anyNA(at))
  expect_equal(unname(cbind(a$Plasma[at], d$Plasma[at])), ref, tolerance = 1e-6)
})

test_that("the typical patient's course against Table 2's medians", {
  dt <- data.frame(Drug = "Coartem", Time = c(0, 8, 24, 36, 48, 60) * 60,
                   Dose = 4, Units = "tablet PO")
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  r <- simulateDrugsWithCovariates(dt, events, 45, 160, 30, "male", 4 * 1440, FALSE,
                                   adjustToFFM = FALSE)
  a <- r$artemether$wide
  d <- r$dihydroartemisinin$wide
  first <- a$Time < 8 * 60
  # Table 2: Cmax 256 (artemether), 135 (dihydroartemisinin), medians
  expect_equal(round(max(a$Plasma[first])), 239)
  expect_equal(round(max(d$Plasma)), 128)
  expect_gt(a$Time[first][which.max(a$Plasma[first])] / 60, 1.8)
  expect_lt(a$Time[first][which.max(a$Plasma[first])] / 60, 2.3)
  # Dihydroartemisinin's area is the whole dose over its own clearance,
  # 480 mg / 255 L/h, whatever artemether's clearance does
  aucD <- sum(diff(d$Time) * (head(d$Plasma, -1) + tail(d$Plasma, -1)) / 2) / 60
  expect_equal(aucD / (480000 / 255), 1, tolerance = 0.02)
  # Artemether's peaks fall as its clearance rises
  peak6 <- max(a$Plasma[a$Time >= 60 * 60 & a$Time < 68 * 60])
  expect_lt(peak6, 0.6 * max(a$Plasma[first]))
})

test_that("a seventh dose keeps the sixth occasion's clearance", {
  doseTimes <- c(0, 8, 24, 36, 48, 60, 72)
  ref <- dingReference(45, 80, doseTimes, c(74, 80))
  dt <- data.frame(Drug = "artemether", Time = doseTimes * 60, Dose = 80,
                   Units = "mg PO")
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  a <- withOutputTimes(c(74, 80) * 60, simulateDrugsWithCovariates(
    dt, events, 45, 160, 30, "male", 4 * 1440, FALSE,
    adjustToFFM = FALSE))$artemether$wide
  expect_equal(a$Plasma[match(c(74, 80) * 60, a$Time)], ref[, 1], tolerance = 1e-6)
})

test_that("no band, no effect site, not offered on its own", {
  dd <- getDrugDefaultsGlobal()
  for (drug in c("artemether", "dihydroartemisinin")) {
    row <- dd[dd$Drug == drug, ]
    expect_equal(c(row$Lower, row$Typical, row$Upper, row$MEAC, row$endCe), c(0, 0, 0, 0, 0))
    expect_false(isDosableDrug(drug, dd))
    expect_true(is.na(row$Category) || !nzchar(row$Category))
  }
  expect_equal(artemether(70, 170, 40, "male")$tPeak, 0)
})
