# insulinAspartFiasp: see R/insulinAspart.R (the FDA review of Fiasp, NDA
# 208751, Table 5: a depot, three transit compartments at kt 55.8/h, one
# compartment).  Drafted by Claude Code, 2026-11-18.
#
# The expectations are computed from the source's own equations, in the
# source's own order (the depot first, then the three transits), by a matrix
# exponential independent of the engine's (Matrix::expm), not by the model.

aspartSource <- function(kt, f, W, t, dose_mU, ka = 1.53, cl = 67.8, v = 78.6,
                         ref = 75)
{
  CL <- cl * W / ref; V <- v * W / ref; k <- CL / V
  A <- matrix(0, 5, 5)
  A[1, 1] <- -ka; A[2, 1] <- ka
  A[2, 2] <- -kt; A[3, 2] <- kt
  A[3, 3] <- -kt; A[4, 3] <- kt
  A[4, 4] <- -kt; A[5, 4] <- kt
  A[5, 5] <- -k
  vapply(t / 60, function(h)
    (as.matrix(Matrix::expm(A * h)) %*% c(dose_mU * f, 0, 0, 0, 0))[5] / V,
    numeric(1))
}
noEventsAsp <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 5: the parameters, the chain and the subcutaneous route", {
  m <- insulinAspartFiasp(75, 175, 40, "male", adjustToFFM = FALSE)
  pk <- m$PK$default
  expect_equal(pk$v1, 78.6)
  expect_equal(pk$cl1, 67.8 / 60)
  expect_equal(pk$ka_SC, 1.53 / 60)
  expect_equal(pk$bioavailability_SC, 1)
  expect_equal(pk$tlag_SC, 0)
  expect_equal(m$transitChain$n, 4L)
  expect_equal(m$transitChain$ktr, 55.8 / 60)
  expect_equal(m$transitChain$route, "SC")
  expect_null(m$tPeak); expect_equal(m$ke0, INSULIN_KE0 / 60)   # R/insulinEffectSite.R
  # Weight: CL/F and V/F proportional to it, about 75 kg (assumed reference)
  w <- insulinAspartFiasp(90, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(w$v1, 78.6 * 90 / 75)
  expect_equal(w$cl1, 67.8 / 60 * 90 / 75)
  # Restated at the 70 kg reference man (Shafer, 2026-11-18): 63.28 L/h and
  # 73.36 L, the same in both positions of the switch
  ref <- insulinAspartFiasp(70, 170, 35, "male")$PK$default
  expect_equal(ref$cl1 * 60, 63.28)
  expect_equal(ref$v1, 73.36)
  expect_equal(insulinAspartFiasp(70, 170, 35, "male", FALSE)$PK$default[c("v1", "cl1")],
               ref[c("v1", "cl1")])
  # With the switch on, V/F by fat-free mass and CL/F by its 0.75 power
  on <- insulinAspartFiasp(90, 175, 40, "male")$PK$default
  ffm <- ffmAlSallami(90, 175, 40, "male") / ffmAlSallami(70, 170, 35, "male")
  expect_equal(on$v1, 73.36 * ffm)
  expect_equal(on$cl1 * 60, 63.28 * ffm^0.75)
  # Age group: CL/F x 0.905 from 65 years, V/F unchanged
  old <- insulinAspartFiasp(75, 175, 65, "male", adjustToFFM = FALSE)$PK$default
  young <- insulinAspartFiasp(75, 175, 64.9, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(old$cl1 / young$cl1, 0.905)
  expect_equal(old$v1, young$v1)
  # getDrugPK moves the subcutaneous fields onto the tissue depot
  p <- getDrugPK("insulinAspartFiasp", 75, 175, 40, "male", adjustToFFM = FALSE)
  expect_equal(p$PK$default$ka_RA, 1.53 / 60)
  expect_equal(p$PK$default$bioavailability_RA, 1)
  expect_equal(p$transitChain$route, "SC")
  expect_equal(p$tissueRoute, ROUTE_SC)
})

test_that("the engine reproduces the source's depot-first chain exactly", {
  skip_if_not_installed("Matrix")
  pk <- getDrugPK("insulinAspartFiasp", 75, 175, 40, "male", adjustToFFM = FALSE)
  s <- simCpCe(data.frame(Drug = "insulinAspartFiasp", Time = 0, Dose = 0.2,
                          Units = "U/kg SC"), noEventsAsp, pk, 720, FALSE)
  w <- s$wide
  ref <- aspartSource(55.8, 1, 75, w$Time, 0.2 * 75 * 1000)
  expect_lt(max(abs(w$Plasma - ref)) / max(ref), 1e-10)
  # The Depot panel: the whole bioavailable dose (in U) at the moment of the
  # injection, all of the chain
  expect_equal(s$compartments$Depot[s$compartments$Time == 0], 15)
})

test_that("the review's Figure 4 typical subject, 0.2 U/kg at 75 kg", {
  skip_if_not_installed("Matrix")
  # The figure (printed p 93), pixel-measured: about 564 pmol/L at about 58
  # min.  The model: 545.6 pmol/L at 54.8 min (R/insulinAspart.R, header).
  t <- seq(0, 720, by = 0.1)
  c <- aspartSource(55.8, 1, 75, t, 0.2 * 75 * 1000) * 6   # pmol/L
  expect_equal(max(c), 545.5846, tolerance = 1e-6)
  expect_equal(t[which.max(c)], 54.8)
  expect_lt(abs(max(c) / 564 - 1), 0.04)
  # a 70 kg reference would be 10% low, which the figure rules out
  expect_equal(max(c) * 70 / 75, 509.2123, tolerance = 1e-6)
  # The tail: at six hours, 2.8% of the peak (the review's Figure 2: ~6%)
  expect_equal(c[t == 360] / max(c), 0.02796031, tolerance = 1e-6)
})

test_that("a pump rate settles at F x rate / CL, exactly through the chain", {
  skip_if_not_installed("Matrix")
  pk <- getDrugPK("insulinAspartFiasp", 75, 175, 40, "male", adjustToFFM = FALSE)
  s <- simCpCe(data.frame(Drug = "insulinAspartFiasp", Time = c(0, 600),
                          Dose = c(1, 0), Units = "U/hr SC"),
               noEventsAsp, pk, 1200, FALSE)$wide
  # 1 U/hr = 1000/60 mU/min into the head; steady state 1000 / 67.8 mcU/mL,
  # within 0.04% after ten hours (the slowest rate, CL/V = 0.863/h, leaves
  # exp(-8.63) of the gap, times its coefficient)
  expect_equal(s$Plasma[s$Time == 600], 1000 / 67.8, tolerance = 1e-3)
  # The whole run against the source's chain fed by a constant rate: the
  # source's matrix augmented with a state held at 1 that feeds the depot at
  # 1000/60 mU/min until the pump stops at 600 min
  rateRef <- function(t) {
    k <- 67.8 / 78.6
    A <- matrix(0, 6, 6)
    A[1, 1] <- -1.53; A[2, 1] <- 1.53
    for (j in 2:4) { A[j, j] <- -55.8; A[j + 1, j] <- 55.8 }
    A[5, 5] <- -k
    A[1, 6] <- 1000                      # mU/h into the depot
    E <- function(h, M = A) as.matrix(Matrix::expm(M * h))
    x600 <- E(10) %*% c(0, 0, 0, 0, 0, 1)
    B <- A; B[1, 6] <- 0
    vapply(t / 60, function(h) {
      x <- if (h <= 10) E(h) %*% c(0, 0, 0, 0, 0, 1) else E(h - 10, B) %*% x600
      x[5] / 78.6
    }, numeric(1))
  }
  probe <- s$Time[findInterval(c(30, 120, 600, 660, 900), s$Time)]
  expect_equal(s$Plasma[match(probe, s$Time)], rateRef(probe), tolerance = 1e-9)
})

test_that("mass balance: the AUC of an injection is F x dose / CL", {
  pk <- getDrugPK("insulinAspartFiasp", 75, 175, 40, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "insulinAspartFiasp", Time = 0, Dose = 10,
                          Units = "U SC"), noEventsAsp, pk, 2880, FALSE)$wide
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  expect_equal(auc, 10000 / 67.8, tolerance = 0.005)
})

test_that("Fiasp offers the pump, and nothing intravenous or oral", {
  u <- getDrugDefaults("insulinAspartFiasp")$Units[[1]]
  expect_true("U/hr SC" %in% u)
  expect_false(any(doseRoute(u) %in% c(ROUTE_IV, ROUTE_PO)))
})

test_that("pregnancy: CL/F x 1.25, absorption and V/F unchanged", {
  on  <- getDrugPK("insulinAspartFiasp", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("insulinAspartFiasp", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("insulinAspartFiasp", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  same <- c("v1", "ka_RA", "bioavailability_RA", "tlag_RA")
  expect_identical(a[same], b[same])
  expect_identical(on$transitChain, off$transitChain)
  expect_match(on$reference, "Catalano", fixed = TRUE)
  expect_no_match(off$reference, "Catalano", fixed = TRUE)
  # The values the model gives with the flag on, at the woman's
  # pharmacokinetic weight
  actual <- insulinAspartFiasp(70, 165, 30, "female", pregnant = TRUE)$PK$default
  ffm <- ffmAlSallami(70, 165, 30, "female") / ffmAlSallami(70, 170, 35, "male")
  expect_equal(actual$v1, 73.36 * ffm)
  expect_equal(actual$cl1, 63.28 * ffm^0.75 * 1.25 / 60)
})
