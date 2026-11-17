# Norpethidine, pethidine's toxic metabolite: see the header of
# R/drugs_norpethidine.R for how the model was derived.
#
# The derivation is reproduced here independently of the package: the
# library's pethidine (Bjorkman 2003 at 70 kg, written out from its micro
# constants) forms norpethidine from all of its clearance by moles, and the
# four-compartment rate equations are solved with a matrix exponential
# (eigendecomposition), sharing no code with the package's closed form.
#
# (Claude Code, 2026-11-13, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# --- Independent reference ---------------------------------------------------
#
# States (mg): pethidine central, peripheral 1, peripheral 2, norpethidine.
norSystem <- function(V = 1095) {
  v1 <- 18.1
  k10 <- 0.042131499; k12 <- 0.300022713; k13 <- 0.098461009
  k21 <- 0.089209873; k31 <- 0.010774228
  kel <- log(2) / (20.55 * 60)
  mw <- 233.31 / 247.33
  K <- matrix(0, 4, 4)
  K[1, 1] <- -(k10 + k12 + k13); K[1, 2] <- k21; K[1, 3] <- k31
  K[2, 1] <- k12; K[2, 2] <- -k21
  K[3, 1] <- k13; K[3, 3] <- -k31
  K[4, 1] <- mw * k10; K[4, 4] <- -kel
  e <- eigen(K)
  list(K = K, V = e$vectors, Vi = solve(e$vectors), lambda = e$values,
       v1 = v1, vm = V)
}
norStep <- function(S, x, rate, h) {
  E <- Re(S$V %*% diag(exp(S$lambda * h)) %*% S$Vi)
  as.vector(E %*% x + solve(S$K, (E - diag(4)) %*% c(rate, 0, 0, 0)))
}
# Boluses (mg) at `times`, then a constant rate from `rateFrom`; read at `at`.
norReference <- function(times = numeric(0), doses = numeric(0), at,
                         rate = 0, rateFrom = Inf, V = 1095) {
  S <- norSystem(V)
  x <- rep(0, 4); t <- 0
  ev <- sort(unique(c(times[times < at], if (rateFrom < at) rateFrom)))
  for (tt in ev) {
    x <- norStep(S, x, if (t >= rateFrom) rate else 0, tt - t); t <- tt
    x[1] <- x[1] + sum(doses[times == tt])
  }
  x <- norStep(S, x, if (t >= rateFrom) rate else 0, at - t)
  c(pethidine = x[1] / S$v1, norpethidine = x[4] / S$vm)
}

engineAt <- function(DT, at) {
  o <- simulateDrugsWithCovariates(DT, noEvents, 70, 170, 35, "male", at, FALSE,
                                   adjustToFFM = FALSE)
  p <- o$pethidine$wide
  m <- o$norpethidine$wide
  c(pethidine = p$Plasma[nrow(p)], norpethidine = m$Plasma[nrow(m)])
}

# Kuhnert 1985's mean regimen: 79.04 mg in 2.58 intravenous doses, the first
# 303 min and the last 150 min before delivery (Table I).
KUHNERT_TIMES <- c(0, 76.5, 153)
KUHNERT_DOSES <- c(79.04 / 2.58, 79.04 - 2 * 79.04 / 2.58, 79.04 / 2.58)
KUHNERT_DELIVERY <- 303


# --- The derivation -----------------------------------------------------------

test_that("the apparent volume reproduces Kuhnert 1985's norpethidine at delivery", {
  # The amount formed by delivery, divided by 33.6 ng/mL, is the volume.
  S <- norSystem()
  ref <- norReference(KUHNERT_TIMES, KUHNERT_DOSES, KUHNERT_DELIVERY)
  amount <- ref[["norpethidine"]] * 1095
  expect_equal(amount / 0.0336, 1095, tolerance = 0.001)
  # The model gives Table II's mean, 33.6 ng/mL
  expect_equal(ref[["norpethidine"]], 0.0336, tolerance = 0.001)
  # How the 2.58 doses are spread hardly matters (header: 1089 to 1097 L)
  two <- norReference(c(0, 153), rep(79.04 / 2, 2), KUHNERT_DELIVERY)
  expect_equal(two[["norpethidine"]] * 1095 / 0.0336, 1089, tolerance = 0.002)
  # And the library's pethidine forms it from the pethidine those mothers had:
  # 124 ng/mL at delivery against Kuhnert's 129.2 (Table II)
  expect_equal(ref[["pethidine"]], 0.1237, tolerance = 0.002)
  expect_lt(abs(ref[["pethidine"]] / 0.1292 - 1), 0.05)
})

test_that("norpethidine is one compartment with Kuhnert's half-life", {
  actual <- norpethidine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(actual$v1, 1095)
  expect_equal(actual$cl1, 1095 * log(2) / (20.55 * 60), tolerance = 1e-12)
  expect_equal(actual$cl2, 0)
  expect_equal(actual$cl3, 0)
  PK <- getDrugPK("norpethidine", 70, 170, 35, "male",
                  getDrugDefaults("norpethidine"), adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / PK$lambda_1 / 60, 20.55, tolerance = 1e-9)
  # Apparent clearance 36.9 L/h (header)
  expect_equal(actual$cl1 * 60, 36.92, tolerance = 0.001)
})

test_that("norpethidine scales with pethidine, identically", {
  # 120 kg man: the pair's apparent scale stays consistent only if both
  # take the same factors.
  pet <- pethidine(120, 170, 50, "male")$PK$default
  nor <- norpethidine(120, 170, 50, "male")$PK$default
  expect_equal(nor$v1 / 1095, pet$v1 / 18.1, tolerance = 1e-12)
  expect_equal(nor$cl1 / (1095 * log(2) / (20.55 * 60)), pet$cl1 / 0.7625801,
               tolerance = 1e-6)
})


# --- Through the engine -------------------------------------------------------

test_that("the engine follows the rate equations", {
  DT <- data.frame(Drug = "pethidine", Time = KUHNERT_TIMES, Dose = KUHNERT_DOSES,
                   Units = "mg")
  for (at in c(30, 153, KUHNERT_DELIVERY, 1440, 4320)) {
    expect_equal(engineAt(DT[DT$Time < at, ], at),
                 norReference(KUHNERT_TIMES, KUHNERT_DOSES, at),
                 tolerance = 1e-7, label = paste("boluses, minute", at))
  }
  inf <- data.frame(Drug = "pethidine", Time = 0, Dose = 25, Units = "mg/hr")
  for (h in c(6, 24, 48)) {
    expect_equal(engineAt(inf, h * 60),
                 norReference(at = h * 60, rate = 25 / 60, rateFrom = 0),
                 tolerance = 1e-7, label = paste("infusion, hour", h))
  }
})

test_that("against Austin 1981, unfitted, the model reaches about 60% at 48 h", {
  # 25 mg/h for 2 days: Austin's blood norpethidine rose 0.013 mcg/mL/h,
  # 0.624 at 48 h.  The model's plasma, over 1.25 for blood (Verbeeck 1981):
  # header values 0.29 / 0.48 plasma, 0.23 / 0.39 blood at 24 / 48 h.
  inf <- data.frame(Drug = "pethidine", Time = 0, Dose = 25, Units = "mg/hr")
  p24 <- engineAt(inf, 24 * 60)[["norpethidine"]]
  p48 <- engineAt(inf, 48 * 60)[["norpethidine"]]
  expect_equal(p24, 0.2927, tolerance = 0.002)
  expect_equal(p48, 0.4832, tolerance = 0.002)
  expect_equal(p48 / 1.25 / (0.013 * 48), 0.62, tolerance = 0.02)
})

test_that("the pethidine curve is unchanged by the metabolite", {
  PK <- getDrugPK("pethidine", 70, 170, 35, "male", getDrugDefaults("pethidine"))
  noMet <- PK
  noMet$PK$default$metabolite <- NULL
  noMet$metaboliteName <- NULL
  DT <- data.frame(Drug = "pethidine", Time = c(0, 60, 300),
                   Dose = c(50, 25, 0), Units = c("mg", "mg/hr", "mg/hr"))
  a <- simCpCe(DT, noEvents, PK, 600, FALSE)
  b <- simCpCe(DT, noEvents, noMet, 600, FALSE)
  common <- intersect(a$wide$Time, b$wide$Time)
  expect_gt(length(common), 50)
  expect_equal(a$wide$Plasma[match(common, a$wide$Time)],
               b$wide$Plasma[match(common, b$wide$Time)], tolerance = 1e-10)
  expect_equal(a$wide$"Effect Site"[match(common, a$wide$Time)],
               b$wide$"Effect Site"[match(common, b$wide$Time)], tolerance = 1e-10)
})

test_that("norpethidine has no dosing unit, no effect site, no band, no MEAC", {
  d <- getDrugDefaults("norpethidine")
  expect_true(is.na(d$Units) || d$Units == "")
  expect_true(is.na(d$Category) || d$Category == "")
  expect_equal(c(d$Lower, d$Upper, d$Typical, d$MEAC, d$endCe), rep(0, 5))
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "pethidine", Time = 0, Dose = 50, Units = "mg"),
    noEvents, 70, 170, 35, "male", 600, FALSE)
  expect_equal(o$norpethidine$formedFrom, "pethidine")
  expect_true(all(is.na(o$norpethidine$wide$"Effect Site")))
  expect_equal(max(o$norpethidine$equiSpace$MEAC), 0)
})
