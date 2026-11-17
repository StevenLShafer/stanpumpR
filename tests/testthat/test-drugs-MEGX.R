# MEGX, lidocaine's toxic metabolite: see the header of R/drugs_MEGX.R.
#
# The pins are Foong 2025 Table IV converted by hand from L/h to L/min.  The
# curves are checked against the compartmental rate equations written out
# below -- lidocaine's two compartments (Schnider 1996 set C at 70 kg, the
# switch off), its tissue depot, Foong's transit compartment and MEGX's two
# compartments -- solved with a matrix exponential (eigendecomposition),
# sharing no code with the package's closed form.  That is the check of the
# two pieces of engine this drug needed: the transit step in
# metaboliteCoefficients() and the tissue (RA) route in
# advanceClosedFormMetabolite().  Engine values are read at the end of each
# run, where every grid has a point.
#
# (Claude Code, 2026-11-13, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# --- Independent reference ---------------------------------------------------
#
# States (mg): lidocaine central, lidocaine peripheral, tissue depot, transit,
# MEGX central, MEGX peripheral.  All of lidocaine's elimination becomes MEGX
# by moles (206.28 / 234.34) and enters the transit compartment.
megxSystem <- function() {
  v1 <- 7.07; v2 <- 31.64; cl1 <- 1.505; cl2 <- 4.123      # Schnider, 70 kg
  ka <- 0.0124                                              # lidocaine RA
  ktr <- 1 / (0.513 * 60)                                   # Foong TRTm1
  m1 <- 30.7; m2 <- 480; mc1 <- 52.8 / 60; mc2 <- 442 / 60  # Foong Table IV
  mw <- 206.28 / 234.34
  K <- matrix(0, 6, 6)
  K[1, 1] <- -(cl1 + cl2) / v1; K[1, 2] <- cl2 / v2; K[1, 3] <- ka
  K[2, 1] <- cl2 / v1;          K[2, 2] <- -cl2 / v2
  K[3, 3] <- -ka
  K[4, 1] <- mw * cl1 / v1;     K[4, 4] <- -ktr
  K[5, 4] <- ktr; K[5, 5] <- -(mc1 + mc2) / m1; K[5, 6] <- mc2 / m2
  K[6, 5] <- mc2 / m1;          K[6, 6] <- -mc2 / m2
  e <- eigen(K)
  list(K = K, V = e$vectors, Vi = solve(e$vectors), lambda = e$values,
       v1 = v1, m1 = m1)
}

# Amounts after h minutes of constant inputs b (mg/min per state), from x.
megxStep <- function(S, x, b, h) {
  E <- Re(S$V %*% diag(exp(S$lambda * h)) %*% S$Vi)
  as.vector(E %*% x + solve(S$K, (E - diag(6)) %*% b))
}

# Lidocaine and MEGX (mg/L) at `at` minutes.  `events` is a list of
# list(time, bolus = c(central, depot), rate = c(iv, ra)); a rate persists
# until the next event that sets one.
megxReference <- function(events, at) {
  S <- megxSystem()
  x <- rep(0, 6); rate <- c(0, 0); t <- 0
  for (ev in events) {
    if (ev$time >= at) break
    if (ev$time > t) { x <- megxStep(S, x, c(rate[1], 0, rate[2], 0, 0, 0), ev$time - t); t <- ev$time }
    if (!is.null(ev$bolus)) x[c(1, 3)] <- x[c(1, 3)] + ev$bolus
    if (!is.null(ev$rate)) rate <- ev$rate
  }
  x <- megxStep(S, x, c(rate[1], 0, rate[2], 0, 0, 0), at - t)
  c(lidocaine = x[1] / S$v1, MEGX = x[5] / S$m1)
}

engineAt <- function(DT, at) {
  o <- simulateDrugsWithCovariates(DT, noEvents, 70, 170, 35, "male", at, FALSE,
                                   adjustToFFM = FALSE)
  l <- o$lidocaine$wide
  m <- o$MEGX$wide
  c(lidocaine = l$Plasma[nrow(l)], MEGX = m$Plasma[nrow(m)])
}


# --- The model ----------------------------------------------------------------

test_that("MEGX returns Foong 2025 Table IV at 70 kg", {
  actual <- MEGX(70, 170, 35, "male")
  expect_equal_rounded(actual$PK$default, list(
    v1 = 30.7, v2 = 480, v3 = 1,
    cl1 = 52.8 / 60, cl2 = 442 / 60, cl3 = 0
  ))
  expect_equal(actual$tPeak, 0)
  expect_equal(actual$MEAC, 0)
  expect_equal(c(actual$typical, actual$lowerTypical, actual$upperTypical), c(0, 0, 0))
  expect_match(actual$reference, "Pharm Res 2025;42:451-473", fixed = TRUE)
})

test_that("MEGX scales with total body mass, as fitted, whatever the switch", {
  # 100 kg: volumes x 100/70, clearances x (100/70)^0.75 = 1.306702 (by hand)
  on  <- MEGX(100, 170, 35, "male", adjustToFFM = TRUE)$PK$default
  off <- MEGX(100, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(on, off)
  expect_equal(on$v1, 30.7 * 100 / 70, tolerance = 1e-12)
  expect_equal(on$v2, 480 * 100 / 70, tolerance = 1e-12)
  expect_equal(on$cl1, 52.8 / 60 * 1.306702, tolerance = 1e-6)
  expect_equal(on$cl2, 442 / 60 * 1.306702, tolerance = 1e-6)
  # A woman of the same weight is the same: no sex or height term
  expect_equal(MEGX(100, 160, 70, "female")$PK$default, on)
})

test_that("the half-lives are those of Table IV's parameters", {
  # Two-compartment exponents from the micro constants, by hand:
  # k10 = 0.880/30.7, k12 = 7.367/30.7, k21 = 7.367/480 (per min)
  PK <- getDrugPK("MEGX", 70, 170, 35, "male", getDrugDefaults("MEGX"))$PK$default
  expect_equal(log(2) / PK$lambda_1, 2.454395, tolerance = 1e-5)    # min
  expect_equal(log(2) / PK$lambda_2 / 60, 7.416194, tolerance = 1e-5) # h
})


# --- Formation from lidocaine -------------------------------------------------

test_that("lidocaine forms MEGX from all of its clearance, through the transit", {
  PK <- getDrugPK("lidocaine", 70, 170, 35, "male", getDrugDefaults("lidocaine"),
                  adjustToFFM = FALSE)$PK$default
  expect_equal(PK$metabolite$name, "MEGX")
  # K = kFormation x V1 x mwRatio = CL1 x 206.28/234.34, mg of MEGX per
  # minute per mg/L of lidocaine
  expect_equal(PK$metabolite$coefs$K, 1.505 * 206.28 / 234.34, tolerance = 1e-12)
  # Parent (2) + MEGX (2) + transit (1) + tissue depot (1) exponentials
  expect_length(PK$metabolite$coefs$lambda, 6)
  expect_true(any(abs(PK$metabolite$coefs$lambda - 1 / (0.513 * 60)) < 1e-9))
  expect_true(any(abs(PK$metabolite$coefs$lambda - 0.0124) < 1e-9))
})

test_that("steady state is the infusion rate x the molar ratio over MEGX clearance", {
  # 1.5 mg/kg/h at 70 kg = 105 mg/h for 14 days: every milligram given
  # becomes 0.8803 mg of MEGX, so MEGX = 105 x 0.8803 / 52.8 = 1.7505 mg/L,
  # whatever lidocaine's clearance.  Lidocaine = 105/60 / 1.505 = 1.1628.
  at <- 14 * 1440
  got <- engineAt(data.frame(Drug = "lidocaine", Time = 0, Dose = 105, Units = "mg/hr"), at)
  expect_equal(unname(got["MEGX"]), 105 * (206.28 / 234.34) / 52.8, tolerance = 1e-6)
  expect_equal(unname(got["lidocaine"]), 105 / 60 / 1.505, tolerance = 1e-6)
})

test_that("an intravenous bolus and infusion follow the rate equations", {
  # Foong's protocol at 70 kg: 105 mg over 10 min, then 105 mg/h for 2 h,
  # then stop; read during, at the end and over the next day.
  DT <- data.frame(Drug = "lidocaine", Time = c(0, 10, 130),
                   Dose = c(630, 105, 0), Units = "mg/hr")
  ev <- list(list(time = 0, rate = c(10.5, 0)), list(time = 10, rate = c(1.75, 0)),
             list(time = 130, rate = c(0, 0)))
  for (at in c(5, 30, 130, 180, 360, 720, 1440)) {
    expect_equal(engineAt(DT[DT$Time < at, ], at), megxReference(ev, at),
                 tolerance = 1e-7, label = paste("infusion, minute", at))
  }
  bolus <- data.frame(Drug = "lidocaine", Time = 0, Dose = 100, Units = "mg")
  for (at in c(1, 20, 90, 600)) {
    expect_equal(engineAt(bolus, at), megxReference(list(list(time = 0, bolus = c(100, 0))), at),
                 tolerance = 1e-7, label = paste("bolus, minute", at))
  }
})

test_that("a nerve block and a perineural infusion form MEGX too", {
  # 400 mg RA at 0; then a perineural catheter at 50 mg/h from 60 to 1500 min.
  DT <- data.frame(Drug = "lidocaine", Time = c(0, 60, 1500),
                   Dose = c(400, 50, 0), Units = c("mg RA", "mg/hr RA", "mg/hr RA"))
  ev <- list(list(time = 0, bolus = c(0, 400)),
             list(time = 60, rate = c(0, 50 / 60)),
             list(time = 1500, rate = c(0, 0)))
  for (at in c(30, 60, 240, 1500, 2000, 2880)) {
    expect_equal(engineAt(DT[DT$Time < at, ], at), megxReference(ev, at),
                 tolerance = 1e-7, label = paste("RA, minute", at))
  }
})

test_that("the lidocaine curve is unchanged by the metabolite", {
  # advanceClosedFormMetabolite() must give the parent the same Cp that
  # advanceClosedFormPO_IM_IN() gives it without one.
  PK <- getDrugPK("lidocaine", 70, 170, 35, "male", getDrugDefaults("lidocaine"))
  noMet <- PK
  noMet$PK$default$metabolite <- NULL
  noMet$metaboliteName <- NULL
  DT <- data.frame(Drug = "lidocaine", Time = c(0, 0, 60, 300),
                   Dose = c(100, 300, 40, 0),
                   Units = c("mg", "mg RA", "mg/hr RA", "mg/hr RA"))
  a <- simCpCe(DT, noEvents, PK, 600, FALSE)
  b <- simCpCe(DT, noEvents, noMet, 600, FALSE)
  common <- intersect(a$wide$Time, b$wide$Time)
  expect_gt(length(common), 50)
  pa <- a$wide$Plasma[match(common, a$wide$Time)]
  pb <- b$wide$Plasma[match(common, b$wide$Time)]
  expect_equal(pa, pb, tolerance = 1e-10)
  ea <- a$wide$"Effect Site"[match(common, a$wide$Time)]
  eb <- b$wide$"Effect Site"[match(common, b$wide$Time)]
  expect_equal(ea, eb, tolerance = 1e-10)
})

test_that("MEGX has no dosing unit, no effect site and no band", {
  d <- getDrugDefaults("MEGX")
  expect_true(is.na(d$Units) || d$Units == "")
  expect_true(is.na(d$Category) || d$Category == "")
  expect_equal(c(d$Lower, d$Upper, d$Typical, d$MEAC, d$endCe), rep(0, 5))
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "lidocaine", Time = 0, Dose = 100, Units = "mg"),
    noEvents, 70, 170, 35, "male", 600, FALSE)
  expect_equal(o$MEGX$formedFrom, "lidocaine")
  expect_true(all(is.na(o$MEGX$wide$"Effect Site")))
  expect_gt(max(o$MEGX$wide$Plasma), 0)
})
