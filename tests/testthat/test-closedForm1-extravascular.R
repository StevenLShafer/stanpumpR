# Oral, intramuscular and intranasal doses in advanceClosedForm1(), the engine
# that switches PK set on a clinical event.
#
# Until 2026-10-07 that engine had no extravascular route: anything that was
# not a bolus was taken as an infusion rate, so "10 mg PO" ran as 10 mg/min
# until the next rate change.  No drug in the library had both a PK event and
# an extravascular route, so the drugs here are given a PK event by hand: a
# second PK set taken from a heavier patient, switched in at 50 minutes.
#
# The reference is an independent solution of the same system -- an absorption
# depot feeding a three-compartment model, amounts carried across the switch --
# by matrix exponential, which shares no code with the engines.  Writing it
# exposed a second, older defect: convertState() kept a ONE-compartment
# model's concentration continuous across a change in V1 instead of its
# amount.
#
# (Claude Code, 2026-10-07, at the request of Steven L. Shafer; run on R 4.3.3.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

pkWith <- function(drug, weight = 70) {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK(drug, weight, 170, 50, "male", dd[dd$Drug == drug, ])
  PK$endCe <- dd$endCe[dd$Drug == drug]
  PK
}

# The drug's own PK until 50 minutes, then a 130 kg patient's.  `same` keeps
# the first set on both sides, so that only the engine changes.
switchedPK <- function(drug, same = FALSE) {
  PK <- pkWith(drug)
  PK$PK$Switch <- if (same) PK$PK$default else pkWith(drug, 130)$PK$default
  PK$pkEvents <- c(PK$pkEvents, "Switch")
  PK
}
switchAt50 <- data.frame(Time = 50, Event = "Switch")

# exp(M) by scaling and squaring a Taylor series; M is small and well scaled.
expmTaylor <- function(M) {
  s <- max(0, ceiling(log2(max(abs(M)) * 4 + 1e-300)))
  A <- M / 2^s
  E <- term <- diag(nrow(M))
  for (k in 1:20) { term <- term %*% A / k; E <- E + term }
  for (i in seq_len(s)) E <- E %*% E
  E
}

# Plasma concentration from first principles.  State: depot, central,
# peripheral 2, peripheral 3 (amounts) and a constant 1 that carries the
# infusion rate.  Doses here are mg; `scale` converts mg/L to plotted units.
exactCp <- function(DT, sets, switchAt, times, weight = 70, scale = 1) {
  setAt <- function(t) if (t < switchAt) sets[[1]] else sets[[2]]
  sys <- function(s, R) {
    A <- matrix(0, 5, 5)
    A[1, 1] <- -s$ka_PO
    A[2, 1] <-  s$ka_PO
    A[2, 2] <- -(s$k10 + s$k12 + s$k13)
    A[2, 3] <-  s$k21; A[3, 2] <- s$k12; A[3, 3] <- -s$k21
    A[2, 4] <-  s$k31; A[4, 2] <- s$k13; A[4, 4] <- -s$k31
    A[2, 5] <-  R
    A
  }
  perKg <- ifelse(grepl("kg", DT$Units), weight, 1)
  isRate <- grepl("hr", DT$Units)
  rateAt <- function(t) {
    i <- which(isRate & DT$Time <= t)
    if (length(i) == 0) 0 else DT$Dose[max(i)] * perKg[max(i)] / 60
  }
  breaks <- sort(unique(c(0, DT$Time, switchAt, times)))
  x <- c(0, 0, 0, 0, 1)
  out <- setNames(numeric(length(times)), times)
  for (k in seq_along(breaks)) {
    t <- breaks[k]
    s <- setAt(t)
    for (d in which(DT$Time == t & !isRate)) {
      if (grepl("PO", DT$Units[d])) x[1] <- x[1] + DT$Dose[d] * perKg[d] * s$bioavailability_PO
      else x[2] <- x[2] + DT$Dose[d] * perKg[d]
    }
    if (t %in% times) out[as.character(t)] <- x[2] / s$v1 * scale
    if (k < length(breaks))
      x <- as.vector(expmTaylor(sys(s, rateAt(t)) * (breaks[k + 1] - t)) %*% x)
  }
  unname(out)
}

# Engine output at its own time points, so nothing is interpolated.
engineCp <- function(sim, from = 0) {
  w <- sim$wide
  w[w$Time >= from, c("Time", "Plasma")]
}


test_that("an oral dose is absorbed, not infused, across a change in PK", {
  for (drug in c("clindamycin", "hydromorphone")) {
    scale <- if (drug == "hydromorphone") 1000 else 1      # ng/mL from mg/L
    mg    <- if (drug == "hydromorphone") c(4, 1, 0.003, 3, 0) else c(600, 300, 0.5, 450, 0)
    DT <- data.frame(Drug = drug, Time = c(0, 20, 30, 50, 100), Dose = mg,
                     Units = c("mg PO", "mg", "mg/kg/hr", "mg PO", "mg/kg/hr"))
    PK  <- switchedPK(drug)
    sim <- simCpCe(DT, switchAt50, PK, 300, FALSE)
    got <- engineCp(sim)
    # Every engine point but the instant of the switch, where the step into it
    # runs on the new eigenvalues for 0.01 minutes (an older approximation,
    # good to 1e-4 here and untouched by this change).
    got <- got[abs(got$Time - 50) > 1e-9, ]
    ref <- exactCp(DT, list(PK$PK$default, PK$PK$Switch), 50, got$Time, scale = scale)
    keep <- ref > max(ref) * 1e-3
    expect_lt(max(abs(got$Plasma[keep] - ref[keep]) / ref[keep]), 2e-3,
              label = paste(drug, "largest relative error against the exact solution"))
    # And well after the switch, where nothing approximate remains
    late <- got$Time > 60 & keep
    expect_lt(max(abs(got$Plasma[late] - ref[late]) / ref[late]), 2e-4,
              label = paste(drug, "after the switch"))
  }
})


test_that("with the same PK on both sides of an event it matches the oral engine", {
  for (drug in c("hydromorphone", "clindamycin", "cefalexin")) {
    po <- "mg PO"
    DT <- data.frame(Drug = drug, Time = c(0, 90, 240), Dose = c(4, 4, 4), Units = po)
    if (drug != "hydromorphone") DT$Dose <- c(500, 500, 500)
    if (drug == "hydromorphone")
      DT <- rbind(DT, data.frame(Drug = drug, Time = 20, Dose = 1, Units = "mg"))
    PK <- pkWith(drug)
    if (PK$endCe == 0) PK$endCe <- 2         # no default threshold yet: time something
    one <- simCpCe(DT, noEvents, PK, 480, TRUE)

    PK2 <- switchedPK(drug, same = TRUE)
    PK2$endCe <- PK$endCe
    two <- simCpCe(DT, switchAt50, PK2, 480, TRUE)

    a <- one$wide; b <- two$wide
    t <- intersect(a$Time, b$Time)
    expect_equal(b$Plasma[match(t, b$Time)], a$Plasma[match(t, a$Time)],
                 tolerance = 1e-10, label = paste(drug, "plasma"))
    # Time until threshold.  The effect site in this engine comes from
    # calculateCe(), an approximation the oral engine no longer uses, so the
    # times agree to a fraction of a minute rather than exactly.
    expect_lt(max(abs(one$equiSpace$Recovery - two$equiSpace$Recovery)), 1,
              label = paste(drug, "time until threshold, minutes"))
  }
})


test_that("time until threshold counts drug still being absorbed after a change in PK", {
  # Clindamycin is timed on its plasma (no effect site).  Checked after the
  # switch, so that "the PK in force now" is also the PK the brute-force run
  # goes on using.
  PK <- switchedPK("clindamycin")
  PK$endCe <- 2
  DT <- data.frame(Drug = "clindamycin", Time = c(0, 120), Dose = c(600, 600),
                   Units = "mg PO")
  sim <- simCpCe(DT, switchAt50, PK, 480, TRUE)
  for (t in c(60, 100, 125, 150, 300)) {
    es <- sim$equiSpace
    i  <- max(which(es$Time <= t + 1e-9))
    at <- es$Time[i]
    d  <- DT[DT$Time <= at, , drop = FALSE]
    r  <- simCpCe(d, switchAt50, PK, at + 1440, FALSE)$wide
    r  <- r[r$Time >= at, ]
    above <- which(r$Plasma > PK$endCe)
    # Log-linear between the bracketing points: hours out, the engine's points
    # are far apart and the decline is exponential, so a straight line would
    # cross minutes late.
    brute <- if (length(above) == 0) 0 else {
      j <- max(above)
      r$Time[j] + log(r$Plasma[j] / PK$endCe) / log(r$Plasma[j] / r$Plasma[j + 1]) *
        (r$Time[j + 1] - r$Time[j]) - at
    }
    expect_lt(abs(es$Recovery[i] - brute), 0.5,
              label = paste("clindamycin at", round(at, 1), "min: difference in minutes"))
  }
  # Straight after the second dose the plasma is still low, but the drug in
  # the gut is going to take it over the threshold.
  es <- sim$equiSpace
  early <- es[es$Time > 121 & es$Time < 130, ]
  expect_gt(nrow(early), 0)
  expect_true(all(early$Recovery > 60))
})


test_that("an absorption lag is applied, and masked, across a change in PK", {
  PK <- switchedPK("clindamycin")
  PK$endCe <- 2
  PK$PK$Switch$tlag_PO <- 30          # the set in force when the dose is given
  DT <- data.frame(Drug = "clindamycin", Time = 60, Dose = 600, Units = "mg PO")
  sim <- simCpCe(DT, switchAt50, PK, 240, TRUE)
  w <- sim$wide
  expect_true(all(w$Plasma[w$Time < 90] == 0))
  expect_gt(w$Plasma[w$Time > 95][1], 0)
  # Given but not yet absorbing: no time, rather than zero
  expect_true(all(is.na(w$Recovery[w$Time >= 60 & w$Time < 90])))
  expect_false(anyNA(w$Recovery[w$Time >= 90]))
})


test_that("convertState carries a one-compartment AMOUNT across a change in V1", {
  old <- list(lambda_2 = 0, v1 = 10)
  new <- list(lambda_2 = 0, v1 = 25)
  expect_equal(convertState(c(5, 0, 0), old, new), c(2, 0, 0))
  # Same volume, nothing changes
  expect_equal(convertState(c(5, 0, 0), old, old), c(5, 0, 0))
})


test_that("depotInput is continuous through lambda == ka", {
  ka <- 0.05; G <- 100; dt <- 3
  exact <- depotInput(0.05, ka, G, dt)
  expect_equal(exact, ka * G * dt * exp(-ka * dt))
  expect_equal(depotInput(0.05 * (1 + 1e-7), ka, G, dt), exact, tolerance = 1e-6)
  expect_equal(depotInput(0.2, ka, 0, dt), 0)
  expect_equal(depotInput(0.2, 0, G, dt), 0)
})
