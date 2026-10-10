# Regional anesthesia (RA): a local anesthetic injected into tissue, absorbed
# first-order from a depot into the systemic circulation.  (Claude Code,
# 2026-10-10, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# The 70 kg, 170 cm, 35 year reference man, whose size factors are exactly 1.
raPK <- function(drug, weight = 70) {
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK(drug, weight, 170, 35, "male", dd[dd$Drug == drug, ])
  PK$endCe <- dd$endCe[dd$Drug == drug]
  PK
}

# Plasma concentration (mg/L) after `dose` mg into a first-order depot, from
# the compartment rate matrix of the PK set, by eigen-decomposition: shares no
# code with getDrugPK()'s coefficients or the engines.
raReference <- function(s, dose, t) {
  K <- matrix(0, 3, 3)
  K[1, ] <- c(-(s$k10 + s$k12 + s$k13), s$k21, s$k31)
  K[2, ] <- c(s$k12, -s$k21, 0)
  K[3, ] <- c(s$k13, 0, -s$k31)
  keep <- c(TRUE, s$k12 > 0, s$k13 > 0)
  K <- K[keep, keep, drop = FALSE]
  e <- eigen(K)
  lam <- -Re(e$values)
  coef <- Re(e$vectors)[1, ] * solve(Re(e$vectors))[, 1] / s$v1
  depot <- function(ka, F) {
    if (is.null(ka) || ka <= 0) return(0)
    a <- coef * ka / (ka - lam)
    dose * F * (colSums(a * exp(-outer(lam, t))) - sum(a) * exp(-ka * t))
  }
  # A drug with a slow second depot splits the dose between the two
  # (getDrugPK() carries each depot's share in its bioavailability).
  depot(s$ka_RA, s$bioavailability_RA) + depot(s$ka_RAslow, s$bioavailability_RAslow)
}

test_that("RA units are read as the RA route and as amounts, not rates", {
  expect_true(all(doseRoute(raUnits) == ROUTE_RA))
  expect_false(any(isRateUnit(raUnits)))
  expect_true(all(raUnits %in% allUnits))
  expect_false(any(raUnits %in% scheduledUnits))
  expect_equal(groupUnitsByRoute(c("mg RA", "mg PO", "mg")), c("mg", "mg PO", "mg RA"))
})

test_that("an RA dose is absorbed first-order, matching an independent solution", {
  for (drug in c("lidocaine", "bupivacaine", "ropivacaine", "mepivacaine")) {
    PK <- raPK(drug)
    s <- PK$PK$default
    expect_gt(s$ka_RA, 0)
    DT <- data.frame(Drug = drug, Time = 0, Dose = 300, Units = "mg RA")
    w <- simCpCe(DT, noEvents, PK, 600, FALSE)$wide
    ref <- raReference(s, 300, w$Time)
    keep <- ref > max(ref) * 1e-6
    expect_lt(max(abs(w$Plasma[keep] / ref[keep] - 1)), 1e-8, label = drug)
    expect_equal(w$Plasma[1], 0)
  }
})

test_that("the area under an RA dose is F x dose / CL", {
  for (drug in c("bupivacaine", "mepivacaine")) {
    s <- raPK(drug)$PK$default
    area <- 0
    for (r in c("RA", "RAslow")) {
      coefs <- unlist(s[paste0("p_coef_", r, c("_l1", "_l2", "_l3", "_ka"))])
      rates <- c(s$lambda_1, s$lambda_2, s$lambda_3, s[[paste0("ka_", r)]])
      used <- rates > 0
      area <- area + sum(coefs[used] / rates[used])
    }
    # The two depots' shares add up to the whole bioavailability, 1
    expect_equal(s$bioavailability_RA + s$bioavailability_RAslow, 1, label = drug)
    expect_equal(area, 1 / s$cl1, tolerance = 1e-10, label = drug)
  }
  # Racemic mepivacaine: AUC = dose x (0.5 / CLR + 0.5 / CLS) at F = 1
  s <- raPK("mepivacaine")$PK$default
  expect_equal(1 / s$cl1, 0.5 / 0.79 + 0.5 / 0.35, tolerance = 1e-10)
})

test_that("an RA dose and an intravenous bolus superpose", {
  PK <- raPK("lidocaine")
  both <- data.frame(Drug = "lidocaine", Time = c(0, 30), Dose = c(400, 100),
                     Units = c("mg RA", "mg"))
  w <- simCpCe(both, noEvents, PK, 300, FALSE)$wide
  ra <- simCpCe(both[1, ], noEvents, PK, 300, FALSE)$wide
  iv <- simCpCe(both[2, ], noEvents, PK, 300, FALSE)$wide
  at <- c(10, 29, 31, 60, 120, 240)
  f <- function(x) stats::approx(x$Time, x$Plasma, at)$y
  expect_equal(f(w), f(ra) + f(iv), tolerance = 1e-3)
})

test_that("an RA dose is absorbed, not infused, when the PK set changes", {
  # The same PK set on both sides of an event: advanceClosedForm1() must give
  # what the single-set engine gives.
  PK <- raPK("bupivacaine")
  DT <- data.frame(Drug = "bupivacaine", Time = c(0, 60), Dose = c(150, 100),
                   Units = "mg RA")
  single <- simCpCe(DT, noEvents, PK, 300, FALSE)$wide
  PK$PK$Switch <- PK$PK$default
  PK$pkEvents <- c(PK$pkEvents, "Switch")
  switched <- simCpCe(DT, data.frame(Time = 50, Event = "Switch"), PK, 300, FALSE)$wide
  # Compared at the points both engines computed, so nothing is interpolated.
  at <- intersect(single$Time, switched$Time)
  expect_gt(length(at), 20)
  f <- function(x) x$Plasma[match(at, x$Time)]
  expect_equal(f(switched), f(single), tolerance = 1e-8)
})

test_that("the local anesthetics are listed together, plasma only", {
  dd <- getDrugDefaultsGlobal()
  la <- dd[dd$Category %in% "Local anesthetics", ]
  expect_setequal(la$Drug, c("lidocaine", "bupivacaine", "ropivacaine", "mepivacaine"))
  for (drug in setdiff(la$Drug, "lidocaine")) {
    expect_equal(raPK(drug)$PK$default$ke0, 0, label = drug)
    row <- dd[dd$Drug == drug, ]
    expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0), label = drug)
  }
})

test_that("a slow second RA depot is absorbed across a change in PK set, and with a lag", {
  PK <- raPK("mepivacaine")
  DT <- data.frame(Drug = "mepivacaine", Time = c(0, 60), Dose = c(300, 200),
                   Units = "mg RA")
  single <- simCpCe(DT, noEvents, PK, 400, FALSE)$wide
  PK2 <- PK
  PK2$PK$Switch <- PK2$PK$default
  PK2$pkEvents <- c(PK2$pkEvents, "Switch")
  switched <- simCpCe(DT, data.frame(Time = 50, Event = "Switch"), PK2, 400, FALSE)$wide
  at <- intersect(single$Time, switched$Time)
  expect_gt(length(at), 20)
  f <- function(x) x$Plasma[match(at, x$Time)]
  expect_equal(f(switched), f(single), tolerance = 1e-8)

  # Both depots follow the drug's lag
  PK3 <- PK
  PK3$PK$default$tlag_RA <- PK3$PK$default$tlag_RAslow <- 10
  lagged <- simCpCe(DT[1, ], noEvents, PK3, 400, FALSE)$wide
  plain  <- simCpCe(DT[1, ], noEvents, PK, 400, FALSE)$wide
  expect_true(all(lagged$Plasma[lagged$Time <= 10] == 0))
  g <- function(x, t) stats::approx(x$Time, x$Plasma, t)$y
  expect_equal(g(lagged, c(40, 130, 300)), g(plain, c(30, 120, 290)), tolerance = 1e-3)
})

test_that("a drug without a slow depot has none, and a bad one is refused", {
  s <- raPK("bupivacaine")$PK$default
  expect_equal(s$ka_RAslow, 0)
  expect_equal(s$bioavailability_RAslow, 0)
  original <- bupivacaine
  bad <- function(weight, height, age, sex, adjustToFFM = TRUE) {
    x <- original(weight, height, age, sex, adjustToFFM)
    x$PK$default$ka_RA_slow <- 0.005
    x$PK$default$fraction_RA_slow <- 1.5
    x
  }
  local_mocked_bindings(bupivacaine = bad)
  dd <- getDrugDefaultsGlobal()
  expect_error(getDrugPK("bupivacaine", 70, 170, 35, "male", dd[dd$Drug == "bupivacaine", ]),
               "slow RA depot")
})
