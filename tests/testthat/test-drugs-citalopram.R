# citalopram: see the header of R/drugs_citalopram.R.
#
# Akil et al. 2016: R-citalopram CL/F 13 L/h (male) or 9.05 (female)
# x (age/60)^-0.822, V/F 1830 L; S-citalopram CL/F 22.1 L/h (CYP2C19 EM/RM)
# or 16.3 (IM/PM) x (age/60)^-1.33 x (weight/70)^0.75, V/F 1390 L; common ka
# 1 /h; each racemic dose half R, half S.  The pins below were worked out by
# hand from those numbers through the reduction written out in
# reduceByHand(), which shares no code with the package, and the engine's
# oral curve is compared with the sum of the two enantiomers' own
# one-compartment closed forms.
#
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Two-compartment model whose central impulse response is
# 0.5/vR exp(-kR t) + 0.5/vS exp(-kS t).  Clearances in, and out, in L/h.
reduceByHand <- function(clR, vR, clS, vS) {
  A <- 0.5 / vR; B <- 0.5 / vS; l1 <- clR / vR; l2 <- clS / vS
  v1 <- 1 / (A + B)
  k21 <- (A * l2 + B * l1) / (A + B)
  k10 <- l1 * l2 / k21
  k12 <- l1 + l2 - k21 - k10
  list(v1 = v1, v2 = k12 * v1 / k21, cl1 = k10 * v1, cl2 = k12 * v1)
}

# Total citalopram, ng/mL, after an oral dose D mg at time 0 (t in min)
enantiomerSum <- function(t, D, clR, vR, clS, vS, ka = 1 / 60) {
  one <- function(cl, v) {
    k <- cl / 60 / v
    (D / 2) * ka / (v * (ka - k)) * (exp(-k * t) - exp(-ka * t)) * 1000
  }
  one(clR, vR) + one(clS, vS)
}

test_that("returns the reduced published parameters at the reference patient", {
  # Male, 60 years, 70 kg, switch off: CL_R 13, CL_S 22.1 L/h.
  # V1 = 2 / (1/1830 + 1/1390) = 1579.937888 L; total CL = harmonic mean of
  # 13 and 22.1 = 16.370370 L/h.
  actual <- citalopram(70, 171, 60, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 1579.93788820, v2 = 252.352921129, v3 = 1,
      cl1 = 0.272839506173, cl2 = 0.0458467263542, cl3 = 0,
      ka_PO = 1 / 60,
      bioavailability_PO = 1,
      tlag_PO = 0
    )),
    tPeak = 0,
    MEAC = 0,
    typical = 80,
    upperTypical = 110,
    lowerTypical = 50,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  expect_equal_rounded(actual$PK$default$cl1, 16.3703703704 / 60)
  r <- reduceByHand(13, 1830, 22.1, 1390)
  expect_equal_rounded(actual$PK$default$v2, r$v2)
  expect_equal_rounded(actual$PK$default$cl2, r$cl2 / 60)
  expect_match(actual$reference, "Akil", fixed = TRUE)
  expect_match(actual$reference, "10.1007/s10928-015-9457-6", fixed = TRUE)
})

test_that("sex changes R-citalopram clearance only", {
  # Female, 60 y, 70 kg: CL_R 9.05 L/h
  actual <- citalopram(70, 160, 60, "female", adjustToFFM = FALSE)$PK$default
  expected <- list(v1 = 1579.93788820, v2 = 496.969067994,
                   cl1 = 0.214023542001, cl2 = 0.0801272591511)
  expect_equal_rounded(actual[names(expected)], expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126;
  # the S weight term is evaluated at the pharmacokinetic weight
  # 70 x 1.3049067 = 91.3435 kg, so (91.3435/70)^0.75 = 1.2209126 too.
  # CL_R = 13 x (50/60)^-0.822 x 1.2209126, CL_S = 22.1 x (50/60)^-1.33 x
  # 1.2209126, V_R = 1830 x 1.3049067, V_S = 1390 x 1.3049067.
  actual <- citalopram(120, 170, 50, "male")$PK$default
  expected <- list(v1 = 2061.67153589, v2 = 404.075504392,
                   cl1 = 0.400079066677, cl2 = 0.0846665135481)
  expect_equal_rounded(actual[names(expected)], expected)
})

test_that("the switch off evaluates the S weight term at total body weight", {
  # 120 kg, 50 y male, switch off: volumes and CL_R as published,
  # CL_S x (120/70)^0.75
  actual <- citalopram(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  r <- reduceByHand(13 * (50 / 60)^-0.822, 1830,
                    22.1 * (50 / 60)^-1.33 * (120 / 70)^0.75, 1390)
  expect_equal_rounded(actual[c("v1", "v2")], r[c("v1", "v2")])
  expect_equal_rounded(actual$cl1, r$cl1 / 60)
  expect_equal_rounded(actual$cl2, r$cl2 / 60)
})

test_that("CYP2C19: normal, rapid and ultrarapid 22.1 L/h; intermediate and poor 16.3", {
  pk <- function(ph) citalopram(70, 171, 60, "male", cyp2c19 = ph, adjustToFFM = FALSE)$PK$default
  em <- list(v1 = 1579.93788820, v2 = 252.352921129, cl1 = 0.272839506173, cl2 = 0.0458467263542)
  pm <- list(v1 = 1579.93788820, v2 = 100.041377722, cl1 = 0.241069397042, cl2 = 0.0151719066628)
  for (ph in c("normal", "rapid", "ultrarapid")) expect_equal_rounded(pk(ph)[names(em)], em)
  for (ph in c("intermediate", "poor")) expect_equal_rounded(pk(ph)[names(pm)], pm)
  # Total clearance, IM/PM: 1 / (0.5/13 + 0.5/16.3) = 14.4641 L/h
  expect_equal_rounded(pk("poor")$cl1 * 60, 1 / (0.5 / 13 + 0.5 / 16.3))
  expect_equal(citalopram(70, 171, 60, "male"), citalopram(70, 171, 60, "male", cyp2c19 = "normal"))
})

test_that("an invalid cyp2c19 fails loudly", {
  expect_error(citalopram(70, 171, 60, "male", cyp2c19 = "extensive"), "Invalid cyp2c19")
  expect_error(citalopram(70, 171, 60, "male", cyp2c19 = c("poor", "normal")), "Invalid cyp2c19")
  expect_error(citalopram(70, 171, 60, "male", cyp2c19 = NA), "Invalid cyp2c19")
})

test_that("citalopram is offered orally only, because the parameters are apparent", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "citalopram", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_false(any(grepl("min|hr", units)))
  X <- citalopram(70, 171, 60, "male")
  expect_equal(X$PK$default$bioavailability_PO, 1)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC),
               c(X$lowerTypical, X$upperTypical, X$typical, X$MEAC))
  expect_equal(X$tPeak, 0)
})

test_that("the reduced model's oral curve is exactly the sum of the two enantiomers", {
  cases <- list(
    list(wt = 70, ht = 171, age = 60, sex = "male",   ph = "normal", ffm = FALSE,
         clR = 13, vR = 1830, clS = 22.1, vS = 1390),
    list(wt = 70, ht = 160, age = 80, sex = "female", ph = "poor",   ffm = FALSE,
         clR = 9.05 * (80 / 60)^-0.822, vR = 1830,
         clS = 16.3 * (80 / 60)^-1.33, vS = 1390)
  )
  for (cs in cases) {
    o <- simulateDrugsWithCovariates(
      data.frame(Drug = "citalopram", Time = 0, Dose = 20, Units = "mg PO"),
      noEvents, cs$wt, cs$ht, cs$age, cs$sex, 7 * 1440, FALSE,
      adjustToFFM = cs$ffm, cyp2c19 = cs$ph)
    w <- o$citalopram$wide
    ref <- enantiomerSum(w$Time, 20, cs$clR, cs$vR, cs$clS, cs$vS)
    expect_lt(max(abs(w$Plasma - ref)) / max(ref), 1e-9)
    expect_gt(length(w$Time), 50)
  }
})

test_that("the degenerate case of equal elimination rates is one compartment", {
  # kR = kS when CL_S / 1390 = CL_R / 1830.  For a 60 y man, switch off, that
  # needs 22.1 x (W/70)^0.75 = 13 x 1390 / 1830 = 9.874, i.e. W = 70 x
  # (9.874/22.1)^(4/3).
  W <- 70 * (13 * 1390 / 1830 / 22.1)^(4 / 3)
  p <- citalopram(W, 171, 60, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(p$cl2, 0)
  expect_equal(p$v2, 1)
  expect_equal_rounded(p$v1, 1579.93788820)
  # One compartment with k = 13/1830 /h: CL = 1579.94 x 13/1830 L/h
  expect_equal_rounded(p$cl1, 1579.93788820 * 13 / 1830 / 60)
  # Nearby: a small separation still reduces, positively and continuously
  for (f in c(0.999, 0.99999, 1.00001, 1.001)) {
    q <- citalopram(W * f, 171, 60, "male", adjustToFFM = FALSE)$PK$default
    expect_true(all(is.finite(unlist(q))) && q$v2 > 0 && q$cl2 >= 0, info = f)
    expect_equal(q$cl1, p$cl1, tolerance = 2e-3, info = f)
  }
  # and the engine's curve is still the enantiomer sum near coincidence
  for (f in c(0.99999, 0.999)) {
    o <- simulateDrugsWithCovariates(
      data.frame(Drug = "citalopram", Time = 0, Dose = 20, Units = "mg PO"),
      noEvents, W * f, 171, 60, "male", 7 * 1440, FALSE, adjustToFFM = FALSE)
    w <- o$citalopram$wide
    ref <- enantiomerSum(w$Time, 20, 13, 1830, 22.1 * (W * f / 70)^0.75, 1390)
    expect_lt(max(abs(w$Plasma - ref)) / max(ref), 1e-6, label = paste("f =", f))
  }
})

test_that("pediatric profiles give finite, positive parameters", {
  for (age in c(0.01, 0.5, 5, 12)) for (ffm in c(TRUE, FALSE)) for (ph in CYP2C19_VALUES) {
    p <- citalopram(3.5 + age * 3, 50 + age * 8, age, "female", cyp2c19 = ph, adjustToFFM = ffm)$PK$default
    expect_true(all(is.finite(unlist(p))), info = paste(age, ffm, ph))
    expect_true(p$v1 > 0 && p$v2 > 0 && p$cl1 > 0 && p$cl2 >= 0, info = paste(age, ffm, ph))
  }
})

test_that("half-lives and steady state agree with the published parameters", {
  p <- citalopram(70, 171, 60, "male", adjustToFFM = FALSE)$PK$default
  # The reduced model's eigenvalues are the enantiomers' own rate constants:
  # R ln2 x 1830/13 = 97.57 h, S ln2 x 1390/22.1 = 43.60 h
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  s <- k10 + k12 + k21; d <- sqrt(s^2 - 4 * k10 * k21)
  halfLives <- log(2) / (c(s - d, s + d) / 2) / 60
  expect_equal(halfLives, c(97.5737954, 43.5961349), tolerance = 1e-7)

  # 20 mg once daily: Css,avg = 10/24/13 + 10/24/22.1 mg/L = 50.905 ng/mL
  css <- (10 / 24 / 13 + 10 / 24 / 22.1) * 1000
  expect_equal(css, 50.904977, tolerance = 1e-7)
  expect_equal(20 / 24 / (p$cl1 * 60) * 1000, css, tolerance = 1e-9)

  # The engine after 8 weeks (13.8 R half-lives) of 20 mg qd, at a trough:
  # the superposed enantiomer troughs.
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "citalopram", Time = 0, Dose = 20, Units = "mg PO qd"),
    noEvents, 70, 171, 60, "male", 56 * 1440, FALSE, adjustToFFM = FALSE)
  w <- o$citalopram$wide
  expect_equal(utils::tail(w$Time, 1), 56 * 1440)
  tau <- 1440; ka <- 1 / 60
  trough <- function(cl, v) {
    k <- cl / 60 / v
    10 * ka / (v * (ka - k)) * (exp(-k * tau) / (1 - exp(-k * tau)) - exp(-ka * tau) / (1 - exp(-ka * tau))) * 1000
  }
  expect_equal(utils::tail(w$Plasma, 1), trough(13, 1830) + trough(22.1, 1390), tolerance = 1e-4)
  expect_lt(utils::tail(w$Plasma, 1), css)
  expect_gt(max(w$Plasma[w$Time > 55 * 1440]), css)
})
