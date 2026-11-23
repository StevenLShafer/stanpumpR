# Bosentan (R/drugs_bosentan.R): Miura 2026's one-compartment model, Table 3
# and its footnote.  Drafted by Claude Code, 2026-11-18.  Expectations are
# computed here from the paper, not from the drug file.

clMiura <- function(w) 8.85 * (w / 42.2)^0.472   # L/h, ALT at its median

test_that("Miura 2026, Table 3, switch off", {
  set <- bosentan(42.2, 160, 40, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1, 8.85 / 60)
  expect_equal(set$v1, 41.0)
  expect_equal(set$ka_PO, 2.47 / 60)
  expect_equal(set$tlag_PO, 0.379 * 60)
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$cl2, 0)
  child <- bosentan(12, 90, 4, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(child$cl1, clMiura(12) / 60)
  expect_equal(child$v1, 41.0)     # V/F has no covariate
})

test_that("switch on: weight at the pharmacokinetic weight", {
  pkW <- 70 * 1.3049067   # 120 kg, 170 cm, 50 y man, as the other tests pin it
  set <- bosentan(120, 170, 50, "male")$PK$default
  expect_equal_rounded(set$cl1, clMiura(pkW) / 60)
})

test_that("the source's median steady-state concentrations (Figure 5)", {
  # Css = daily dose / CL/F; adults 1221 ng/mL on 250 mg/day (median 48.5 kg),
  # children 339 ng/mL on 4 mg/kg/day (median 12 kg)
  css <- function(w, mgPerDay)
    mgPerDay * 1000 / 24 / (bosentan(w, 160, 40, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60)
  expect_equal(round(css(48.5, 250)), 1102)
  expect_equal(round(css(12, 48)), 409)
})

test_that("the simulated steady state at 125 mg twice daily matches dose / CL", {
  dt <- data.frame(Drug = "bosentan", Time = 0, Dose = 125, Units = "mg PO bid")
  events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 40, "male", 10 * 1440, FALSE,
                                   adjustToFFM = FALSE)$bosentan$wide
  last <- w[w$Time >= 9 * 1440 & w$Time <= 9.5 * 1440, ]
  avg <- mean(approx(last$Time, last$Plasma, seq(9 * 1440, 9.5 * 1440, by = 1))$y)
  expect_equal(avg, 250 * 1000 / 24 / clMiura(70), tolerance = 0.01)
})

test_that("no band, no effect site", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "bosentan", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$MEAC, row$endCe), c(0, 0, 0, 0, 0))
  expect_identical(therapeuticRange("bosentan")$band_status, "none_established")
  expect_equal(bosentan(70, 170, 40, "male")$tPeak, 0)
})

# ---------------------------------------------------------------------------
# V/F scaled by fat-free mass, anchored at each drug's cohort (Shafer's
# first-principles rule of 2026-11-19; the anchor, two groups, his choice of
# 2026-11-20).  The cohorts are Table 1's medians, written out here: the
# children's heights the JSPE 2000 reference's 50th percentile (Appendix
# Table 2), the adults' NCD-RisC Japan less the loss with age, and each
# group at the cohort's sex ratio (Table 1 does not split it).
miuraCohort <- function(wc, ac, hb, hg, wa, aa, hm, hw, M, F, nc, na) {
  N <- nc + na
  list(groups = list(
    list(weight = wc, height = hb, age = ac, men = M * nc / N, women = 0),
    list(weight = wc, height = hg, age = ac, men = 0, women = F * nc / N),
    list(weight = wa, height = hm, age = aa, men = M * na / N, women = 0),
    list(weight = wa, height = hw, age = aa, men = 0, women = F * na / N)))
}
ffmMiura <- function(w, h, a, male) {
  bmi <- w / (h / 100)^2
  if (male) (0.88 + 0.12 / (1 + (a / 13.4)^(-12.7))) * 9270 * w / (6680 + 216 * bmi)
  else (1.11 - 0.11 / (1 + (a / 7.1)^(-1.1))) * 9270 * w / (8780 + 244 * bmi)
}
FFM_REF_MIURA <- ffmMiura(70, 170, 35, TRUE)
# a man of twice the reference fat-free mass: 200 cm, 35 years (the
# maturation term at 35, 1 - 6e-7, is the reference man's too)
W2_MIURA <- local({
  mat35 <- 0.88 + 0.12 / (1 + (35 / 13.4)^(-12.7))
  2 * FFM_REF_MIURA * 6680 / (mat35 * 9270 - 2 * FFM_REF_MIURA * 216 / 4)
})

# (a) each of the cohort's four typical subjects gets V x its own FFM factor
# over the pooled divisor (V/F has no sex term), and their geometric mean,
# weighted by the counts, is the published V; (b) twice the reference FFM
# doubles V/F against the reference man, and CL/F keeps the fitted weight
# term at the pharmacokinetic weight, 140 kg; (c) the switch off is V for
# everyone
checkMiuraVolume <- function(f, V, cohort, clFitted) {
  pooled <- cohortFactorsIndependent(cohort)$volume
  logSum <- 0; n <- 0
  for (g in cohort$groups) {
    male <- g$men > 0
    v <- f(g$weight, g$height, g$age, if (male) "male" else "female")$PK$default$v1
    expect_equal(v, V * ffmMiura(g$weight, g$height, g$age, male) / FFM_REF_MIURA / pooled)
    k <- g$men + g$women
    logSum <- logSum + k * log(v); n <- n + k
  }
  expect_equal(exp(logSum / n), V)
  ref <- f(70, 170, 35, "male")$PK$default
  expect_equal(ref$v1, V / pooled)
  big <- f(W2_MIURA, 200, 35, "male")$PK$default
  expect_equal(big$v1 / ref$v1, 2)
  expect_equal(big$cl1 * 60, clFitted(140))
  expect_equal(f(W2_MIURA, 200, 35, "male", adjustToFFM = FALSE)$PK$default$v1, V)
  expect_equal(f(10, 80, 1.5, "female", adjustToFFM = FALSE)$PK$default$v1, V)
  pooled
}

test_that("switch on: V/F by fat-free mass, anchored at the cohort", {
  # 7 children (4.8 y, 12 kg), 9 adults (47 y, 48.5 kg), 5 M / 11 F
  p <- checkMiuraVolume(bosentan, 41.0,
                        miuraCohort(12, 4.8, 105.6, 105.4, 48.5, 47, 170.5, 157.4, 5, 11, 7, 9),
                        clMiura)
  expect_equal(p, 0.3889, tolerance = 1e-3)
})
