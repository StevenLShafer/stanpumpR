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
