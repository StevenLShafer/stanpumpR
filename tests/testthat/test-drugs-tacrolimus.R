# Tacrolimus, Prograf (R/drugs_tacrolimus.R): Woillard 2011's two compartments
# after an Erlang absorption of three compartments, whole blood.
# Drafted by Claude Code, 2026-11-12.  Expectations from Table 4 of the source
# (PMC3045548), not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table 4's Prograf parameters, with the switch off", {
  pk <- tacrolimus(70, 170, 35, "male", adjustToFFM = FALSE)
  set <- pk$PK$default
  expect_equal(set$cl1, 21.2 / 60)
  expect_equal(set$v1, 486 * 0.29)
  expect_equal(set$cl2, 79 / 60)
  expect_equal(set$v2, 271)
  ktr <- 3.34 * 1.53 / 60
  expect_equal(set$ka_PO, ktr)
  expect_equal(pk$transitChain, list(n = 3L, ktr = ktr, kMetabolic = 0))
  expect_equal(set$bioavailability_PO, 1)
})

test_that("the Erlang chain gives the gamma input of shape 3", {
  # A dose into three compartments in series at Ktr appears in the central
  # compartment at the rate D ktr^3 t^2 exp(-ktr t) / 2; the amount absorbed by
  # time t is the gamma(3) distribution function
  dose <- data.frame(Drug = "tacrolimus", Time = 0, Dose = 5, Units = "mg PO")
  res <- simulateDrugsWithCovariates(dose, events, 70, 170, 35, "male", 600,
                                     FALSE, adjustToFFM = FALSE)$tacrolimus
  wide <- res$wide
  # peak in whole blood within the label's range of a few hours
  tPeak <- wide$Time[which.max(wide$Plasma)]
  expect_true(tPeak > 30 && tPeak < 180)
  # the model's own Tmax for a gamma(3) input into a slow system is near
  # 2 / ktr = 23.5 min of input mode, so the blood peak lags it
  expect_gt(tPeak, 2 / (3.34 * 1.53 / 60))
})

test_that("pregnancy multiplies whole blood CL/F by Zheng 2012's 47.4 / 34.2", {
  on  <- tacrolimus(70, 165, 30, "female", pregnant = TRUE)$PK$default
  off <- tacrolimus(70, 165, 30, "female")$PK$default
  expect_equal(on$cl1 / off$cl1, 47.4 / 34.2)
  for (p in c("v1", "v2", "cl2", "ka_PO")) expect_equal(on[[p]], off[[p]])
})

test_that("with the switch on, the library's size factors scale it", {
  size <- pkSizeFactors(100, 175, 50, "male", TRUE, legacyVolume = 1, legacyClearance = 1)
  set <- tacrolimus(100, 175, 50, "male")$PK$default
  expect_equal(set$cl1, 21.2 / 60 * size$clearance)
  expect_equal(set$v2, 271 * size$volume)
})

test_that("the band is the label's 4-11 ng/mL whole blood trough", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "tacrolimus", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(4, 11, 7.5))
  expect_true(all(doseRoute(unlist(row$Units)) == ROUTE_PO))
})
