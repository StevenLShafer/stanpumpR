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
  # anchored at Woillard's cohort (2026-11-20): the old pins over its factors
  size <- pkSizeFactors(100, 175, 50, "male", TRUE, legacyVolume = 1, legacyClearance = 1)
  kc <- cohortFactorsIndependent(TACROLIMUS_COHORT)
  set <- tacrolimus(100, 175, 50, "male")$PK$default
  expect_equal(set$cl1, 21.2 / 60 * size$clearance / kc$clearance)
  expect_equal(set$v2, 271 * size$volume / kc$volume)
})

test_that("anchored at Woillard's cohort: its typical patients get Table 4, FFM scales the rest", {
  # (a) Table 1 (PDF p3): Prograf 19 men and 13 women, 55 y, 65 kg; Advagraf
  # 19 men and 22 women, 53 y, 69 kg; heights from NCD-RisC (France, born
  # 1956 and 1958).  No parameter has a sex term, so the four typical
  # patients pooled geometrically by their numbers receive Table 4
  kc <- cohortFactorsIndependent(TACROLIMUS_COHORT)
  expect_equal(round(c(kc$volume, kc$clearance), 4), c(0.8799, 0.9085))
  g <- list(c(65, 174.2, 55, 19, 1), c(65, 161.2, 55, 13, 0),
            c(69, 174.6, 53, 19, 1), c(69, 161.6, 53, 22, 0))
  pooled <- function(field) {
    s <- 0
    for (x in g) s <- s + x[4] * log(tacrolimus(x[1], x[2], x[3],
                                                if (x[5] == 1) "male" else "female")$PK$default[[field]])
    exp(s / 73)
  }
  expect_equal(pooled("v1"), 486 * 0.29)
  expect_equal(pooled("v2"), 271)
  expect_equal(pooled("cl1") * 60, 21.2)
  expect_equal(pooled("cl2") * 60, 79)
  # (b) twice the reference fat-free mass: volumes x 2 and clearances x 2^0.75
  # over the cohort's factors; the absorption is not scaled
  w <- uniroot(function(w) ffmAlSallami(w, 200, 50, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- tacrolimus(w, 200, 50, "male")$PK$default
  expect_equal(c(big$v1, big$v2), c(486 * 0.29, 271) * 2 / kc$volume)
  expect_equal(c(big$cl1, big$cl2) * 60, c(21.2, 79) * 2^0.75 / kc$clearance)
  expect_equal(big$ka_PO * 60, 3.34 * 1.53)
  # (c) the switch off: Table 4 at any size
  off <- tacrolimus(w, 200, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(off$v1, off$v2, off$cl1 * 60, off$cl2 * 60),
               c(486 * 0.29, 271, 21.2, 79))
})

test_that("the band is the label's 4-11 ng/mL whole blood trough", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "tacrolimus", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(4, 11, 7.5))
  expect_true(all(doseRoute(unlist(row$Units)) == ROUTE_PO))
})
