# Tests for advanceGasManBaseline(), the faithful restatement of Gas Man's own
# integration scheme.
#
# The purpose of this routine is to BE Gas Man, so these tests check that it
# reproduces Gas Man's structure and its documented behaviours -- including the
# ones the matrix-exponential engine does differently on purpose.
#
# What they cannot check is agreement with Gas Man itself.  That needs Gas Man's
# CSV export and is the whole point of the exercise.

baseDT <- function(...) {
  d <- list(...)
  data.frame(Time = vapply(d, `[[`, numeric(1), 1),
             Drug = vapply(d, `[[`, character(1), 2),
             Dose = vapply(d, `[[`, numeric(1), 3),
             stringsAsFactors = FALSE)
}

simpleDT <- function() data.frame(
  Time = c(0, 0, 0), Drug = c("oxygen", "ventilation", "sevoflurane"),
  Dose = c(8, 4, 2), stringsAsFactors = FALSE)

seriesOf <- function(b, drug, site) {
  r <- b$results[b$results$Drug == drug & b$results$Site == site, ]
  r[order(r$Time), ]
}
valueAt <- function(b, drug, site, t) {
  r <- seriesOf(b, drug, site)
  stats::approx(r$Time, r$Y, t)$y
}


test_that("the baseline carries Gas Man's compartments and agents", {
  b <- advanceGasManBaseline(simpleDT(), maximum = 10, dt = 1/60)

  expect_setequal(unique(b$results$Site),
                  c("CKT", "ALV", "VRG", "MUS", "FAT", "VEN"))

  # Oxygen is not one of Gas Man's agents, so the baseline leaves it out
  expect_false("oxygen" %in% b$results$Drug)
  expect_setequal(unique(b$results$Drug),
                  c("nitrousOxide", "sevoflurane", "isoflurane", "desflurane",
                    "nitrogen"))
})


test_that("Gas Man's volumes and blood flow ratios are used as published", {
  # [Volumes] and [Ratio] in gasman.ini
  expect_equal(unname(GASMAN_VOLUME[c("CKT","ALV","VRG","MUS","FAT","VEN")]),
               c(8, 2.5, 6, 33, 14.5, 1))
  expect_equal(unname(GASMAN_RATIO[c("VRG","MUS","FAT")]), c(0.76, 0.18, 0.06))
  expect_equal(sum(GASMAN_RATIO), 1)

  # Circuit and alveolus are gas phase, tissues carry tissue:gas, venous
  # carries blood:gas -- exactly as GasAnes.cpp sets m_fSolubility
  props <- getGasProperties()
  s <- gasManSolubility(props[props$gas == "sevoflurane", ])
  expect_equal(unname(s[["CKT"]]), 1)
  expect_equal(unname(s[["ALV"]]), 1)
  expect_equal(unname(s[["VRG"]]), 1.1)
  expect_equal(unname(s[["VEN"]]), 0.65)
})


test_that("nitrogen starts at Gas Man's ambient and washes out", {
  b <- advanceGasManBaseline(simpleDT(), maximum = 30, dt = 1/60)
  n2 <- seriesOf(b, "nitrogen", "ALV")

  expect_equal(n2$Y[1], 80)            # gasman.ini Ambient
  expect_lt(valueAt(b, "nitrogen", "ALV", 30), 2)
  expect_true(all(diff(n2$Y) <= 1e-9))
})


test_that("the circuit settles on Gas Man's algebraic target", {
  # target[CKT] = (effCKT*DEL + effALV*ALV) / (effCKT + effALV)
  FGF <- 8; VA <- 4; DEL <- 2
  b <- advanceGasManBaseline(simpleDT(), maximum = 120, dt = 1/60)

  ckt <- valueAt(b, "sevoflurane", "CKT", 120)
  alv <- valueAt(b, "sevoflurane", "ALV", 120)
  expect_equal(ckt, (FGF * DEL + VA * alv) / (FGF + VA), tolerance = 1e-3)
})


test_that("an open circuit fixes the circuit at the delivered tension", {
  b <- advanceGasManBaseline(simpleDT(), maximum = 10, dt = 1/60,
                             circuit = "open")
  ckt <- seriesOf(b, "sevoflurane", "CKT")
  expect_true(all(abs(ckt$Y[-1] - 2) < 1e-9))
})


test_that("an ideal circuit has an explicit threshold at FGF equal to VA", {
  # Gas Man's IDEAL_CKT mixes only while effCKT < effALV; at or above it the
  # circuit simply is the delivered gas.  This is the discrete threshold the
  # semi-closed differential form does not have.
  mk <- function(fgf) data.frame(
    Time = c(0, 0, 0), Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(fgf, 4, 2), stringsAsFactors = FALSE)

  above <- advanceGasManBaseline(mk(6), maximum = 10, dt = 1/60, circuit = "ideal")
  below <- advanceGasManBaseline(mk(2), maximum = 10, dt = 1/60, circuit = "ideal")

  expect_true(all(abs(seriesOf(above, "sevoflurane", "CKT")$Y[-1] - 2) < 1e-9))
  expect_lt(valueAt(below, "sevoflurane", "CKT", 5), 2)
})


test_that("disabling recirculation zeroes the venous compartment", {
  on  <- advanceGasManBaseline(simpleDT(), maximum = 20, dt = 1/60)
  off <- advanceGasManBaseline(simpleDT(), maximum = 20, dt = 1/60,
                               recirculation = FALSE)

  expect_gt(max(seriesOf(on, "sevoflurane", "VEN")$Y), 0)
  expect_true(all(seriesOf(off, "sevoflurane", "VEN")$Y == 0))

  # And without venous return the alveolus sees no recirculated agent, so it
  # equilibrates differently
  expect_false(isTRUE(all.equal(valueAt(on,  "sevoflurane", "ALV", 20),
                                valueAt(off, "sevoflurane", "ALV", 20))))
})


test_that("nitrous oxide accelerates a volatile: the second gas effect", {
  # Same vaporiser setting, same total fresh gas flow and ventilation.  The only
  # difference is that some oxygen is replaced by nitrous oxide, whose uptake
  # enters totUptake and augments sevoflurane's alveolar tension.
  withN2O <- data.frame(
    Time = c(0, 0, 0, 0),
    Drug = c("oxygen", "nitrousOxide", "ventilation", "sevoflurane"),
    Dose = c(2, 6, 4, 2), stringsAsFactors = FALSE)
  without <- data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(8, 4, 2), stringsAsFactors = FALSE)

  a <- advanceGasManBaseline(withN2O, maximum = 10, dt = 1/60)
  b <- advanceGasManBaseline(without, maximum = 10, dt = 1/60)

  # Sevoflurane reaches a higher alveolar tension in the presence of nitrous
  expect_gt(valueAt(a, "sevoflurane", "ALV", 2),
            valueAt(b, "sevoflurane", "ALV", 2))
  expect_gt(valueAt(a, "sevoflurane", "ALV", 5),
            valueAt(b, "sevoflurane", "ALV", 5))
})


test_that("the second gas effect vanishes when the uptake term is turned off", {
  withN2O <- data.frame(
    Time = c(0, 0, 0, 0),
    Drug = c("oxygen", "nitrousOxide", "ventilation", "sevoflurane"),
    Dose = c(2, 6, 4, 2), stringsAsFactors = FALSE)
  without <- data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(8, 4, 2), stringsAsFactors = FALSE)

  a <- advanceGasManBaseline(withN2O, maximum = 10, dt = 1/60, uptakeEffect = FALSE)
  b <- advanceGasManBaseline(without, maximum = 10, dt = 1/60, uptakeEffect = FALSE)

  # With m_bUptEnb off, the gases no longer influence one another at all, so
  # sevoflurane behaves identically whether nitrous oxide is running or not.
  # This is what isolates the uptake term as the mechanism.
  expect_equal(valueAt(a, "sevoflurane", "ALV", 5),
               valueAt(b, "sevoflurane", "ALV", 5), tolerance = 1e-12)
  expect_equal(valueAt(a, "sevoflurane", "ALV", 10),
               valueAt(b, "sevoflurane", "ALV", 10), tolerance = 1e-12)
})


test_that("a coarse tick is rejected and retried at finer resolution", {
  # At a one-second tick nothing needs sub-stepping.  Make the tick long enough
  # that the circuit turns over more than ten times within it and the vernier
  # has to engage.
  fine   <- advanceGasManBaseline(simpleDT(), maximum = 10, dt = 1/60)
  coarse <- advanceGasManBaseline(simpleDT(), maximum = 10, dt = 20)

  expect_equal(fine$maxVernier, 0)
  expect_gt(coarse$maxVernier, 0)
})


test_that("tensions stay physical throughout", {
  b <- advanceGasManBaseline(simpleDT(), maximum = 60, dt = 1/60)
  expect_true(all(is.finite(b$results$Y)))
  expect_true(all(b$results$Y >= -1e-9))
  # Nothing exceeds the delivered tension of the agent that produced it
  sevo <- b$results[b$results$Drug == "sevoflurane", ]
  expect_lt(max(sevo$Y), 2 + 1e-9)
})


test_that("weight scales the alveolus and tissues but not the circuit", {
  # Gas Man's ComputeTerms divides the ALVEOLAR and TISSUE rate constants by
  # fWtFactor = weight/70, equivalent to multiplying those effective volumes.
  # The circuit is machine, not patient, and is left alone.  Uptake carries the
  # same factor.  This engine previously applied the factor to uptake only,
  # which was internally inconsistent off 70 kg.
  at <- function(w, site, t) {
    b <- advanceGasManBaseline(simpleDT(), weight = w, maximum = 30, dt = 0.1)
    valueAt(b, "sevoflurane", site, t)
  }

  # A heavier patient has a larger alveolus and larger tissues, so the alveolar
  # tension rises more slowly.
  expect_gt(at(70, "ALV", 5), at(100, "ALV", 5))
  expect_gt(at(100, "ALV", 5), at(140, "ALV", 5))
  expect_gt(at(70, "VRG", 10), at(140, "VRG", 10))

  # The circuit is unscaled, so it is barely moved -- only through the
  # alveolar term feeding back into it.
  ckt70  <- at(70,  "CKT", 30)
  ckt140 <- at(140, "CKT", 30)
  expect_lt(abs(ckt70 - ckt140) / ckt70, 0.05)

  # And 70 kg is exactly the reference: the factor is one.
  expect_equal(GASMAN_STD_WEIGHT, 70)
})
