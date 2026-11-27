# The MAC panel's effect-site line is the summed brain MAC (gasBrainMac();
# Shafer, 2026-11-25, Gemini's gas-engine audit of 2026-11-23, C-04 and D-05)

macRows <- function(age = 40, ...) {
  dt <- data.frame(Drug = c("oxygen", "nitrousOxide", "sevoflurane", "ventilation", "sevoflurane"),
                   Time = c(0, 0, 0, 0, 30), Dose = c(2, 4, 2, 6, 0))
  list(dt = dt, sim = simulateGases(dt, weight = 70, age = age, maximum = 60, ...))
}

test_that("the brain MAC sums each potent agent's brain tension over its MAC", {
  r <- macRows()
  b <- gasBrainMac(r$sim)
  expect_equal(b$Time, r$sim$timeLine)
  pk <- gasPropertiesAt(37)
  mac <- function(g) macForAge(pk$MAC40[pk$gas == g], 40)
  expect_equal(b$Y, r$sim$state$sevoflurane[, 3] / mac("sevoflurane") +
                    r$sim$state$nitrousOxide[, 3] / mac("nitrousOxide"))
  # The end-tidal series is the same sum over the alveolar column
  et <- r$sim$results$Y[r$sim$results$Drug == "MAC"]
  expect_equal(et, gasMacSum(r$sim$state, 2L, r$sim$bodyRow, list(r$sim$body), 40))
  # The brain lags: below end-tidal on the way in, above it on the way out
  i5 <- which.min(abs(b$Time - 5))
  i35 <- which.min(abs(b$Time - 35))
  expect_lt(b$Y[i5], et[i5])
  expect_gt(b$Y[i35], et[i35])
})

test_that("the MAC panel draws the brain MAC as its effect site, the threshold on end-tidal", {
  r <- macRows()
  entries <- gasDrugEntries(r$sim, r$dt, getDrugDefaultsGlobal(), 60, age = 40)
  m <- entries[["MAC"]]$results
  b <- gasBrainMac(r$sim)
  expect_equal(m$Y[m$Site == "Effect Site"], b$Y)
  expect_equal(m$Y[m$Site == "Plasma"], r$sim$results$Y[r$sim$results$Drug == "MAC"])
  expect_false(isTRUE(all.equal(m$Y[m$Site == "Effect Site"], m$Y[m$Site == "Plasma"])))
})

test_that("a temperature event steps the brain MAC as it steps end-tidal", {
  ev <- data.frame(Time = 20, t(physiologyEventDecode("temperature=32")))
  r <- macRows(physiologyEvents = ev)
  b <- gasBrainMac(r$sim)
  pk32 <- gasPropertiesAt(32)
  rows <- which(r$sim$bodyRow == max(r$sim$bodyRow))
  mac32 <- function(g) macForAge(pk32$MAC40[pk32$gas == g], 40)
  expect_equal(b$Y[rows], r$sim$state$sevoflurane[rows, 3] / mac32("sevoflurane") +
                          r$sim$state$nitrousOxide[rows, 3] / mac32("nitrousOxide"))
})

test_that("the opioid interaction divides both lines by the same reduction", {
  dt <- data.frame(Drug = c("oxygen", "sevoflurane", "ventilation"), Time = 0, Dose = c(6, 2, 6))
  sim <- simulateGases(dt, weight = 70, age = 40, maximum = 60)
  entries <- gasDrugEntries(sim, dt, getDrugDefaultsGlobal(), 60, age = 40)
  before <- entries[["MAC"]]$results
  opioid <- list(remifentanil = list(
    results = data.frame(Drug = "remifentanil", Time = c(0, 60), Site = "Effect Site", Y = c(2, 2)),
    MEAC = 1))
  after <- applyOpioidMacInteraction(entries, opioid)[["MAC"]]$results
  ratio <- function(site) {
    a <- after[after$Site == site, ]
    b <- before[before$Site == site, ]
    stats::approx(a$Time, a$Y, 30)$y / stats::approx(b$Time, b$Y, 30)$y
  }
  expect_equal(ratio("Effect Site"), ratio("Plasma"), tolerance = 1e-6)
  expect_gt(ratio("Plasma"), 1)
})

test_that("an opioid time between gas samples scales each line's own interpolation", {
  # CodeRabbit on #939: times 0 and 60 are gas points already, so the branch
  # that inserts an opioid's own time, reading brain and end-tidal each off its
  # own curve, was not exercised
  dt <- data.frame(Drug = c("oxygen", "sevoflurane", "ventilation"), Time = 0, Dose = c(6, 2, 6))
  sim <- simulateGases(dt, weight = 70, age = 40, maximum = 60)
  entries <- gasDrugEntries(sim, dt, getDrugDefaultsGlobal(), 60, age = 40)
  before <- entries[["MAC"]]$results
  t <- 3.4567
  expect_false(t %in% before$Time)
  opioid <- list(remifentanil = list(
    results = data.frame(Drug = "remifentanil", Time = c(0, t, 60), Site = "Effect Site",
                         Y = c(0.5, 3, 1)),
    MEAC = 1))
  after <- applyOpioidMacInteraction(entries, opioid)[["MAC"]]$results
  R <- opioidMacReduction(totalOpioidMEAC(opioid, t)$U)
  at <- function(d, site) {
    s <- d[d$Site == site, ]
    seriesStepAt(s$Time, s$Y, t)
  }
  expect_true(t %in% after$Time[after$Site == "Effect Site"])
  expect_true(t %in% after$Time[after$Site == "Plasma"])
  expect_equal(at(after, "Effect Site"), at(before, "Effect Site") / (1 - R))
  expect_equal(at(after, "Plasma"), at(before, "Plasma") / (1 - R))
  # The two lines differ there, so a brain read off the end-tidal curve fails
  expect_false(isTRUE(all.equal(at(before, "Effect Site"), at(before, "Plasma"))))
})
