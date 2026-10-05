# Opioid reduction of MAC.  These tests check that the code does what the
# equations in R/opioidMacInteraction.R say.  They do NOT establish that the
# parameters are right: this is an approximate model, and the parameters are
# expected to be replaced.  See the header of that file.

test_that("opioidMacReduction follows the sigmoid and its limits", {
  expect_equal(opioidMacReduction(0), 0)
  # At U50 the reduction is half the ceiling.
  expect_equal(opioidMacReduction(OPIOID_MAC_U50), OPIOID_MAC_EMAX / 2)
  # Monotone, and never reaches the ceiling: opioids cannot replace the agent.
  U <- c(0, 0.5, 1, 2, 5, 10, 100, 1e6)
  R <- opioidMacReduction(U)
  expect_true(all(diff(R) > 0))
  expect_lt(max(R), OPIOID_MAC_EMAX)
  expect_equal(opioidMacReduction(1e9), OPIOID_MAC_EMAX, tolerance = 1e-9)
  # The explicit form, with parameters given and at the defaults.
  expect_equal(opioidMacReduction(3, Emax = 0.68, U50 = 2.2, gamma = 1.75),
               0.68 * 3^1.75 / (2.2^1.75 + 3^1.75))
  expect_equal(opioidMacReduction(3),
               OPIOID_MAC_EMAX * 3^OPIOID_MAC_GAMMA /
                 (OPIOID_MAC_U50^OPIOID_MAC_GAMMA + 3^OPIOID_MAC_GAMMA))
  # The defaults put a 50% reduction near 2.2 x MEAC, where the fentanyl and
  # sufentanil studies have it.
  expect_equal(opioidMacReduction(2.2), 0.5, tolerance = 0.01)
  # Bad input is treated as no opioid.
  expect_equal(opioidMacReduction(c(-1, NA)), c(0, 0))
})


test_that("totalOpioidMEAC adds the opioids and ignores everything else", {
  Time <- seq(0, 10, by = 1)
  drugs <- list(
    fentanyl     = list(equiSpace = data.frame(Time = Time, Ce = 1, MEAC = 100)),  # 1.0 x MEAC
    remifentanil = list(equiSpace = data.frame(Time = Time, Ce = 2, MEAC = 50)),   # 0.5 x MEAC
    propofol     = list(equiSpace = data.frame(Time = Time, Ce = 3, MEAC = 0)),
    notSimulated = list(Color = "red")
  )
  out <- totalOpioidMEAC(drugs)
  expect_equal(out$Time, Time)
  expect_equal(out$U, rep(1.5, length(Time)))

  expect_null(totalOpioidMEAC(drugs["propofol"]))
  expect_null(totalOpioidMEAC(list()))
  expect_null(totalOpioidMEAC(NULL))
})


test_that("the interaction raises the MAC series by exactly 1 / (1 - R), and only that series", {
  gasDose <- data.frame(
    Time = 0, Drug = c("oxygen", "ventilation", "sevoflurane"), Dose = c(4, 4, 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 30)
  entries <- gasDrugEntries(sim, gasDose, maximum = 30)

  # A steady opioid at twice its MEAC.
  Time <- entries$MAC$equiSpace$Time
  opioid <- list(fentanyl = list(equiSpace = data.frame(Time = Time, Ce = 1.2, MEAC = 200)))
  out <- applyOpioidMacInteraction(entries, opioid)

  scale <- 1 / (1 - opioidMacReduction(2))
  before <- entries$MAC$results; after <- out$MAC$results
  for (site in c("Plasma", "Effect Site"))
    expect_equal(after$Y[after$Site == site], before$Y[before$Site == site] * scale)
  expect_equal(out$MAC$equiSpace$Ce, entries$MAC$equiSpace$Ce * scale)
  expect_equal(out$MAC$max$Ce, entries$MAC$max$Ce * scale)

  # The gases themselves do not move, and the entry keeps its shape.
  expect_identical(out$sevoflurane, entries$sevoflurane)
  expect_identical(out$oxygen, entries$oxygen)
  expect_setequal(names(out$MAC), names(entries$MAC))
  expect_true(isTRUE(out$MAC$isGas))
})


test_that("the interaction follows the opioid in time, and is a no-op without one", {
  gasDose <- data.frame(
    Time = 0, Drug = c("oxygen", "ventilation", "sevoflurane"), Dose = c(4, 4, 2))
  sim <- simulateGases(gasDose, weight = 70, age = 40, maximum = 30)
  entries <- gasDrugEntries(sim, gasDose, maximum = 30)
  Time <- entries$MAC$equiSpace$Time

  # Opioid present only in the second half.
  late <- list(remifentanil = list(equiSpace = data.frame(
    Time = Time, Ce = 0, MEAC = ifelse(Time >= 15, 300, 0))))
  out <- applyOpioidMacInteraction(entries, late)
  ratio <- out$MAC$equiSpace$Ce / entries$MAC$equiSpace$Ce
  expect_equal(ratio[Time > 0 & Time < 14], rep(1, sum(Time > 0 & Time < 14)))
  expect_equal(ratio[Time > 16], rep(1 / (1 - opioidMacReduction(3)), sum(Time > 16)))

  # No opioid, no MAC entry, or no intravenous drugs at all: nothing changes.
  expect_identical(applyOpioidMacInteraction(entries, list()), entries)
  expect_identical(applyOpioidMacInteraction(entries, NULL), entries)
  noMac <- entries[setdiff(names(entries), "MAC")]
  expect_identical(applyOpioidMacInteraction(noMac, late), noMac)
})
