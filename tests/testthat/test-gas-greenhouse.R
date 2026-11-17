# The greenhouse effect of the inhaled anesthetics (R/gasGreenhouse.R;
# Shafer, 2026-11-17). The values are Table 1 of Sulbaek Andersen 2012
# (Anesth Analg 2012;114:1081-5), read from the rendered page.

test_that("the GWPs and lifetimes are Table 1's", {
  t1 <- GAS_GREENHOUSE
  rownames(t1) <- t1$gas
  expect_equal(t1[c("nitrousOxide", "isoflurane", "desflurane", "sevoflurane"), "gwp100"], c(298, 510, 2540, 130))
  expect_equal(t1[c("nitrousOxide", "isoflurane", "desflurane", "sevoflurane"), "gwp20"], c(289, 1800, 6810, 440))
  expect_equal(t1[c("nitrousOxide", "isoflurane", "desflurane", "sevoflurane"), "gwp500"], c(153, 160, 130, 40))
  expect_equal(t1[c("nitrousOxide", "isoflurane", "desflurane", "sevoflurane"), "lifetime"], c(114, 3.2, 14, 1.1))
  # Every potent inhaled agent in the library has a row.
  expect_setequal(intersect(potentAgents(), t1$gas), potentAgents())
})

test_that("the molecular weights follow from the formulas", {
  expect_equal(gasMolecularWeight("nitrousOxide"), 44.013, tolerance = 1e-6)
  expect_equal(gasMolecularWeight("isoflurane"), 184.488, tolerance = 1e-6)
  expect_equal(gasMolecularWeight("desflurane"), 168.036, tolerance = 1e-6)
  expect_equal(gasMolecularWeight("sevoflurane"), 200.053, tolerance = 1e-6)
  expect_true(is.na(gasMolecularWeight("oxygen")))
  expect_null(gasGreenhouse("oxygen"))
  # An ideal gas at 20 C: about 196 mL of isoflurane vapour weighs 1.5 g, a
  # millilitre of the liquid.
  expect_equal(GAS_VAPOUR_MOLAR_VOLUME, 24.055, tolerance = 1e-4)
  expect_equal(gasGreenhouse("isoflurane")$kgPerLitre * 1000, 7.669, tolerance = 1e-4)
})

test_that("CO2 equivalent: kg of agent times its 100-year GWP", {
  # 2% sevoflurane in 6 L/min for an hour is 7.2 L of vapour, 59.9 g, 7.78 kg CO2e.
  mL <- 10 * 6 * 2 * 60
  expect_equal(gasCO2Equivalent("sevoflurane", mL), 7.2 * 200.053 / 24.05513 / 1000 * 130, tolerance = 1e-6)
  expect_equal(gasCO2Equivalent("desflurane", 1000), 168.036 / 24.05513 / 1000 * 2540, tolerance = 1e-6)
  expect_true(is.na(gasCO2Equivalent("oxygen", 1000)))
})

test_that("the help's table is built from the same numbers", {
  md <- helpGasGreenhouseTableMarkdown()
  expect_match(md, "| Desflurane | 14 | 2540 | 6.99 | 6% in 2 L/min | 127.7 |", fixed = TRUE)
  expect_match(md, "| Sevoflurane | 1.1 | 130 | 8.32 | 2% in 2 L/min | 2.6 |", fixed = TRUE)
  expect_match(md, "| Nitrous oxide | 114 | 298 | 1.83 | 60% in 2 L/min | 39.3 |", fixed = TRUE)
})
