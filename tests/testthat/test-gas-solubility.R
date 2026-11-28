# Blood:gas coefficients, haematocrit and the tissues' temperature slopes
# (the third piece of the gas engine expansion, docs/gas-engine-expansion.md).
# Steven L. Shafer: "Baseline blood:gas - Use esper" (2026-11-28);
# "Hematocrit changes solubility.  Use Zhou's 2002 coefficients." and "Yes,
# use tissue specific slopes relative to temperature" (2026-11-27).

solubilityTable <- function(drug = "sevoflurane", dose = 2)
  data.frame(Drug = c(drug, "oxygen", "ventilation"), Time = 0, Dose = c(dose, 6, 6),
             stringsAsFactors = FALSE)

solubilityAt <- function(sim, site, t, drug = "sevoflurane")
{
  r <- sim$results[sim$results$Drug == drug & sim$results$Site == site, ]
  stats::approx(r$Time, r$Y, t, ties = "ordered")$y
}

test_that("the volatile agents' blood:gas coefficients are Esper 2015's", {
  expect_equal(GAS_BLOOD_GAS, c(sevoflurane = 0.74, isoflurane = 1.45, desflurane = 0.57))
  p <- getGasProperties()
  expect_equal(p$lambda_blood[match(names(GAS_BLOOD_GAS), p$gas)], unname(GAS_BLOOD_GAS))
  # Nitrous oxide, nitrogen and every tissue:gas coefficient are GasMan's still
  gm <- getGasProperties(gasMans = TRUE)
  expect_equal(p$lambda_blood[p$gas == "nitrousOxide"], 0.47)
  expect_identical(p[c("tg_brain", "tg_muscle", "tg_fat")], gm[c("tg_brain", "tg_muscle", "tg_fat")])
  # So tissue:blood fell: isoflurane's brain:blood from 1.62 to 1.45
  iso <- p$gas == "isoflurane"
  expect_equal(round(gm$tg_brain[iso] / gm$lambda_blood[iso], 2), 1.62)
  expect_equal(round(p$tg_brain[iso] / p$lambda_blood[iso], 2), 1.45)
})

test_that("a body in GasMan's mode runs on GasMan's coefficients", {
  body <- getGasBody(70)
  expect_identical(gasPropertiesOf(body), getGasProperties())
  expect_identical(gasPropertiesOf(NULL), getGasProperties())
  body$volumesAsStated <- TRUE
  expect_identical(gasPropertiesOf(body), getGasProperties(gasMans = TRUE))
  # Through the solver, with nothing else changed (Devin's review of PR 1028:
  # comparing GasMan's mode with the default changed the shunt and the
  # volumes too).  With the table that ignores gasMans, so that GasMan's mode
  # is handed Esper's coefficients, a run in GasMan's mode changes, on the
  # ideal circuit and the semi-closed; a default run does not.
  run <- function(...) {
    sim <- simulateGases(solubilityTable(), weight = 70, age = 40, maximum = 20, ...)
    solubilityAt(sim, "Alveolar", 20)
  }
  asGasMan <- run(volumeConversion = FALSE)
  semi <- run(circuit = "semi-closed")
  usual <- run()
  esperOnly <- getGasProperties
  testthat::local_mocked_bindings(
    getGasProperties = function(gasMans = FALSE) esperOnly(gasMans = FALSE))
  # More soluble blood carries more from the lungs: end-tidal lower
  expect_lt(run(volumeConversion = FALSE), asGasMan - 0.01)
  expect_lt(run(circuit = "semi-closed"), semi - 0.01)
  expect_identical(run(), usual)
})

test_that("Esper's coefficients slow end-tidal and the brain (the help page's numbers)", {
  sim <- simulateGases(solubilityTable(), weight = 70, age = 40, maximum = 30)
  # (1.726 and 1.684 until the fresh gas added its inspiratory share to the
  # delivered ventilation, 2026-11-28)
  expect_equal(round(solubilityAt(sim, "Alveolar", 30), 3), 1.775)
  expect_equal(round(solubilityAt(sim, "Brain", 30), 3), 1.732)
  # More soluble blood carries more from the lungs: with the blood:gas
  # coefficient alone raised, end-tidal is lower at every time
  props <- getGasProperties()
  expect_gt(props$lambda_blood[props$gas == "sevoflurane"],
            GASMAN_BLOOD_GAS[["sevoflurane"]])
})

test_that("haematocrit moves the blood:gas coefficient by Zhou 2002's slopes", {
  expect_equal(gasHaematocrit(NULL), 45)
  expect_equal(gasHaematocrit(15), 45)
  expect_equal(gasHaematocrit(7), 21)
  p <- getGasProperties()
  expect_identical(gasPropertiesAt(NULL, 15), p)
  low <- gasPropertiesAt(NULL, 7)
  for (g in names(GAS_HAEMATOCRIT_SOLUBILITY_SLOPE)) {
    i <- p$gas == g
    expect_equal(low$lambda_blood[i],
                 p$lambda_blood[i] * exp(GAS_HAEMATOCRIT_SOLUBILITY_SLOPE[[g]] * (21 - 45)))
  }
  # Zhou's own regression for isoflurane at 37 C: ln(lambda) = -0.0382 T +
  # 0.0154 HCT + 1.120, so the ratio between two haematocrits
  zhou <- function(hct) exp(-0.0382 * 37 + 0.0154 * hct + 1.120)
  iso <- p$gas == "isoflurane"
  expect_equal(gasPropertiesAt(NULL, 20 / 3)$lambda_blood[iso] /
                 gasPropertiesAt(NULL, 40 / 3)$lambda_blood[iso], zhou(20) / zhou(40))
  # The help page: sevoflurane 0.59 at 7 g/dL
  expect_equal(round(low$lambda_blood[p$gas == "sevoflurane"], 2), 0.59)
  # Nothing else moves
  rest <- c("tg_brain", "tg_muscle", "tg_fat", "MAC40")
  expect_identical(low[rest], p[rest])
  same <- p$gas %in% c("nitrousOxide", "nitrogen")
  expect_identical(low$lambda_blood[same], p$lambda_blood[same])
})

test_that("the Haemoglobin slider reaches the agents through the body", {
  body <- simulateGases(solubilityTable(), maximum = 5, haemoglobin = 7)$body
  expect_equal(body$haemoglobin, 7)
  expect_equal(gasPropertiesOf(body), gasPropertiesAt(NULL, 7))
  usual <- simulateGases(solubilityTable(), weight = 70, age = 40, maximum = 30)
  anaemic <- simulateGases(solubilityTable(), weight = 70, age = 40, maximum = 30,
                           haemoglobin = 7)
  # Diluted blood dissolves less, so less is carried from the lungs
  expect_gt(solubilityAt(anaemic, "Alveolar", 30), solubilityAt(usual, "Alveolar", 30))
  # (1.771 and 1.725 at the ventilation as set, until 2026-11-28)
  expect_equal(round(solubilityAt(anaemic, "Alveolar", 30), 3), 1.815)
  expect_equal(round(solubilityAt(anaemic, "Brain", 30), 3), 1.769)
  # The time until threshold reads the same table
  d <- solubilityTable()
  w <- gasWashout(anaemic, d)
  expect_true(is.finite(utils::tail(gasRecoveryTime(w, "sevoflurane", 0.18), 1)))
  # A physiology event that changes the haemoglobin changes the coefficient
  events <- physiologyEventsFrom(eventTableRows(
    Time = 10, Event = PHYSIOLOGY_EVENT_NAME,
    Settings = physiologyEventEncode(c(haemoglobin = 7))))
  stepped <- simulateGases(d, weight = 70, age = 40, maximum = 30, physiologyEvents = events)
  lambda <- vapply(stepped$bodySteps$body, function(b) {
    p <- gasPropertiesOf(b); p$lambda_blood[p$gas == "sevoflurane"] }, 0)
  expect_equal(lambda, c(0.74, 0.74 * exp(0.0092 * (21 - 45))))
  expect_gt(solubilityAt(stepped, "Alveolar", 30), solubilityAt(usual, "Alveolar", 30))
  # ... and by 30 minutes reads close to a run anaemic throughout (a little
  # above it: the tissues took up more in the first ten minutes)
  expect_equal(solubilityAt(stepped, "Alveolar", 30), solubilityAt(anaemic, "Alveolar", 30),
               tolerance = 0.005)
})

test_that("each tissue takes its own temperature slope (Zhou and Liu 2001, Table 1)", {
  expect_equal(GAS_TISSUE_TEMPERATURE_SLOPE$tg_brain,
               c(desflurane = 0.0322, sevoflurane = 0.0307, isoflurane = 0.0303))
  expect_equal(GAS_TISSUE_TEMPERATURE_SLOPE$tg_muscle,
               c(desflurane = 0.0393, sevoflurane = 0.0382, isoflurane = 0.0405))
  expect_equal(GAS_TISSUE_TEMPERATURE_SLOPE$tg_fat,
               c(desflurane = 0.0325, sevoflurane = 0.0378, isoflurane = 0.0434))
  p37 <- getGasProperties(); p31 <- gasPropertiesAt(31)
  # The help page: at 31 C isoflurane's brain:blood is 5% lower, and
  # sevoflurane's fat:blood 5% higher
  ratio <- function(g, col) {
    i <- p37$gas == g
    (p31[[col]][i] / p31$lambda_blood[i]) / (p37[[col]][i] / p37$lambda_blood[i])
  }
  expect_equal(round(ratio("isoflurane", "tg_brain"), 2), 0.95)
  expect_equal(round(ratio("sevoflurane", "tg_fat"), 2), 1.05)
  # Nitrous oxide keeps Allott 1973's coefficients, nitrogen nothing
  n2o <- p37$gas == "nitrousOxide"
  expect_equal(p31$tg_brain[n2o] / p37$tg_brain[n2o],
               exp(GAS_N2O_SOLUBILITY_TEMPERATURE_SLOPE[["aqueous"]] * 6))
  expect_equal(p31$tg_fat[n2o] / p37$tg_fat[n2o],
               exp(GAS_N2O_SOLUBILITY_TEMPERATURE_SLOPE[["fat"]] * 6))
  n2 <- p37$gas == "nitrogen"
  expect_identical(p31[n2, ], p37[n2, ])
  # Temperature and haematocrit multiply
  both <- gasPropertiesAt(31, 7); cold <- p31; thin <- gasPropertiesAt(NULL, 7)
  s <- p37$gas == "sevoflurane"
  expect_equal(both$lambda_blood[s] / p37$lambda_blood[s],
               (cold$lambda_blood[s] / p37$lambda_blood[s]) * (thin$lambda_blood[s] / p37$lambda_blood[s]))
})

test_that("a tissue's agent is conserved across a step in temperature with its own slope", {
  d <- solubilityTable()
  events <- physiologyEventsFrom(eventTableRows(
    Time = 15, Event = PHYSIOLOGY_EVENT_NAME,
    Settings = physiologyEventEncode(c(temperature = 31))))
  sim <- simulateGases(d, weight = 70, age = 40, maximum = 30, physiologyEvents = events)
  at <- which(abs(sim$timeLine - 15) < 1e-9)
  expect_length(at, 2)
  before <- gasPropertiesOf(sim$bodySteps$body[[1]]); after <- gasPropertiesOf(sim$bodySteps$body[[2]])
  s <- before$gas == "sevoflurane"
  y <- sim$state$sevoflurane
  # Tension x tissue:gas x the vapour in a litre at each temperature
  amount <- function(row, props, body, col, state)
    y[row, state] * props[[col]][s] * gasMachineLitres(body)
  for (k in list(c("tg_brain", 3), c("tg_muscle", 4), c("tg_fat", 5)))
    expect_equal(amount(at[2], after, sim$bodySteps$body[[2]], k[[1]], as.integer(k[[2]])),
                 amount(at[1], before, sim$bodySteps$body[[1]], k[[1]], as.integer(k[[2]])),
                 tolerance = 1e-10)
})
