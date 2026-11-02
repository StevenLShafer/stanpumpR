# opicapone: see R/drugs_opicapone.R (FDA Clinical Pharmacology Review of NDA
# 212489, Table 9 and the equations on pp. 55-56).

test_that("Table 9 for the micronised capsule at bedtime, without levodopa", {
  m <- opicapone(71, 175, 35, "male", FALSE)
  p <- m$PK$default
  expect_equal(p$cl1 * 60, 37.6)
  expect_equal(p$v1, 49.7)
  expect_equal(p$ka_PO * 60, 1.24 * (1 + 0.245) * (1 - 0.595))
  expect_equal(m$transitChain, list(n = 2L, ktr = p$ka_PO, kMetabolic = 0))
  expect_equal(opicapone(71, 175, 35, "female", FALSE)$PK$default$cl1 * 60, 37.6 * (1 - 0.138))
})

test_that("F1 is Table 9's at both marketed strengths", {
  expect_equal(opicaponeF1(50), 1.681)
  expect_equal(opicaponeF1(25), 0.5^-0.288 * 1.609)
  sat <- opicapone(71, 175, 35, "male", FALSE)$oralSaturation
  f <- function(d) oralSaturationFraction(d, sat)
  expect_equal(f(50), 1)
  expect_equal(f(25), opicaponeF1(25) / opicaponeF1(50))
  # through the engine: exposure per mg at 25 mg against 50 mg
  auc <- function(d) pdAUC(pdSimulate(pdRegimen("opicapone", "mg PO", d, 1440, 1),
                                      maximum = 1440, weight = 71, height = 175, age = 35), 0, 1440)
  expect_equal((auc(25) / 25) / (auc(50) / 50), opicaponeF1(25) / opicaponeF1(50), tolerance = 1e-4)
})

test_that("CL/F for 50 mg is near LeWitt 2023's 24.7 L/h", {
  auc <- pdAUC(pdSimulate(pdRegimen("opicapone", "mg PO", 50, 1440, 1), maximum = 1440,
                          weight = 71, height = 175, age = 35), 0, 1440)
  expect_equal(50 / (auc / 1000), 37.6 / 1.681, tolerance = 0.01)
})
