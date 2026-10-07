# Renal function estimated from the four covariates the app collects, at an
# assumed normal serum creatinine.  Expected values were worked out by hand
# from the published formulas, not from the code under test.

test_that("Cockcroft-Gault reproduces the textbook formula", {
  # (140 - 35) x 70 / (72 x 1.0)
  expect_equal(creatinineClearanceCG(70, 35, "male"), 102.0833333, tolerance = 1e-7)
  # Woman: assumed creatinine 0.8 and the 0.85 factor
  expect_equal(creatinineClearanceCG(60, 50, "female"), (90 * 60 / (72 * 0.8)) * 0.85)
  # An explicit creatinine overrides the assumption
  expect_equal(creatinineClearanceCG(70, 35, "male", scr = 2), 102.0833333 / 2, tolerance = 1e-7)
  # Falls with age
  expect_lt(creatinineClearanceCG(70, 80, "male"), creatinineClearanceCG(70, 35, "male"))
})

test_that("the assumed creatinine depends on sex only", {
  expect_equal(assumedCreatinine("male"), 1.0)
  expect_equal(assumedCreatinine("female"), 0.8)
})

test_that("Du Bois body surface area", {
  expect_equal(bsaDuBois(70, 170), 1.809708, tolerance = 1e-6)
})

test_that("CKD-EPI 2009 without the race term", {
  # Male, 35 y, creatinine 1.0: 141 x (1/0.9)^-1.209 x 0.993^35
  expect_equal(egfrCKDEPI2009(35, "male"), 97.07826, tolerance = 1e-6)
  # Female, 50 y, creatinine 0.8: ratio 0.8/0.7 > 1, so the -1.209 branch,
  # times 1.018
  expect_equal(egfrCKDEPI2009(50, "female"),
               141 * (0.8 / 0.7)^(-1.209) * 0.993^50 * 1.018, tolerance = 1e-9)
  # Below kappa the alpha branch applies
  expect_equal(egfrCKDEPI2009(35, "male", scr = 0.6),
               141 * (0.6 / 0.9)^(-0.411) * 0.993^35, tolerance = 1e-9)
  # De-indexing multiplies by BSA / 1.73
  expect_equal(egfrDeindexed(70, 170, 35, "male"), 97.07826 * 1.809708 / 1.73, tolerance = 1e-5)
})
