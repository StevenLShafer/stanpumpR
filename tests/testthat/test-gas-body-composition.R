# The gas compartments scaled by body composition (getGasBody(); Shafer,
# 2026-11-25, Gemini's gas-engine audit of 2026-11-23, B-01 and F-01: "Ted
# Eger would have approved")

fields <- c("V_alveolar", "V_brain", "V_muscle", "V_fat", "Q_cardiac",
            "f_brain", "f_muscle", "f_fat", "VO2", "V_circuit")

test_that("the reference man is GasMan's standard exactly", {
  gasman <- getGasBody(70)
  composed <- getGasBody(70, height = 170, age = 35, sex = "male")
  for (k in fields) expect_equal(composed[[k]], gasman[[k]], info = k)
  expect_equal(unname(composed$composition), c(1, 1))
})

test_that("without height, age or sex, or with the switch off, the scaling is GasMan's", {
  for (w in c(5, 100, 150)) {
    gasman <- getGasBody(w)
    expect_identical(getGasBody(w, height = 170, age = 35, sex = "male", adjustToFFM = FALSE), gasman)
    expect_identical(getGasBody(w, height = NULL, age = 35, sex = "male"), gasman)
    expect_identical(getGasBody(w, height = 170, age = NULL, sex = "male"), gasman)
    expect_identical(getGasBody(w, height = 170, age = 35, sex = NULL), gasman)
  }
  # And simulateGases() without them runs as before
  dt <- data.frame(Drug = c("oxygen", "sevoflurane", "ventilation"), Time = 0, Dose = c(6, 2, 6))
  expect_identical(simulateGases(dt, weight = 100, maximum = 30)$state,
                   simulateGases(dt, weight = 100, maximum = 30, height = 170, sex = "male",
                                 adjustToFFM = FALSE)$state)
})

test_that("lean volumes and the lungs follow fat-free mass, fat follows fat mass", {
  for (p in list(list(100, 170, 35, "male"), list(150, 170, 35, "male"),
                 list(60, 165, 40, "female"), list(20, 115, 6, "female"),
                 list(3.5, 50, 0, "male"))) {
    w <- p[[1]]
    b <- getGasBody(w, height = p[[2]], age = p[[3]], sex = p[[4]])
    ffm <- ffmAlSallami(w, p[[2]], p[[3]], p[[4]])
    lean <- ffm / FFM_REFERENCE
    fat <- (w - ffm) / (70 - FFM_REFERENCE)
    expect_equal(b$V_brain, 6 * lean)
    expect_equal(b$V_muscle, 33 * lean)
    expect_equal(b$V_alveolar, 2.5 * lean)
    expect_equal(b$V_fat, 14.5 * fat)
    # Flows: lean groups allometric in FFM, fat in its volume (Weber 2021,
    # Equation 1), cardiac output their sum
    expect_equal(b$f_brain * b$Q_cardiac, 5 * 0.76 * lean^0.75)
    expect_equal(b$f_muscle * b$Q_cardiac, 5 * 0.18 * lean^0.75)
    expect_equal(b$f_fat * b$Q_cardiac, 0.3 * (b$V_fat / 14.5)^0.75)
    expect_equal(b$f_brain + b$f_muscle + b$f_fat, 1)
    # Oxygen consumption stays per kilogram of weight
    expect_equal(b$VO2, 0.0035 * w)
  }
})

test_that("an obese patient has more fat and less lean tissue than linear scaling gives", {
  b <- getGasBody(150, height = 170, age = 35, sex = "male")
  g <- getGasBody(150)
  expect_gt(b$V_fat, 2 * g$V_fat)
  expect_lt(b$V_muscle, 0.7 * g$V_muscle)
  expect_lt(b$Q_cardiac, g$Q_cardiac)
  # Weber 2021's fat flow: fat added to the standard man at its Table 2's
  # 100 kg volume, 44.87 L, perfused at 0.3 (44.87/14.5)^0.75 = 0.70 L/min
  # (its table prints 0.77, its shares rounded to whole per cent)
  expect_equal(0.3 * (44.87 / 14.5)^0.75, 0.70, tolerance = 0.01)
})

test_that("the simulation, its physiology events and its washout run on the composed body", {
  dt <- data.frame(Drug = c("oxygen", "sevoflurane", "ventilation", "sevoflurane"),
                   Time = c(0, 0, 0, 60), Dose = c(6, 2, 6, 0))
  composed <- simulateGases(dt, weight = 150, age = 35, maximum = 120, height = 170, sex = "male")
  linear <- simulateGases(dt, weight = 150, age = 35, maximum = 120)
  expect_equal(composed$body$V_fat, getGasBody(150, height = 170, age = 35, sex = "male")$V_fat)
  expect_false(isTRUE(all.equal(composed$state$sevoflurane, linear$state$sevoflurane)))
  ev <- data.frame(Time = 30, t(physiologyEventDecode("cardiac=0.5")))
  stepped <- simulateGases(dt, weight = 150, age = 35, maximum = 120, height = 170, sex = "male",
                           physiologyEvents = ev)
  expect_equal(stepped$bodySteps$body[[1]]$V_fat, composed$body$V_fat)
  w <- gasWashout(composed, dt[, c("Time", "Drug", "Dose")])
  expect_true(all(is.finite(gasRecoveryTime(w, "sevoflurane", 0.18))))
})
