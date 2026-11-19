# Arterial oxygen through an endobronchial tube's shunt (R/arterialOxygen.R;
# Shafer, 2026-11-19).

test_that("the dissociation curve is Severinghaus 1979's equation 1", {
  # Its own normal points: P50 about 26.8 mmHg, 97.5% at 100 mmHg
  expect_equal(oxygenSaturation(26.86), 0.5, tolerance = 0.005)
  expect_equal(oxygenSaturation(100), 0.977, tolerance = 0.002)
  expect_equal(oxygenSaturation(0), 0)
  expect_true(all(diff(oxygenSaturation(seq(1, 600, 1))) > 0))
  # Content and its inverse agree
  p <- c(20, 40, 60, 100, 300, 600)
  expect_equal(oxygenTensionOfContent(oxygenContent(p)), p, tolerance = 1e-6)
  expect_equal(oxygenTensionOfContent(-1), 0)
})

test_that("without a shunt the arterial tension is the alveolar, to the last digit", {
  a <- arterialOxygen(c(14, 50, 93), shunt = 0, Qco = 5, VO2 = 0.25)
  expect_identical(a$PaO2, a$PAO2)
  expect_equal(a$arterial, c(14, 50, 93), tolerance = 1e-12)
})

test_that("a 50% shunt makes the patient hypoxaemic whatever the inspired oxygen", {
  # By hand: content leaving the ventilated lung at PAO2 700 mmHg is
  # 1.34 x 15 x S + 0.003 x 700; the shunt takes (0.5/0.5) x 250 / 50 = 5 mL/dL
  cc <- 1.34 * 15 * oxygenSaturation(700) + 0.003 * 700
  a <- arterialOxygen(700 / 7.6, shunt = 0.5, Qco = 5, VO2 = 0.25)
  expect_equal(oxygenContent(a$PaO2), cc - 5, tolerance = 1e-8)
  expect_lt(a$PaO2, 60)
  expect_lt(a$SaO2, 90)
  # More oxygen hardly helps: from 300 to 700 mmHg alveolar, PaO2 rises little
  b <- arterialOxygen(300 / 7.6, shunt = 0.5, Qco = 5, VO2 = 0.25)
  expect_lt(a$PaO2 - b$PaO2, 15)
})

test_that("the gas run carries it: the oxygen panel's second line and the hover", {
  DT <- data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"), Time = 0,
                   Dose = c(2, 6, 0, 6))
  tube <- simulateGases(DT, 70, 40, maximum = 60, endobronchial = TRUE)
  plain <- simulateGases(DT, 70, 40, maximum = 60)
  a <- gasArterialOxygen(tube)
  i <- which.min(abs(a$Time - 30))
  expect_equal(a$PaO2[i], 50.6, tolerance = 0.01)
  expect_equal(a$SaO2[i], 85.5, tolerance = 0.01)
  p <- gasArterialOxygen(plain)
  expect_identical(p$PaO2, p$PAO2)
  entries <- gasDrugEntries(tube, DT, maximum = 60, age = 40)
  ox <- entries$oxygen
  expect_false(is.null(ox$arterialOxygen))
  brain <- ox$results[ox$results$Site == "Effect Site", ]
  plasma <- ox$results[ox$results$Site == "Plasma", ]
  expect_lt(max(brain$Y[brain$Time > 1]), 10)
  expect_gt(min(plasma$Y[plasma$Time > 5]), 80)
})

test_that("an endobronchial physiology event steps the arterial oxygen", {
  DT <- data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"), Time = 0,
                   Dose = c(2, 6, 0, 6))
  s <- physiologyEventNormal()
  s[["endobronchial"]] <- 1
  ev <- data.frame(Time = 20)
  for (f in PHYSIOLOGY_EVENT_FIELDS) ev[[f]] <- s[[f]]
  sim <- simulateGases(DT, 70, 40, maximum = 60, physiologyEvents = ev)
  a <- gasArterialOxygen(sim)
  expect_gt(a$PaO2[which.min(abs(a$Time - 19))], 600)
  expect_lt(a$PaO2[which.min(abs(a$Time - 30))], 60)
})
