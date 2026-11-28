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
  a <- arterialOxygen(700 / (GAS_ALVEOLAR_TORR / 100), shunt = 0.5, Qco = 5, VO2 = 0.25)
  expect_equal(oxygenContent(a$PaO2), cc - 5, tolerance = 1e-8)
  expect_lt(a$PaO2, 60)
  expect_lt(a$SaO2, 90)
  # More oxygen hardly helps: from 300 to 700 mmHg alveolar, PaO2 rises little
  b <- arterialOxygen(300 / (GAS_ALVEOLAR_TORR / 100), shunt = 0.5, Qco = 5, VO2 = 0.25)
  expect_lt(a$PaO2 - b$PaO2, 15)
})

test_that("the gas run carries it: the oxygen panel's second line and the hover", {
  DT <- data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"), Time = 0,
                   Dose = c(2, 6, 0, 6))
  tube <- simulateGases(DT, 70, 40, maximum = 60, endobronchial = TRUE)
  plain <- simulateGases(DT, 70, 40, maximum = 60)
  a <- gasArterialOxygen(tube)
  i <- which.min(abs(a$Time - 30))
  # (50.6 and 85.5 until 2026-11-24, when the alveolar tension was read at
  # 760 Torr per 100% rather than 713, Gemini's C-06)
  expect_equal(a$PaO2[i], 49.8, tolerance = 0.01)
  expect_equal(a$SaO2[i], 84.8, tolerance = 0.01)
  # The alveolar oxygen on oxygen stays within the dry share of the pressure
  # (it read 722.7 mmHg with no tube, above the 713 Torr there are)
  expect_lt(max(gasArterialOxygen(plain)$PAO2), GAS_ALVEOLAR_TORR)
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

test_that("the haemoglobin moves the arterial saturation through a shunt, and not without one", {
  lo <- arterialOxygen(700 / (GAS_ALVEOLAR_TORR / 100), shunt = 0.5, Qco = 5, VO2 = 0.245, hb = 7)
  hi <- arterialOxygen(700 / (GAS_ALVEOLAR_TORR / 100), shunt = 0.5, Qco = 5, VO2 = 0.245, hb = 17)
  expect_equal(lo$SaO2, 69.0, tolerance = 0.002)
  expect_equal(hi$SaO2, 86.9, tolerance = 0.002)
  none <- arterialOxygen(700 / (GAS_ALVEOLAR_TORR / 100), shunt = 0, Qco = 5, VO2 = 0.245, hb = 7)
  expect_identical(none$PaO2, none$PAO2)
  # The oxygen the ventilated lung loads is the body's use, whatever the haemoglobin
  for (hb in c(7, 15)) {
    a <- arterialOxygen(113 / (GAS_ALVEOLAR_TORR / 100), shunt = 0.5, Qco = 5, VO2 = 0.245, hb = hb)
    cc <- oxygenContent(a$PAO2, hb)
    cv <- oxygenContent(a$PaO2, hb) - 1000 * 0.245 / 50
    expect_equal(0.5 * 50 * (cc - cv) / 1000, 0.245, tolerance = 1e-9)
  }
})

test_that("the gas run carries the haemoglobin, and a physiology event records it", {
  DT <- data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"), Time = 0,
                   Dose = c(2, 6, 0, 6))
  sim <- simulateGases(DT, 70, 40, maximum = 60, endobronchial = TRUE, haemoglobin = 7)
  a <- gasArterialOxygen(sim)
  expect_lt(a$SaO2[which.min(abs(a$Time - 30))], 71)
  expect_error(simulateGases(DT, 70, 40, maximum = 60, haemoglobin = 2), "haemoglobin")
  s <- physiologyEventNormal()
  s[["endobronchial"]] <- 1
  s[["haemoglobin"]] <- 8
  expect_match(physiologyEventEncode(s), "haemoglobin=8")
  expect_equal(physiologyEventDecode(physiologyEventEncode(s))[["haemoglobin"]], 8)
  expect_null(physiologyEventDecode("haemoglobin=30"))
  expect_match(physiologyEventSummary(s), "haemoglobin 8 g/dL")
  ev <- data.frame(Time = 20)
  for (f in PHYSIOLOGY_EVENT_FIELDS) ev[[f]] <- s[[f]]
  stepped <- simulateGases(DT, 70, 40, maximum = 60, physiologyEvents = ev)
  b <- gasArterialOxygen(stepped)
  expect_lt(b$SaO2[which.min(abs(b$Time - 30))], 75)
  expect_gt(b$SaO2[which.min(abs(b$Time - 10))], 99)
})

test_that("altitude deepens the hypoxaemia: the arterial oxygen reads the engine's partial pressure", {
  DT <- data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"), Time = 0,
                   Dose = c(2, 0, 6, 6))
  sat <- function(alt) {
    a <- gasArterialOxygen(simulateGases(DT, 70, 40, maximum = 60, endobronchial = TRUE,
                                         altitude = alt))
    a$SaO2[which.min(abs(a$Time - 30))]
  }
  s <- vapply(c(0, 3000, 5000), sat, 0)
  # (75.2, 68.0 and 47.0 at 760 Torr per 100%, until 2026-11-24; 74.7, 66.5
  # and 43.2 until 2026-11-28, when the oxygen consumed, an STPD volume, was
  # converted to the lungs' conditions: the alveolar oxygen had read about
  # 9 mmHg high at sea level, and more at altitude, gasStpdFactor())
  expect_equal(s, c(74.1, 62.1, 26.4), tolerance = 0.002)
})

test_that("the Visualizer carries the arterial saturation only where a tube shunts", {
  DT <- data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"), Time = 0,
                   Dose = c(2, 6, 0, 6))
  tube <- gasDrugEntries(simulateGases(DT, 70, 40, maximum = 60, endobronchial = TRUE),
                         DT, maximum = 60, age = 40)
  run <- viewerGasRun(tube$sevoflurane, 0, 60)
  expect_length(run$saturation, length(run$t))
  expect_equal(run$saturation[length(run$t)], 85.5, tolerance = 0.01)
  expect_length(run$paO2, length(run$t))
  plain <- gasDrugEntries(simulateGases(DT, 70, 40, maximum = 60), DT, maximum = 60, age = 40)
  expect_null(viewerGasRun(plain$sevoflurane, 0, 60)$saturation)
})
