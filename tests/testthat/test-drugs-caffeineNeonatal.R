# Caffeine in preterm neonates (R/drugs_caffeineNeonatal.R): Lee 2002's
# one-compartment model, linear in weight, dosed as caffeine citrate.
# Drafted by Claude Code, 2026-11-12.  Expectations from the source's printed
# equations (PMC1874379), not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("clearance and volume are Lee 2002's lines in weight", {
  for (w in c(0.7, 1.1, 1.8)) {
    set <- caffeineNeonatal(w, 38, 5 / 365.25, "male", adjustToFFM = FALSE)$PK$default
    expect_equal(set$cl1, (0.004248 * w + 0.00154) / 60)
    expect_equal(set$v1, 0.6299 * w + 0.259)
    expect_equal(c(set$v2, set$v3, set$cl2, set$cl3), c(1, 1, 0, 0))
  }
  # 1.1 kg: half-life 106 h, the study's mean
  set <- caffeineNeonatal(1.1, 38, 5 / 365.25, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) * set$v1 / set$cl1 / 60, 106, tolerance = 0.01)
})

test_that("caffeine citrate is half caffeine base", {
  expect_true(all(caffeineCitrateUnits %in% allUnits))
  expect_true(all(paste(caffeineCitrateUnits, "qd") %in% scheduledUnits))
  expect_true(all(doseRoute(caffeineCitrateUnits) == ROUTE_IV))
  # the loading dose: 20 mg/kg of citrate at 1.1 kg is 11 mg of base, which in
  # 0.952 L is 11.6 mg/L
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "caffeineNeonatal", Time = 0, Dose = 20, Units = "mg/kg citrate"),
    events, 1.1, 38, 5 / 365.25, "male", 60, FALSE, adjustToFFM = FALSE
  )$caffeineNeonatal$wide
  expect_equal(max(wide$Plasma), 0.5 * 20 * 1.1 / (0.6299 * 1.1 + 0.259), tolerance = 1e-4)
})

test_that("the labelled regimen holds the concentration in Lee 2002's 10-20 mg/L", {
  # 20 mg/kg citrate, then 5 mg/kg citrate a day: the study's mean
  # concentrations ran from 10 to 20 mg/L over three weeks
  dose <- data.frame(Drug = "caffeineNeonatal", Time = c(0, 1440), Dose = c(20, 5),
                     Units = c("mg/kg citrate", "mg/kg citrate qd"))
  wide <- simulateDrugsWithCovariates(dose, events, 1.1, 38, 5 / 365.25, "male",
                                      21 * 1440, FALSE, adjustToFFM = FALSE
  )$caffeineNeonatal$wide
  later <- wide$Plasma[wide$Time > 60]
  expect_true(all(later > 8 & later < 21))
})

test_that("the defaults row offers only the labelled salt", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "caffeineNeonatal", ]
  expect_true(all(grepl("citrate", unlist(row$Units))))
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(5, 20, 12.5))
})
