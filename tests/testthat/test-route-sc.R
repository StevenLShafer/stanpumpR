# The subcutaneous route (ROUTE_SC): a first-order depot into the central
# compartment, exactly as the intramuscular route, used by the
# low-molecular-weight heparins and fondaparinux.  The engines carry it on the
# tissue-depot (RA) states (R/subcutaneous.R): getDrugPK() moves ka_SC and
# the rest onto the _RA fields and simCpCe() flags SC rows as RA.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("doseRoute() reads SC from the units, and the SC units are offered", {
  expect_equal(doseRoute(c("mg SC", "mg/kg SC", "IU SC", "IU/kg SC")),
               rep(ROUTE_SC, 4))
  expect_true(all(c(scUnits, iuScUnits) %in% allUnits))
  expect_true(all(doseRoute(scUnits) == ROUTE_SC))
  expect_true(all(doseRoute(iuScUnits) == ROUTE_SC))
  # No SC unit is a rate (single injections) or a scheduled frequency.
  expect_false(any(isRateUnit(c(scUnits, iuScUnits))))
})

test_that("the SC depot reproduces the IM depot for identical parameters", {
  # A subcutaneous dose and an intramuscular dose with the same ka, F, lag and
  # disposition must give the same plasma curve: the two depots share the math.
  ds <- getDrugPK("fondaparinux", 68, 170, 74, "male", creatinine = 0.9)$PK$default
  expect_gt(ds$ka_RA, 0)
  imset <- ds
  for (fld in grep("_RA(_|$)", names(imset), value = TRUE)) {
    imset[[sub("_RA", "_IM", fld)]] <- imset[[fld]]
    imset[[fld]] <- 0
  }
  scDose <- data.frame(Time = 0, Dose = 2.5, Bolus = FALSE, RA = TRUE)
  imDose <- data.frame(Time = 0, Dose = 2.5, Bolus = FALSE, IM = TRUE)
  rSC <- advanceClosedFormPO_IM_IN(scDose, ds,    4320, FALSE, 0)
  rIM <- advanceClosedFormPO_IM_IN(imDose, imset, 4320, FALSE, 0)
  expect_equal(rSC$Cp, rIM$Cp, tolerance = 1e-12)
})

test_that("an SC dose builds a depot that peaks after the injection", {
  # A single SC dose rises from zero, peaks later than time zero (first-order
  # absorption), then falls.
  dose <- data.frame(Drug = "fondaparinux", Time = 0, Dose = 2.5, Units = "mg SC")
  r <- simulateDrugsWithCovariates(dose, noEvents, 68, 170, 74, "male",
                                   2880, TRUE, creatinine = 0.9,
                                   adjustToFFM = FALSE)$fondaparinux
  pl <- r$results[r$results$Site == "Plasma", ]
  peakTime <- pl$Time[which.max(pl$Y)]
  expect_gt(peakTime, 0)
  expect_equal(pl$Y[pl$Time == 0], 0)
  expect_gt(max(pl$Y), 0)
})

test_that("a lag delays the subcutaneous absorption", {
  # Build two PK sets differing only in the depot's lag (tlag_SC, carried as
  # tlag_RA) and confirm the curve shifts.
  base <- getDrugPK("fondaparinux", 68, 170, 74, "male", creatinine = 0.9)$PK$default
  lagged <- base; lagged$tlag_RA <- 120
  dose <- data.frame(Time = 0, Dose = 2.5, Bolus = FALSE, RA = TRUE)
  r0 <- advanceClosedFormPO_IM_IN(dose, base,   2880, FALSE, 0)
  rL <- advanceClosedFormPO_IM_IN(dose, lagged, 2880, FALSE, 0)
  # At 60 min the unlagged curve has drug and the lagged one has essentially none.
  expect_gt(approx(r0$Time, r0$Cp, 60)$y, 0)
  expect_equal(approx(rL$Time, rL$Cp, 60)$y, 0, tolerance = 1e-8)
})
