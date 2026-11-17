# Nortriptyline (R/drugs_nortriptyline.R): Vos 2025's well-stirred model.
# Drafted by Claude Code, 2026-11-12.  Expectations from ESM 1 of the source
# and Hart 2026, not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("the well-stirred equations with Q 36, CLint 42.3, V 1080, ka 0.533", {
  p <- nortriptyline(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(p$cl1 * 60, 36 * 42.3 / (36 + 42.3))
  expect_equal(p$bioavailability_PO, 36 / (36 + 42.3))
  # apparent oral clearance is CLint
  expect_equal(p$cl1 * 60 / p$bioavailability_PO, 42.3)
  expect_equal(p$v1, 1080)
  expect_equal(p$ka_PO * 60, 0.533)
  expect_equal(log(2) * p$v1 / p$cl1 / 60, 38.5, tolerance = 0.005)
})

test_that("Hart 2026's concentration per dose, 24 h after a once-daily dose", {
  # Table 6: 0.804 (0.578-1.319) ng/mL per mg/day
  d <- data.frame(Drug = "nortriptyline", Time = 0, Dose = 100, Units = "mg PO qd")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 21 * 1440,
                                   FALSE)$nortriptyline$wide
  trough <- w$Plasma[which.min(abs(w$Time - 21 * 1440 + 1e-6))]
  expect_equal(trough / 100, 0.804, tolerance = 0.03)
})

test_that("the band is Hart 2026's 70-170 ng/mL", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "nortriptyline", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(70, 170, 120))
  expect_equal(row$Category, "Antidepressants")
})
