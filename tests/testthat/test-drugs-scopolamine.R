# Scopolamine by transdermal patch (R/drugs_scopolamine.R): Liem-Moolenaar
# 2011's disposition fed by a skin depot, on the new TD route.  Drafted by
# Claude Code, 2026-11-13.  Expectations from Table 2 of the source and the
# Transderm Scop label, not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table 2's disposition; the skin depot empties with the label's 9.5 h", {
  p <- scopolamine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(p$cl1, p$v1, p$cl2), c(2.53, 66.3, 4.78))
  expect_equal(p$v1 + p$v2, 250)
  expect_equal(p$ka_TD, log(2) / (9.5 * 60))
  pk <- getDrugPK("scopolamine", 70, 170, 35, "male",
                  drugDefaults = getDrugDefaultsGlobal())
  expect_equal(pk$tissueRoute, ROUTE_TD)
})

test_that("the TD route is a rate into the tissue depot", {
  expect_true(ROUTE_TD %in% DOSE_ROUTES)
  expect_true(ROUTE_TD %in% TISSUE_ROUTES)
  expect_equal(doseRoute(tdRateUnits), ROUTE_TD)
  expect_true(isRateUnit(tdRateUnits))
  expect_true(all(tdRateUnits %in% allUnits))
  expect_equal(routeKaField(ROUTE_TD), "ka_RA")
  # one tissue route per drug
  expect_error(subcutaneousToTissueDepot(list(ka_SC = 1, ka_TD = 1), "x"), "one tissue route")
})

test_that("one patch: the label's 87 pg/mL plateau, and a 9.5 h decline after removal", {
  d <- data.frame(Drug = "scopolamine", Time = c(0, 72 * 60), Dose = c(1 / 3, 0),
                  Units = "mg/day TD")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 5 * 1440,
                                   FALSE, adjustToFFM = FALSE)$scopolamine$wide
  at <- function(h) w$Plasma[which.min(abs(w$Time - h * 60))]
  # plateau: 1 mg over 72 h at CL 151.8 L/h, 91.5 pg/mL; the label's average 87
  expect_equal(at(72), 1e6 / 72 / (2.53 * 60), tolerance = 0.01)
  expect_equal(at(72), 87, tolerance = 0.06)
  # detected within 4 h; most of the plateau by 24 h
  expect_gt(at(4), 10); expect_gt(at(24) / at(72), 0.75)
  # after removal the decline is the skin's: halving in about 9.5 h
  ratio <- at(72 + 30) / at(72 + 20.5)
  expect_equal(ratio, 0.5, tolerance = 0.05)
  # the whole delivered dose reaches the circulation
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)  # pg.min/mL
  expect_equal(auc * 2.53 / 1e6, 1, tolerance = 0.02)                       # mg
})

test_that("switch on, a 120-kg man: volumes by the FFM ratio, clearances to 0.75", {
  f <- ffmRatioMaleIndependent(120, 170, 50)
  p <- scopolamine(120, 170, 50, "male")$PK$default
  expect_equal_rounded(c(66.3, 250 - 66.3) * f, c(p$v1, p$v2))
  expect_equal_rounded(c(2.53, 4.78) * f^0.75, c(p$cl1, p$cl2))
  expect_equal(c(p$v3, p$cl3), c(1, 0))
})
