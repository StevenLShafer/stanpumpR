# Oral thioguanine (R/drugs_thioguanine.R): Kitchen 1997's disposition with
# absorption derived from Lowe 2001.  Drafted by Claude Code, 2026-11-16.

noEvents <- data.frame(Time = double(), Event = character())

test_that("F is Lowe's oral AUC x his intravenous clearance / dose", {
  f <- 1.5 * 0.16719 * (935 * 60 / 1000) / 60
  expect_equal(THIOGUANINE_ORAL$f, f)
  expect_equal(round(f, 3), 0.234)
  d <- thioguanine(20, 100, 3, "male")$PK$default
  expect_equal(c(d$ka_PO * 60, d$tlag_PO), c(0.62, 60))
  # the disposition is Kitchen's
  iv <- thioguanineIV(20, 100, 3, "male")
  expect_equal(d$v1, iv$PK$default$v1)
  expect_equal(thioguanine(20, 100, 3, "male")$michaelisMenten, iv$michaelisMenten)
})

test_that("60 mg/m2 in a child of 0.6 m2 meets Lowe's medians", {
  h <- 100; w <- uniroot(function(w) bsaMosteller(w, h) - 0.6, c(3, 80))$root
  dose <- data.frame(Drug = "thioguanine", Time = 0, Dose = 36, Units = "mg PO")
  wd <- simulateDrugsWithCovariates(dose, noEvents, w, h, 3, "male", 24 * 60,
                                    FALSE)$thioguanine$wide
  tt <- seq(0, 600, 0.5); cc <- approx(wd$Time, wd$Plasma, tt)$y / 0.16719   # uM
  expect_equal(max(cc), 0.29, tolerance = 0.01)                 # the fit: median Cmax
  expect_equal(tt[which.max(cc)] / 60, 2.0, tolerance = 0.02)   # the fit: median Tmax
  auc <- with(wd, sum(diff(Time) * (head(Plasma, -1) + tail(Plasma, -1)) / 2)) / 60 / 0.16719
  expect_equal(auc, 1.0, tolerance = 0.05)                      # check: median AUC
  late <- tt >= 240 & tt <= 480
  t12 <- log(2) / (-coef(lm(log(cc[late]) ~ tt[late]))[[2]] * 60)
  expect_true(t12 > 1.4 && t12 < 2.0)                           # check: Lowe's 1.6 h
})

test_that("an oral chemotherapy drug on the saturable engine, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "thioguanine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Default.Units, "mg PO")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_true(drugOnSaturableEngine("thioguanine"))
  expect_match(getDrugPK("thioguanine", 30, 130, 8, "male")$reference, "DERIVED")
  expect_identical(therapeuticRange("thioguanine")$band_status, "not_applicable")
})
