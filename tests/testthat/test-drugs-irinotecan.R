# Irinotecan and SN-38 (R/drugs_irinotecan.R, R/drugs_irinotecanSN38.R):
# Klein 2002.  Drafted by Claude Code, 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())

test_that("Table I: irinotecan CL 25.2 L/h, Vss 262.4 L, half-lives 0.01, 2.5, 13.5 h", {
  set <- getDrugPK("irinotecan", 70, 170, 60, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(set$cl1 * 60, 2), 25.17)
  expect_equal(round(set$v1 + set$v2 + set$v3, 1), 262.4)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2, set$lambda_3) / 60)
  expect_equal(round(halfLives, c(4)), round(c(0.0104, 2.47, 13.55), 4), tolerance = 0.005)
})

test_that("SN-38: CL/Fmsn 1174 L/h, half-lives 0.135 and 24.3 h", {
  s <- getDrugPK("irinotecanSN38", 70, 170, 60, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(s$cl1 * 60, 1), 1174.3)
  halfLives <- sort(log(2) / c(s$lambda_1, s$lambda_2) / 60)
  expect_equal(round(halfLives, 3), c(0.135, 24.264), tolerance = 1e-3)
})

test_that("180 mg over 90 min: irinotecan peak about 1273 ng/mL, SN-38 about 12.5 ng/mL", {
  d <- data.frame(Drug = "irinotecan", Time = c(0, 90), Dose = c(120, 0), Units = "mg/hr")
  w <- simulateDrugsWithCovariates(d, noEvents, 70, 170, 60, "male", 72 * 60, FALSE,
                                   adjustToFFM = FALSE)
  expect_equal(max(w$irinotecan$wide$Plasma), 1273, tolerance = 0.01)
  expect_equal(max(w$irinotecanSN38$wide$Plasma), 12.5, tolerance = 0.02)
  # all of irinotecan's elimination forms SN-38 (Fmsn-scaled): AUC = dose / CL
  m <- w$irinotecanSN38$wide
  aucM <- sum(diff(m$Time) * (head(m$Plasma, -1) + tail(m$Plasma, -1)) / 2) / 60
  expect_lt(aucM, 180000 / 1174.3)
  expect_gt(aucM, 0.6 * 180000 / 1174.3)   # 72 h of a 24-h terminal half-life
})

test_that("irinotecan is a chemotherapy drug with the metabolite-only irinotecanSN38", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "irinotecan", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("irinotecan", 70, 170, 60, "male")
  expect_identical(PK$PK$default$metabolite$name, "irinotecanSN38")
  expect_match(PK$reference, "Klein")
  expect_identical(therapeuticRange("irinotecan")$band_status, "not_applicable")
  expect_identical(therapeuticRange("irinotecanSN38")$band_status, "not_applicable")
})
