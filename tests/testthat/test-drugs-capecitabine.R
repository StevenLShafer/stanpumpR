# Capecitabine (R/drugs_capecitabine.R): Gieschke 2003.  Drafted by Claude
# Code, 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60

test_that("2250 mg by mouth reproduces Table 6 on the molar reading", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "capecitabine", Time = 0, Dose = 2250, Units = "mg PO"),
    noEvents, 70, 170, 61, "male", 48 * 60, FALSE, adjustToFFM = FALSE)
  d <- w$capecitabine$wide; f <- w$capecitabine5FU$wide
  expect_equal(auc(d), 20.34, tolerance = 0.01)            # Table 6 median 20.13
  expect_equal(max(d$Plasma), 7.10, tolerance = 0.01)
  expect_equal(d$Time[which.max(d$Plasma)] / 60, 1.04, tolerance = 0.03)
  expect_equal(auc(f), 0.684, tolerance = 0.01)            # Table 6 median 0.707
  expect_equal(max(f$Plasma), 0.239, tolerance = 0.02)
})

test_that("the molar factors and Table 4's rates", {
  set <- getDrugPK("capecitabine", 70, 170, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$bioavailability_PO, 246.21 / 359.35)
  expect_equal_rounded(set$ka_PO * 60, 1.09)
  expect_equal_rounded(set$v1, 90.6)
  expect_equal_rounded(set$cl1 * 60, 75.8)
  expect_identical(set$metabolite$name, "capecitabine5FU")
  m <- getDrugPK("capecitabine5FU", 70, 170, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) * m$v1 / (m$cl1 * 60) * 3600), 37)   # seconds
})

test_that("capecitabine is a chemotherapy drug with the metabolite-only capecitabine5FU", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "capecitabine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_match(getDrugPK("capecitabine", 70, 170, 60, "male")$reference, "Gieschke")
  expect_identical(therapeuticRange("capecitabine")$band_status, "not_applicable")
  expect_identical(therapeuticRange("capecitabine5FU")$band_status, "not_applicable")
})
