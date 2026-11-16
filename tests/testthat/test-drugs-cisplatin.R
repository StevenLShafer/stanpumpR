# Cisplatin and protein-bound platinum (R/drugs_cisplatin.R,
# R/drugs_cisplatinBound.R): Urien 2004.  Drafted by Claude Code, 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())
htRef <- 1.7^2 * 3600 / 64                       # Mosteller BSA 1.7 m2 at 64 kg
scrRef <- (140 - 58) * 64 / (72 * 81)            # Cockcroft-Gault 81 mL/min

test_that("Tables 4 and 5 at the references: half-lives 0.333, 1.321 and 49.5 h", {
  u <- getDrugPK("cisplatin", 64, htRef, 58, "male", adjustToFFM = FALSE,
                 creatinine = scrRef)$PK$default
  expect_equal_rounded(u$v1, 23.4)
  expect_equal_rounded(u$cl1 * 60, 35.5)
  expect_equal_rounded(u$cl2 * 60, 8.64)
  expect_equal_rounded(u$v2, 12.0)
  halfLives <- sort(log(2) / c(u$lambda_1, u$lambda_2) / 60)
  expect_equal(round(halfLives, 3), c(0.333, 1.321))
  b <- cisplatinBound(64, htRef, 58, "male", adjustToFFM = FALSE, creatinine = scrRef)$PK$default
  expect_equal_rounded(b$v1, 1 / 0.017)
  expect_equal(round(log(2) / (b$cl1 / b$v1) / 60, 1), 49.5)
})

test_that("Table 4's V1 exponent 1.60, not the text's 2.3", {
  v1 <- function(bsa) cisplatin(64, bsa^2 * 3600 / 64, 58, "male", adjustToFFM = FALSE,
                                creatinine = scrRef)$PK$default$v1
  expect_equal(v1(2.0) / v1(1.7), (2.0 / 1.7)^1.60)
})

test_that("bound formation is (fm/Vm) CL C1: its BSA and CLCr terms cancel those on CL", {
  rate <- function(bsa, scr) {
    ht <- bsa^2 * 3600 / 64
    u <- cisplatin(64, ht, 58, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
    b <- cisplatinBound(64, ht, 58, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
    u$cl1 / b$v1          # (fm/Vm) x CL
  }
  expect_equal(rate(1.5, 1.2), rate(2.0, 0.7))
  expect_equal(rate(1.7, scrRef) * 60, 0.017 * 35.5)
})

test_that("34.4 mg over 30 min: unbound about 960 ug/L at its end, total near the figures", {
  d <- data.frame(Drug = "cisplatin", Time = c(0, 30), Dose = c(68.8, 0), Units = "mg/hr")
  w <- simulateDrugsWithCovariates(d, noEvents, 64, htRef, 58, "male", 25 * 60, FALSE,
                                   adjustToFFM = FALSE, creatinine = scrRef)
  u <- w$cisplatin$wide; b <- w$cisplatinBound$wide
  at <- function(x, h) x$Plasma[which.min(abs(x$Time - h * 60))]
  expect_equal(at(u, 0.5), 962, tolerance = 0.02)
  expect_equal(at(u, 1) + at(b, 1), 764, tolerance = 0.03)
  expect_equal(at(u, 24) + at(b, 24), 425, tolerance = 0.03)
})

test_that("cisplatin is a chemotherapy drug with the metabolite-only cisplatinBound", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "cisplatin", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("cisplatin", 70, 170, 60, "male")
  expect_identical(PK$PK$default$metabolite$name, "cisplatinBound")
  expect_match(PK$reference, "Urien")
  expect_identical(therapeuticRange("cisplatin")$band_status, "not_applicable")
  expect_identical(therapeuticRange("cisplatinBound")$band_status, "not_applicable")
})
