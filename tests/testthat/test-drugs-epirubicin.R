# Epirubicin (R/drugs_epirubicin.R): Ralph 2003, clearance set by the serum
# AST.  Drafted by Claude Code, 2026-11-10.

noEvents <- data.frame(Time = double(), Event = character())

test_that("Table 3's final model: CL 72.9 x (1 - 0.135 ln AST), three compartments", {
  d <- epirubicin(70, 170, 35, "male", adjustToFFM = FALSE, ast = 90)$PK$default
  expect_equal_rounded(d$cl1 * 60, 72.9 * (1 - 0.135 * log(90)))
  expect_equal(round(d$cl1 * 60, 1), 28.6)                # the cohort's median AST
  expect_equal(c(d$v1, d$v2, d$v3), c(10.3, 35.7, 772))
  expect_equal_rounded(c(d$cl2, d$cl3) * 60, c(30.2, 61.5))
})

test_that("the AST is held within 7-815 IU/L, and a blank field is a normal 20 IU/L", {
  cl <- function(a) epirubicin(70, 170, 35, "male", FALSE, ast = a)$PK$default$cl1 * 60
  expect_equal(round(cl(7), 1), 53.7)
  expect_equal(cl(1), cl(7))
  expect_equal(round(cl(815), 1), 6.9)
  expect_equal(cl(5000), cl(815))                         # never zero or negative
  expect_equal(round(cl(NULL), 1), 43.4)
  expect_equal(cl(NULL), cl(EPIRUBICIN_REFERENCE_AST))
  expect_true(all(is.finite(sapply(c(MIN_AST, MAX_AST), cl))) && cl(MAX_AST) > 0)
})

test_that("getDrugPK passes the AST to epirubicin only, and refuses one out of range", {
  pk <- function(a) getDrugPK("epirubicin", 70, 170, 35, "male", adjustToFFM = FALSE,
                              ast = a)$PK$default$cl1
  expect_lt(pk(200), pk(20))
  expect_equal(pk(NA), pk(NULL))
  expect_error(pk(0), "Invalid ast")
  expect_equal(getDrugPK("doxorubicin", 70, 170, 35, "male", ast = 500)$PK$default$cl1,
               getDrugPK("doxorubicin", 70, 170, 35, "male")$PK$default$cl1)
  expect_identical(patientFieldReaders("ast"), "epirubicin")
})

test_that("a 100 mg dose: a higher AST raises the exposure as clearance falls", {
  auc <- function(a) {
    dose <- data.frame(Drug = "epirubicin", Time = 0, Dose = 100, Units = "mg")
    w <- simulateDrugsWithCovariates(dose, noEvents, 63, 157, 57, "female", 14 * 1440,
                                     FALSE, adjustToFFM = FALSE, ast = a)$epirubicin$wide
    sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  }
  # ng h/mL; dose / CL, nearly all of it within 14 days
  expect_equal(auc(90) / auc(20), epirubicinClearance(20) / epirubicinClearance(90),
               tolerance = 0.02)
})

test_that("both switch positions are pinned", {
  on <- epirubicin(70, 170, 35, "male")$PK$default
  off <- epirubicin(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(on$v3, off$v3)
  big <- epirubicin(120, 170, 35, "male")$PK$default
  size <- pkSizeFactors(120, 170, 35, "male", TRUE)
  expect_equal_rounded(big$v3, 772 * size$volume)
})

test_that("epirubicin is a chemotherapy drug, plasma only, in ng/mL, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "epirubicin", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("epirubicin", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Ralph")
  expect_identical(therapeuticRange("epirubicin")$band_status, "not_applicable")
})
