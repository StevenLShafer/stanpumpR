# Cytarabine (R/drugs_cytarabine.R): one drug, the program choosing the model
# from the patient's age: Periclou 1996 under 19 years, Krogh-Madsen 2012
# from 19.  Drafted by Claude Code, 2026-11-10.

test_that("the model is chosen by age, at 19 years", {
  expect_identical(cytarabineModel(0), "Periclou 1996")
  expect_identical(cytarabineModel(18.99), "Periclou 1996")
  expect_identical(cytarabineModel(19), "Krogh-Madsen 2012")
  expect_identical(cytarabineModel(80), "Krogh-Madsen 2012")
  expect_match(getDrugPK("cytarabine", 32, 138, 10, "male")$reference, "Periclou")
  expect_match(getDrugPK("cytarabine", 70, 170, 40, "male")$reference, "Krogh-Madsen")
})

test_that("adults: Krogh-Madsen Table 2", {
  X <- cytarabine(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(X$cl1 * 60, 272)
  expect_equal_rounded(X$v1, 62.8)
  expect_equal_rounded(X$cl2 * 60, 13.7)
  expect_equal_rounded(X$v2, 75.4)
  PK <- getDrugPK("cytarabine", 70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) / c(PK$lambda_1, PK$lambda_2) / 60, 2), c(0.15, 4.01))
})

test_that("children: Periclou Model I, every parameter x age x BSA", {
  bsa <- sqrt(32 * 138 / 3600)
  X <- cytarabine(32, 138, 10, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(X$cl1 * 60, 2.59 * 10 * bsa)
  expect_equal_rounded(X$cl2 * 60, 2.01 * 10 * bsa)
  expect_equal_rounded(X$v1, 0.48 * 10 * bsa)
  expect_equal_rounded(X$v2, 38.1 * 10 * bsa)
  # the half-lives are the same at every age (all four parameters scale together)
  hl <- function(...) {
    PK <- getDrugPK("cytarabine", ..., adjustToFFM = FALSE)$PK$default
    log(2) / c(PK$lambda_1, PK$lambda_2) / 60
  }
  expect_equal(hl(32, 138, 10, "male"), hl(12, 85, 2, "female"))
  expect_equal(round(hl(32, 138, 10, "male"), 2), c(0.07, 23.39))
})

test_that("the age is held at 0.17 years, the youngest studied, so a newborn stays finite", {
  newborn <- cytarabine(3.5, 50, 0, "male", adjustToFFM = FALSE)$PK$default
  floor <- cytarabine(3.5, 50, 0.17, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(newborn$cl1, floor$cl1)
  expect_gt(newborn$v1, 0)
})

test_that("the jump at the boundary, as accepted (Shafer, 2026-11-10)", {
  child <- cytarabine(70, 170, 18.9, "male", adjustToFFM = FALSE)$PK$default
  adult <- cytarabine(70, 170, 19, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(child$cl1 * 60), 89)
  expect_equal(adult$cl1 * 60, 272)
})

test_that("both switch positions are pinned", {
  on <- cytarabine(70, 170, 35, "male")$PK$default
  off <- cytarabine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(on$cl1, off$cl1)
  expect_lt(cytarabine(70, 120, 12, "male")$PK$default$v1,
            cytarabine(70, 120, 12, "male", adjustToFFM = FALSE)$PK$default$v1)
})

test_that("cytarabine is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "cytarabine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_equal(getDrugPK("cytarabine", 70, 170, 60, "male")$tPeak, 0)
  expect_identical(therapeuticRange("cytarabine")$band_status, "not_applicable")
})
