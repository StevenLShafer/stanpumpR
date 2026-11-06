# Cyclophosphamide (R/drugs_cyclophosphamide.R): Joerger 2007.  Drafted by
# Claude Code, 2026-11-06.

test_that("Table IV: one compartment, CL 4.23 L/h, V 34.6 L, half-life 5.7 h", {
  set <- getDrugPK("cyclophosphamide", 70, 165, 56, "female",
                   adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$cl1 * 60, 4.23)
  expect_equal_rounded(set$v1, 34.6)
  expect_equal(set$cl2, 0)
  expect_equal(round(log(2) / set$lambda_1 / 60, 2), 5.67)
})

test_that("the switch scales by the library's factors and leaves the published values off", {
  size <- pkSizeFactors(100, 165, 56, "female", TRUE)
  on <- cyclophosphamide(100, 165, 56, "female")$PK$default
  expect_equal_rounded(on$v1, 34.6 * size$volume)
  expect_equal_rounded(on$cl1 * 60, 4.23 * size$clearance)
  off <- cyclophosphamide(100, 165, 56, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off$v1, 34.6)
})

test_that("cyclophosphamide is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "cyclophosphamide", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("cyclophosphamide", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Joerger")
  expect_identical(therapeuticRange("cyclophosphamide")$band_status, "not_applicable")
})
