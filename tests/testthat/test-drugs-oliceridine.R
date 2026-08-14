# Tests for drugs_oliceridine.R. Oliceridine was the only drug (besides
# remimazolam) without a test file.
#
# The model is covariate-independent (fixed volumes/clearances from Dahan
# 2020), so beyond the usual snapshot the tests assert that covariates do not
# change the PK and that the structure matches what getDrugPK() consumes.
#
# The reference is asserted with expect_match on the author/journal rather
# than an exact string, so the test survives citation-formatting updates
# (e.g. the drug-reference-citations work) without churn.
#
# Provenance: drafted by Claude Code (Opus 4.8), 2026-08-13, for the
# pre-deployment test plan (drug library). Expected values verified by
# running drugs_oliceridine.R directly: cl1 = 31.7/60, cl2 = 37.5/60.

test_that("returns the correct calculations", {
  actual <- oliceridine(weight = 70, height = 170, age = 50, sex = "male")

  expect_equal_rounded(
    actual$PK,
    list(
      default = list(
        v1 = 28,
        v2 = 29.1,
        v3 = 1,             # placeholder: model is effectively 2-compartment
        cl1 = 0.5283333,    # 31.7 L/h / 60 = L/min
        cl2 = 0.625,        # 37.5 L/h / 60
        cl3 = 0
      )
    )
  )
  expect_equal(actual$tPeak, 15)   # 0.25 h * 60 = minutes
  expect_match(actual$reference, "Dahan")
})

test_that("PK does not vary with covariates (fixed model)", {
  a <- oliceridine(70, 170, 50, "male")
  b <- oliceridine(40, 150, 25, "female")
  c <- oliceridine(120, 190, 80, "male")
  expect_equal(a$PK, b$PK)
  expect_equal(a$PK, c$PK)
})

test_that("return structure has the fields the engine consumes", {
  actual <- oliceridine(70, 170, 50, "male")
  expect_named(
    actual,
    c("PK", "tPeak", "MEAC", "typical", "upperTypical", "lowerTypical",
      "reference")
  )
  expect_named(actual$PK, "default")
  expect_named(actual$PK$default, c("v1", "v2", "v3", "cl1", "cl2", "cl3"))
  # Volumes and clearances the engine divides by must be positive
  expect_gt(actual$PK$default$v1, 0)
  expect_gt(actual$PK$default$cl1, 0)
  expect_gt(actual$tPeak, 0)
})
