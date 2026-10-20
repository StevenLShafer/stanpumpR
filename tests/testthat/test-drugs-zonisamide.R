# zonisamide: see R/drugs_zonisamide.R (Silva 2025).
test_that("reference patient, switch off", {
  a <- zonisamide(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 48.10)
  expect_equal(a$PK$default$cl1, 0.761 / 60)
  expect_equal(a$PK$default$ka_PO, 0.671 / 60)
})
test_that("FFM scaling, switch on", {
  a <- zonisamide(120, 170, 50, "male", adjustToFFM = TRUE)
  s <- pkSizeFactors(120, 170, 50, "male", TRUE)
  expect_equal(a$PK$default$cl1, 0.761 * s$clearance / 60)
})

test_that("pregnancy: CL/F x 1.6, nothing else", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  See the
  # PREGNANCY section of R/drugs_zonisamide.R (Reimers 2018, MONEAD 2022).
  on  <- getDrugPK("zonisamide", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("zonisamide", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("zonisamide", 70, 165, 30, SEX_FEMALE))
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1,
               ZONISAMIDE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(ZONISAMIDE_PREGNANCY_CL_FACTOR, 1.6)
  for (k in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  expect_equal_rounded(38.77317206, on$PK$default$v1)
  expect_equal_rounded(0.01726406574, on$PK$default$cl1)
  expect_match(on$reference, "Reimers")
  expect_no_match(off$reference, "Reimers")
})
