# lamotrigine: see R/drugs_lamotrigine.R (Milosheska 2016 base model).
test_that("reference patient, switch off", {
  a <- lamotrigine(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 77.6)
  expect_equal(a$PK$default$cl1, 2.32 / 60)
  expect_equal(a$PK$default$ka_PO, 1.96 / 60)
})
test_that("FFM scaling, switch on", {
  a <- lamotrigine(120, 170, 50, "male", adjustToFFM = TRUE)
  s <- pkSizeFactors(120, 170, 50, "male", TRUE)
  expect_equal(a$PK$default$v1, 77.6 * s$volume)
  expect_equal(a$PK$default$cl1, 2.32 * s$clearance / 60)
})

test_that("pregnancy: CL/F x 2.3, nothing else", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  See the
  # PREGNANCY section of R/drugs_lamotrigine.R (Karanam 2025, at 36 weeks).
  on  <- getDrugPK("lamotrigine", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("lamotrigine", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("lamotrigine", 70, 165, 30, SEX_FEMALE))
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1,
               LAMOTRIGINE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(LAMOTRIGINE_PREGNANCY_CL_FACTOR, 2.3)
  # Karanam's Emax model at 36 weeks, which the constant rounds
  expect_equal((2.79 + 4.9 * 36 / (12 + 36)) / 2.79, LAMOTRIGINE_PREGNANCY_CL_FACTOR,
               tolerance = 0.01)
  for (k in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  expect_equal_rounded(62.55297612, on$PK$default$v1)
  expect_equal_rounded(0.0756578965, on$PK$default$cl1)
  expect_match(on$reference, "Karanam")
  expect_no_match(off$reference, "Karanam")
})
