# eslicarbazepine: see R/drugs_eslicarbazepine.R (Falcao 2012 + poster).
test_that("reference patient, switch off", {
  a <- eslicarbazepine(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 61.3)
  expect_equal(a$PK$default$cl1, 2.43 / 60)
  expect_equal(a$PK$default$ka_PO, 2.34 / 60)
  expect_equal(a$PK$default$bioavailability_PO, 254.3 / 296.3, tolerance = 1e-8)
})
test_that("allometric scaling, switch on", {
  a <- eslicarbazepine(120, 170, 50, "male", adjustToFFM = TRUE)
  w <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  expect_equal(a$PK$default$cl1, 2.43 * (w / 70)^0.75 / 60)
})

test_that("pregnancy: CL/F x 1.6 from oxcarbazepine's MHD, nothing else", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  See the
  # PREGNANCY section of R/drugs_eslicarbazepine.R: a surrogate from the
  # monohydroxy derivative of oxcarbazepine, mostly eslicarbazepine.
  on  <- getDrugPK("eslicarbazepine", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("eslicarbazepine", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("eslicarbazepine", 70, 165, 30, SEX_FEMALE))
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1,
               ESLICARBAZEPINE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(ESLICARBAZEPINE_PREGNANCY_CL_FACTOR, 1.6)
  # The acetate-to-eslicarbazepine mass factor stays on bioavailability
  for (k in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  expect_equal_rounded(49.41362676, on$PK$default$v1)
  expect_equal_rounded(0.05512704302, on$PK$default$cl1)
  expect_match(on$reference, "Christensen")
  expect_no_match(off$reference, "Christensen")
})
