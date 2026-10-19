# insulinRegularU500: see R/insulinTham.R (Tham 2017, Table 2).
test_that("U-500 shares U-100's disposition and absorbs at 0.185/h, whatever the dose", {
  a <- insulinRegularU500(80, 175, 40, "male", FALSE, dose = 75)$PK$default
  b <- insulinRegular(80, 175, 40, "male", FALSE, dose = 75)$PK$default
  expect_equal(a$v1, b$v1); expect_equal(a$cl1, b$cl1)
  expect_equal(a$cl1, 127 * 7.5^-0.282 / 60)   # the study's 75 U: CL/F 72 L/h
  expect_equal(a$ka_SC, 0.185 / 60)            # no dose effect on U-500's ka
  expect_false(any(getDrugDefaults("insulinRegularU500")$Units[[1]] %in% insulinRateUnits))
})

test_that("pregnancy: CL/F x 1.25, absorption and V/F unchanged", {
  # A woman of 30 years, 70 kg, 165 cm: CL/F x 1.25 (Catalano 1998), in
  # thamInsulin() (R/insulinTham.R, PREGNANCY); V/F and absorption unchanged.
  on  <- getDrugPK("insulinRegularU500", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("insulinRegularU500", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("insulinRegularU500", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  same <- c("v1", "v2", "v3", "cl2", "cl3", "ka_RA", "bioavailability_RA",
            "tlag_RA")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  # Each dose size is its own system (doseSizePK()); the flag reaches it
  expect_equal(doseSizePK(on, 40)$PK[[1]]$cl1 / doseSizePK(off, 40)$PK[[1]]$cl1,
               INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_match(on$reference, "Catalano", fixed = TRUE)
  expect_no_match(off$reference, "Catalano", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- insulinRegularU500(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(101.1157443, actual$v1)
  expect_equal_rounded(1, actual$v2)
  expect_equal_rounded(1, actual$v3)
  expect_equal_rounded(2.645833, actual$cl1)
  expect_equal_rounded(0, actual$cl2)
  expect_equal_rounded(0, actual$cl3)
})
