# levetiracetam: see R/drugs_levetiracetam.R (Rhee 2017 + renal split).
test_that("reference patient has total clearance 3.9 L/h", {
  a <- levetiracetam(70, 40, 40, "male", adjustToFFM = FALSE)
  # at the reference creatinine clearance, renal + non-renal = 3.9 L/h
  expect_equal(a$PK$default$cl1, 3.9 / 60, tolerance = 1e-8)
  # The label's 0.6 L/kg, 42 L at 70 kg, so the half-life is the label's 7 h
  expect_equal(a$PK$default$v1, 42)
  expect_equal(log(2) * a$PK$default$v1 / (a$PK$default$cl1 * 60), 7.46, tolerance = 1e-3)
  expect_equal(a$PK$default$ka_PO, 2.44 / 60)
})
test_that("clearance falls with renal function", {
  good <- levetiracetam(70, 170, 40, "male", creatinine = 0.8, adjustToFFM = FALSE)
  poor <- levetiracetam(70, 170, 40, "male", creatinine = 4.0, adjustToFFM = FALSE)
  expect_lt(poor$PK$default$cl1, good$PK$default$cl1)
  # non-renal floor: 34% of 3.9 at the reference remains even at very low CrCl
})

test_that("pregnancy: total clearance x 1.6, not through the renal split", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  See the
  # PREGNANCY section of R/drugs_levetiracetam.R.
  on  <- getDrugPK("levetiracetam", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("levetiracetam", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("levetiracetam", 70, 165, 30, SEX_FEMALE))
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1,
               LEVETIRACETAM_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(LEVETIRACETAM_PREGNANCY_CL_FACTOR, 1.6)
  for (k in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  # A direct factor on the whole clearance, the same with a measured
  # creatinine (through the renal split the 50% rise in filtration would give
  # only 0.66 x 1.5 + 0.34 = 1.33)
  onC  <- getDrugPK("levetiracetam", 70, 165, 30, SEX_FEMALE, creatinine = 0.6,
                    pregnant = TRUE)
  offC <- getDrugPK("levetiracetam", 70, 165, 30, SEX_FEMALE, creatinine = 0.6,
                    pregnant = FALSE)
  expect_equal(onC$PK$default$cl1 / offC$PK$default$cl1,
               LEVETIRACETAM_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal_rounded(33.85599223, on$PK$default$v1)
  expect_equal_rounded(0.0935606271, on$PK$default$cl1)
  expect_match(on$reference, "Westin")
  expect_no_match(off$reference, "Westin")
})
