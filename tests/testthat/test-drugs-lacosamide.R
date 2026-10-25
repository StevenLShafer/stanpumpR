# lacosamide: see R/drugs_lacosamide.R (BMC Pharmacol Toxicol 2026).
test_that("reference patient, switch off", {
  a <- lacosamide(70, 170, 40, "male", adjustToFFM = FALSE)
  crcl <- creatinineClearanceCG(70, 40, "male", adultEquivalentCreatinine(NULL, 40, "male"))
  expect_equal(a$PK$default$v1, 0.6 * 70)
  expect_equal(a$PK$default$cl1, 1.86 * (crcl / 119)^0.311 / 60)
  expect_equal(a$PK$default$ka_PO, 6.47 / 60)
})
test_that("women get the 0.875 factor", {
  m <- lacosamide(70, 170, 40, "male", adjustToFFM = FALSE)
  f <- lacosamide(70, 170, 40, "female", adjustToFFM = FALSE)
  crclM <- creatinineClearanceCG(70, 40, "male", adultEquivalentCreatinine(NULL, 40, "male"))
  crclF <- creatinineClearanceCG(70, 40, "female", adultEquivalentCreatinine(NULL, 40, "female"))
  expect_equal(f$PK$default$cl1 / (0.875 * (crclF / 119)^0.311),
               m$PK$default$cl1 / (crclM / 119)^0.311, tolerance = 1e-8)
})

test_that("pregnancy: the whole CL/F x 1.5, not through the renal term", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  See the
  # PREGNANCY section of R/drugs_lacosamide.R (MONEAD 2022, Barry 2025).
  on  <- getDrugPK("lacosamide", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("lacosamide", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("lacosamide", 70, 165, 30, SEX_FEMALE))
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1,
               LACOSAMIDE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(LACOSAMIDE_PREGNANCY_CL_FACTOR, 1.5)
  for (k in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  # A direct factor, so it is the same with a measured creatinine: it does
  # not act through the creatinine clearance (which would give 1.5^0.311)
  onC  <- getDrugPK("lacosamide", 70, 165, 30, SEX_FEMALE, creatinine = 0.6,
                    pregnant = TRUE)
  offC <- getDrugPK("lacosamide", 70, 165, 30, SEX_FEMALE, creatinine = 0.6,
                    pregnant = FALSE)
  expect_equal(onC$PK$default$cl1 / offC$PK$default$cl1,
               LACOSAMIDE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal_rounded(33.85599223, on$PK$default$v1)
  expect_equal_rounded(0.03750667244, on$PK$default$cl1)
  expect_match(on$reference, "MONEAD")
  expect_no_match(off$reference, "MONEAD")
})
