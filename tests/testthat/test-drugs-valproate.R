# valproate: see the header of R/drugs_valproate.R (Teixeira-da-Silva 2022,
# weight only; three oral inputs and IV).  Expected values written out.
test_that("reference patient, switch off", {
  a <- valproate(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 14)
  expect_equal(a$PK$default$cl1, 0.646 / 60)
  expect_equal(a$PK$default$ka_PO, 0.78 / 60)
  expect_equal(a$oralFormulations$liquid$ka_PO, 2.64 / 60)
  expect_equal(a$oralFormulations$ER$ka_PO, 0.38 / 60)
  expect_equal(a$oralFormulations$ER$bioavailability_PO, 0.89)
  expect_equal(a$tPeak, 0); expect_equal(a$MEAC, 0)
})
test_that("weight scaling, switch on", {
  a <- valproate(120, 170, 50, "male", adjustToFFM = TRUE)
  w <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  expect_equal(a$PK$default$v1, 14 * (w / 70))
  expect_equal(a$PK$default$cl1, 0.646 * (w / 70)^0.75 / 60)
})

test_that("pregnancy: total CL/F x 1.6, nothing else", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  See the
  # PREGNANCY section of R/drugs_valproate.R (Nau and Krauer 1986, Yerby 1992).
  on  <- getDrugPK("valproate", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("valproate", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("valproate", 70, 165, 30, SEX_FEMALE))
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1,
               VALPROATE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(VALPROATE_PREGNANCY_CL_FACTOR, 1.6)
  for (k in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  for (form in names(off$oralFormulations))
    for (k in c("ka_PO", "bioavailability_PO", "tlag_PO"))
      expect_identical(on$oralFormulations[[form]]$default[[k]],
                       off$oralFormulations[[form]]$default[[k]], info = paste(form, k))
  expect_equal_rounded(11.28533074, on$PK$default$v1)
  expect_equal_rounded(0.01465517275, on$PK$default$cl1)
  expect_match(on$reference, "Nau H")
  expect_no_match(off$reference, "Nau H")
})
