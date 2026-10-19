# phenobarbital: see R/drugs_phenobarbital.R (Munich 2025, ideal body weight).
test_that("reference patient, switch off (total weight)", {
  a <- phenobarbital(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 34.3 * (70 / 68.8))
  expect_equal(a$PK$default$cl1, 0.38 * (70 / 68.8)^0.75 / 60)
  expect_equal(a$PK$default$bioavailability_PO, 0.96)
  expect_equal(a$PK$default$ka_PO, 1.9 / 60)
})
test_that("adult switch on uses ideal body weight", {
  a <- phenobarbital(70, 178, 40, "male", adjustToFFM = TRUE)
  ibw <- 50 + 2.3 * (178 / 2.54 - 60)   # Devine, shared idealBodyWeightDevine()
  expect_equal(a$PK$default$v1, 34.3 * (ibw / 68.8))
})
test_that("a child falls back to the pharmacokinetic weight", {
  a <- phenobarbital(20, 110, 5, "male", adjustToFFM = TRUE)
  w <- pkSizeFactors(20, 110, 5, "male", TRUE)$pkWeight
  expect_equal(a$PK$default$v1, 34.3 * (w / 68.8))
})

test_that("pregnancy: clearance x 1.25 (Dam 1979), nothing else", {
  on  <- getDrugPK("phenobarbital", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("phenobarbital", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("phenobarbital", 70, 165, 30, SEX_FEMALE))
  expect_equal(PHENOBARBITAL_PREGNANCY_CL_FACTOR, 1.25)
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1, PHENOBARBITAL_PREGNANCY_CL_FACTOR,
               tolerance = 1e-12)
  for (k in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  # The factor is the median of Dam 1979 Table III's per-woman ratios, weeks
  # 37-40 over the latest postpartum values (weeks 8-20 where measured)
  late <- list(c(3.9, 3.9, 3.9, 3.7), c(4.5, 4.5, 5.2, 5.5), c(5.5, 5.2, 5.0, 6.5),
               c(6.9, 8.7, 6.1, 5.8), c(7.9, 6.7, 6.9), c(6.0, 6.5, 6.5, 6.3),
               c(3.9, 4.4, 4.6, 3.7))
  post <- list(c(2.9, 2.9, 3.3, 3.2), c(3.2, 3.3), c(5.0, 5.2, 5.5, 4.7),
               c(5.8, 4.7, 4.3), 6.1, c(7.1, 7.1), c(3.5, 2.9, 2.8))
  expect_equal(median(mapply(function(a, b) mean(a) / mean(b), late, post)),
               PHENOBARBITAL_PREGNANCY_CL_FACTOR, tolerance = 0.005)
  expect_match(on$reference, "Dam M et al.", fixed = TRUE)
  expect_no_match(off$reference, "Dam M")
})
