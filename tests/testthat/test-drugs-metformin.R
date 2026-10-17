# metformin: see R/drugs_metformin.R (Chae 2012).
test_that("CL/F 52.6 L/h x (Cockcroft-Gault/106.5)^0.782, switch off", {
  pk <- metformin(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  crcl <- creatinineClearanceCG(70, 40, "male", adultEquivalentCreatinine(NULL, 40, "male"))
  expect_equal(pk$cl1, 52.6 * (crcl / 106.5)^0.782 / 60)
  expect_equal(pk$v1, 113)
  expect_equal(pk$ka_PO, 0.41 / 60)
})
test_that("a higher creatinine lowers clearance; switch on uses the pharmacokinetic weight", {
  a <- metformin(70, 170, 40, "male", creatinine = 0.9)$PK$default$cl1
  b <- metformin(70, 170, 40, "male", creatinine = 2.0)$PK$default$cl1
  expect_lt(b, a)
  size <- pkSizeFactors(100, 170, 40, "male", TRUE, legacyVolume = 1)
  on <- metformin(100, 170, 40, "male")$PK$default
  crcl <- creatinineClearanceCG(size$pkWeight, 40, "male", adultEquivalentCreatinine(NULL, 40, "male"))
  expect_equal(on$cl1, 52.6 * (crcl / 106.5)^0.782 / 60)
  expect_equal(on$v1, 113 * size$volume)
})
