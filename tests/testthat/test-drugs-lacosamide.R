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
