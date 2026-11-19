# pramlintideIV: see R/drugs_pramlintideIV.R (Fang 2013, Table II).
test_that("Table II, fixed with the switch off", {
  pk <- pramlintideIV(77, 182, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(pk$cl1, pk$cl2), c(0.955, 0.283))
  expect_equal(c(pk$v1, pk$v2), c(19.1, 11.0))
  expect_null(pk$ka_PO)
  expect_null(pk$ka_SC)
})
test_that("intravenous only: a bolus and an infusion, no SC unit", {
  units <- getDrugDefaults("pramlintideIV")$Units[[1]]
  expect_equal(units, c("mcg", "mcg/min"))
  expect_true(all(doseRoute(units) == ROUTE_IV))
})
test_that("a bolus gives C(0) = dose / V1", {
  PK <- getDrugPK("pramlintideIV", 77, 182, 30, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "pramlintideIV", Time = 0, Dose = 60, Units = "mcg"),
               data.frame(Time = numeric(0), Event = character(0)), PK, 300, FALSE)$wide
  expect_equal(max(w$Plasma), 60 / 19.1, tolerance = 0.01)   # mcg/L = ng/mL
})

test_that("with the switch on the parameters are anchored at Fang's 25 men", {
  # Fang 2013: 25 men, 170.4 lb (77.3 kg), 71.6 in (181.9 cm), 29.6 years.
  k <- cohortFactorsIndependent(list(weight = 77.3, height = 181.9, age = 29.6, men = 25, women = 0))
  off <- pramlintideIV(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(pramlintideIV(77.3, 181.9, 29.6, "male")$PK$default$v1, off$v1, tolerance = 1e-12)
  ref <- pramlintideIV(70, 170, 35, "male")$PK$default
  expect_equal_rounded(ref$v1, off$v1 / k$volume)
  expect_equal_rounded(ref$cl1, off$cl1 / k$clearance)
})
