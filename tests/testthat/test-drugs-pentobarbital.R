# pentobarbital: see R/drugs_pentobarbital.R (J Clin Pharmacol 2026, 2C IV).
test_that("reference 70 kg, switch off", {
  a <- pentobarbital(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 37.4); expect_equal(a$PK$default$v2, 63.9)
  expect_equal(a$PK$default$cl1, 5.21 / 60); expect_equal(a$PK$default$cl2, 18.1 / 60)
})
test_that("allometric scaling, switch on", {
  a <- pentobarbital(120, 170, 50, "male", adjustToFFM = TRUE)
  w <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  expect_equal(a$PK$default$cl1, 5.21 * (w / 70)^0.75 / 60)
  expect_equal(a$PK$default$v1, 37.4 * (w / 70))
})
test_that("band is Helfer's 4-21 mg/L for status epilepticus, and the seizure regimen lands in it", {
  # Drawn by decision of Steven L. Shafer, 2026-10-12 (applied 2026-10-28);
  # the registry, the CSV and the function must agree.
  a <- pentobarbital(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(c(a$lowerTypical, a$typical, a$upperTypical), c(4, 12.5, 21))
  r <- therapeuticRange("pentobarbital")
  expect_identical(r$band_status, "drawn")
  expect_equal(c(r$lower, r$typical, r$upper), c(4, 12.5, 21))
  expect_match(r$sampling, "status epilepticus", fixed = TRUE)
  # Helfer's 5 mg/kg then 1 mg/kg/h at 70 kg: 10.4 mg/L at 24 h by an
  # independent integration (steady state 70 / 5.21 = 13.4 mg/L).
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "pentobarbital", Time = c(0, 0), Dose = c(350, 70),
               Units = c("mg", "mg/hr")),
    data.frame(Time = numeric(0), Event = character(0), Fill = character(0)),
    70, 170, 40, "male", 1440, TRUE, adjustToFFM = FALSE)$pentobarbital$wide
  at24 <- approx(w$Time, w$Plasma, 1440)$y
  expect_gt(at24, 10.2); expect_lt(at24, 10.6)
})
