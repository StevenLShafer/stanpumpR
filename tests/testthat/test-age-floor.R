# The adult-fitted age terms see an age floored at ADULT_AGE_TERM_FLOOR (18):
# apixaban, rivaroxaban, fondaparinux and paroxetine carry age as a negative
# power, and egfrMDRD() (enoxaparin, fondaparinux) does too.  An age of 0 is a
# legal entry (MIN_AGE), and before the floor it made these models infinite,
# which left the eigenvalue solver with NaN and stopped the whole simulation.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("ADULT_AGE_TERM_FLOOR is 18, as buprenorphine's own floor is", {
  expect_equal(ADULT_AGE_TERM_FLOOR, 18)
  expect_equal(BUPRENORPHINE_MIN_AGE, ADULT_AGE_TERM_FLOOR)
})

test_that("the age terms are flat below 18 with the switch off", {
  # With the switch off the pharmacokinetic weight is the weight, so the only
  # age-dependent parts of these parameters are the floored terms (and, for
  # apixaban, Cockcroft-Gault's 140 - age, so its renal part is excluded).
  expect_equal(rivaroxaban(70, 170, 0, "male", adjustToFFM = FALSE)$PK$default$v1,
               rivaroxaban(70, 170, 18, "male", adjustToFFM = FALSE)$PK$default$v1)
  expect_equal(fondaparinux(70, 170, 0, "male", adjustToFFM = FALSE)$PK$default$cl1,
               fondaparinux(70, 170, 18, "male", adjustToFFM = FALSE)$PK$default$cl1)
  expect_equal(paroxetine(70, 170, 0, "male", adjustToFFM = FALSE)$PK$default$cl1,
               paroxetine(70, 170, 18, "male", adjustToFFM = FALSE)$PK$default$cl1)
  expect_lt(rivaroxaban(70, 170, 19, "male", adjustToFFM = FALSE)$PK$default$v1,
            rivaroxaban(70, 170, 18, "male", adjustToFFM = FALSE)$PK$default$v1)
})

test_that("a newborn can be simulated on every model that carries such a term", {
  for (drug in c("apixaban", "rivaroxaban", "fondaparinux", "enoxaparin", "paroxetine")) {
    pk <- getDrugPK(drug, 3.5, 50, 0, "male")
    # A missing compartment (v = 1, cl = 0) gives a zero eigenvalue by design;
    # none may be infinite or NaN, and the fastest must be positive.
    lambdas <- unlist(pk$PK$default[c("lambda_1", "lambda_2", "lambda_3")])
    expect_true(all(is.finite(lambdas)) && all(lambdas >= 0) && lambdas[1] > 0, info = drug)
    units <- getDrugDefaults(drug)$Default.Units
    dose <- data.frame(Drug = drug, Time = 0, Dose = 1, Units = units)
    out <- simulateDrugsWithCovariates(dose, noEvents, 3.5, 50, 0, "male", 1440, FALSE)
    expect_true(all(is.finite(out[[drug]]$wide$Plasma)), info = drug)
    expect_gt(max(out[[drug]]$wide$Plasma), 0, label = drug)
  }
})
