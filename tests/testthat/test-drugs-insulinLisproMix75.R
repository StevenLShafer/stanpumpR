# insulinLisproMix75: see R/insulinTham.R (75% lispro protamine, 25% lispro).
test_that("the components' shares are conserved", {
  m <- insulinLisproMix75(70, 170, 35, "male", FALSE)
  pk <- m$PK$default
  zero <- m$zeroOrderSC$fraction
  expect_equal(zero, 0.25 * (1 - 0.729))
  fast <- (1 - zero) * (1 - pk$fraction_SC_slow)
  slow <- (1 - zero) * pk$fraction_SC_slow
  expect_equal(fast, 0.25 * 0.729)
  expect_equal(slow, 0.75)
  expect_equal(pk$ka_SC_slow, 0.0365 / 60); expect_equal(pk$tlag_SC_slow, 0)
  expect_equal(pk$tlag_SC, 0.265 * 60)
})
test_that("10 U of Mix75/25 equals 2.5 U lispro plus 7.5 U protamine-only", {
  noEvents <- data.frame(Time = numeric(0), Event = character(0))
  run <- function(drug, dose, model = NULL) {
    PK <- if (is.null(model)) getDrugPK(drug, 70, 170, 35, "male") else
      getDrugPK(drug, 70, 170, 35, "male", model = model)
    simCpCe(data.frame(Drug = drug, Time = 0, Dose = dose, Units = "U SC"), noEvents, PK, 1440, FALSE)$wide
  }
  protamine <- thamInsulin(70, 170, 35, "male", TRUE, "lispro", c(lisproProtamine = 1), "test")
  mix <- run("insulinLisproMix75", 10)
  lis <- run("insulinLispro", 2.5)
  pro <- run("insulinLispro", 7.5, protamine)
  t <- intersect(intersect(mix$Time, lis$Time), pro$Time)
  at <- function(w) w$Plasma[match(t, w$Time)]
  expect_gt(length(t), 10)
  expect_equal(at(mix), at(lis) + at(pro), tolerance = 1e-9)
})

test_that("pregnancy: CL/F x 1.25, absorption and V/F unchanged", {
  # A woman of 30 years, 70 kg, 165 cm: CL/F x 1.25 (Catalano 1998), in
  # thamInsulin() (R/insulinTham.R, PREGNANCY); V/F and absorption unchanged.
  on  <- getDrugPK("insulinLisproMix75", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("insulinLisproMix75", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("insulinLisproMix75", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  same <- c("v1", "v2", "v3", "cl2", "cl3", "ka_RA", "bioavailability_RA",
            "tlag_RA", "ka_RAslow", "bioavailability_RAslow", "tlag_RAslow")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  expect_match(on$reference, "Catalano", fixed = TRUE)
  expect_no_match(off$reference, "Catalano", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- insulinLisproMix75(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(18.09199012, actual$v1)
  expect_equal_rounded(1, actual$v2)
  expect_equal_rounded(1, actual$v3)
  expect_equal_rounded(0.6354167, actual$cl1)
  expect_equal_rounded(0, actual$cl2)
  expect_equal_rounded(0, actual$cl3)
})
