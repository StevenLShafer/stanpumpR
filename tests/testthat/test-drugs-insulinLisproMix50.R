# insulinLisproMix50: see R/insulinTham.R (50% lispro protamine, 50% lispro).
test_that("the components' shares are conserved", {
  m <- insulinLisproMix50(70, 170, 35, "male", FALSE)
  pk <- m$PK$default
  zero <- m$zeroOrderSC$fraction
  expect_equal(zero, 0.5 * (1 - 0.729))
  expect_equal((1 - zero) * pk$fraction_SC_slow, 0.5)
  expect_equal((1 - zero) * (1 - pk$fraction_SC_slow), 0.5 * 0.729)
})

test_that("pregnancy: CL/F x 1.25, absorption and V/F unchanged", {
  # A woman of 30 years, 70 kg, 165 cm: CL/F x 1.25 (Catalano 1998), in
  # thamInsulin() (R/insulinTham.R, PREGNANCY); V/F and absorption unchanged.
  on  <- getDrugPK("insulinLisproMix50", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("insulinLisproMix50", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("insulinLisproMix50", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  same <- c("v1", "v2", "v3", "cl2", "cl3", "ka_RA", "bioavailability_RA",
            "tlag_RA", "ka_RAslow", "bioavailability_RAslow", "tlag_RAslow")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  expect_match(on$reference, "Catalano", fixed = TRUE)
  expect_no_match(off$reference, "Catalano", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- insulinLisproMix50(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(18.09199012, actual$v1)
  expect_equal_rounded(1, actual$v2)
  expect_equal_rounded(1, actual$v3)
  # 0.6354167 x 0.8507260, the fat-free-mass clearance factor of this woman
  # against the reference man (CL/F scaled, 2026-11-19; R/insulinTham.R)
  expect_equal_rounded(0.5405655, actual$cl1)
  expect_equal_rounded(0, actual$cl2)
  expect_equal_rounded(0, actual$cl3)
})

test_that("body size: CL/F scaled to fat-free mass from the reference man, V/F by the source's own weight term", {
  # Steven L. Shafer's first-principles rule, 2026-11-19 (R/insulinTham.R,
  # BODY SIZE): CL/F has no size term in Tham 2017 and is multiplied by the
  # fat-free-mass clearance factor, anchored at the 70 kg, 170 cm reference
  # man (Table 1 prints no sex split, so no cohort anchor).
  # (a) the reference man, the anchor, receives the published CL/F
  ref <- insulinLisproMix50(70, 170, 35, "male")$PK$default
  expect_equal(ref$cl1, 30.5 / 60)
  pk <- 70
  expect_equal(ref$v1, 43.0 * (pk / 80)^2.48)
  # (b) a man of twice the reference fat-free mass: CL/F x 2^0.75; V/F keeps
  # its own weight term at the pharmacokinetic weight, 140 kg, held at the
  # studied 86 kg
  w <- uniroot(function(w) ffmAlSallami(w, 200, 35, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- insulinLisproMix50(w, 200, 35, "male")$PK$default
  expect_equal(big$cl1, 30.5 / 60 * 2^0.75)
  pk <- 86
  expect_equal(big$v1, 43.0 * (pk / 80)^2.48)
  # (c) the switch off: CL/F as published, whatever the size
  off <- insulinLisproMix50(w, 200, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$cl1, 30.5 / 60)
})
