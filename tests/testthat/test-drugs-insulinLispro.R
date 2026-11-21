# insulinLispro: see R/insulinTham.R (Tham 2017, Table 2; zero-order routing assumed).
test_that("72.9% through a lagged depot, 27.1% zero-order over 1.06 h", {
  m <- insulinLispro(80, 175, 35, "male", FALSE)
  pk <- m$PK$default
  expect_equal(pk$ka_SC, 0.989 / 60); expect_equal(pk$tlag_SC, 0.265 * 60)
  expect_equal(pk$v1, 43.0); expect_equal(pk$cl1, 30.5 / 60)
  expect_equal(m$zeroOrderSC$fraction, 1 - 0.729)
  expect_equal(m$zeroOrderSC$duration, 1.06 * 60)
  expect_equal(m$zeroOrderSC$into, "central")
  p <- getDrugPK("insulinLispro", 70, 170, 35, "male", adjustToFFM = FALSE)
  expect_equal(p$PK$default$bioavailability_RA, 0.729)
  # weight on V/F, (BW/80)^2.48, held within the studied 52.6 to 86 kg
  expect_equal(insulinLispro(70, 170, 35, "male", FALSE)$PK$default$v1, 43 * (70 / 80)^2.48)
  expect_equal(insulinLispro(120, 170, 35, "male", FALSE)$PK$default$v1, 43 * (86 / 80)^2.48)
  expect_equal(insulinLispro(40, 150, 35, "male", FALSE)$PK$default$v1, 43 * (52.6 / 80)^2.48)
  expect_false(insulinLispro(70, 170, 35, "male")$doseDependent)
})
test_that("mass balance: AUC of an injection is dose / CL", {
  PK <- getDrugPK("insulinLispro", 70, 170, 35, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "insulinLispro", Time = 0, Dose = 10, Units = "U SC"),
               data.frame(Time = numeric(0), Event = character(0)), PK, 2880, FALSE)$wide
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60  # mcU.h/mL
  expect_equal(auc, 10000 / 30.5, tolerance = 0.01)
})
test_that("lispro alone offers the pump", {
  expect_true("U/hr SC" %in% getDrugDefaults("insulinLispro")$Units[[1]])
})

test_that("pregnancy: CL/F x 1.25, absorption and V/F unchanged", {
  # A woman of 30 years, 70 kg, 165 cm: CL/F x 1.25 (Catalano 1998), in
  # thamInsulin() (R/insulinTham.R, PREGNANCY); V/F and absorption unchanged.
  on  <- getDrugPK("insulinLispro", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("insulinLispro", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("insulinLispro", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  same <- c("v1", "v2", "v3", "cl2", "cl3", "ka_RA", "bioavailability_RA",
            "tlag_RA")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  expect_match(on$reference, "Catalano", fixed = TRUE)
  expect_no_match(off$reference, "Catalano", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- insulinLispro(70, 165, 30, "female", pregnant = TRUE)$PK$default
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
  ref <- insulinLispro(70, 170, 35, "male")$PK$default
  expect_equal(ref$cl1, 30.5 / 60)
  pk <- 70
  expect_equal(ref$v1, 43.0 * (pk / 80)^2.48)
  # (b) a man of twice the reference fat-free mass: CL/F x 2^0.75; V/F keeps
  # its own weight term at the pharmacokinetic weight, 140 kg, held at the
  # studied 86 kg
  w <- uniroot(function(w) ffmAlSallami(w, 200, 35, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- insulinLispro(w, 200, 35, "male")$PK$default
  expect_equal(big$cl1, 30.5 / 60 * 2^0.75)
  pk <- 86
  expect_equal(big$v1, 43.0 * (pk / 80)^2.48)
  # (c) the switch off: CL/F as published, whatever the size
  off <- insulinLispro(w, 200, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$cl1, 30.5 / 60)
})
