# insulinRegular: see R/insulinTham.R (Tham 2017, Table 2 and the
# supplement's equations).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
test_that("Table 2 at the reference weight and dose, with Tham's covariates", {
  ref <- insulinRegular(80, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(ref$v1, 178); expect_equal(ref$cl1, 127 / 60)
  expect_equal(ref$ka_SC, 0.67 / 60); expect_equal(ref$tlag_SC, 0)
  heavy <- insulinRegular(110, 175, 40, "male", FALSE)$PK$default
  expect_equal(heavy$v1, 178 * (110 / 80)^1.62)
  expect_equal(insulinRegular(150, 175, 40, "male", FALSE)$PK$default$v1,
               178 * (124 / 80)^1.62)                  # held at the studied 124 kg
  d30 <- insulinRegular(80, 175, 40, "male", FALSE, dose = 30)$PK$default
  expect_equal(d30$cl1, 127 * 3^-0.282 / 60)
  expect_equal(d30$ka_SC, 0.67 * 3^-0.275 / 60)
  expect_equal(insulinRegular(80, 175, 40, "male", FALSE, dose = 500)$PK$default$cl1,
               127 * 10^-0.282 / 60)                   # held at the studied 100 U
  # switch on: the pharmacokinetic weight
  size <- pkSizeFactors(100, 175, 40, "male", TRUE)
  expect_equal(insulinRegular(100, 175, 40, "male")$PK$default$v1,
               178 * (size$pkWeight / 80)^1.62)
  expect_null(insulinRegular(80, 175, 40, "male")$zeroOrderSC)
  expect_true(insulinRegular(80, 175, 40, "male")$doseDependent)
})
test_that("each injection is simulated with the parameters for its size", {
  PK <- getDrugPK("insulinRegular", 80, 175, 40, "male", adjustToFFM = FALSE)
  bateman <- function(D, t0, t) {
    ka <- 0.67 * (D / 10)^-0.275 / 60; k <- 127 * (D / 10)^-0.282 / 60 / 178
    s <- pmax(t - t0, 0)
    D * 1000 * ka / (178 * (ka - k)) * (exp(-k * s) - exp(-ka * s))
  }
  w <- simCpCe(data.frame(Drug = "insulinRegular", Time = c(0, 240), Dose = c(10, 30),
                          Units = "U SC"), noEvents, PK, 1440, FALSE)$wide
  expect_equal(w$Plasma, bateman(10, 0, w$Time) + bateman(30, 240, w$Time), tolerance = 1e-9)
  # 10 U, the reference dose: a peak of about 20 mcU/mL some 87 minutes on
  one <- simCpCe(data.frame(Drug = "insulinRegular", Time = 0, Dose = 10, Units = "U SC"),
                 noEvents, PK, 600, FALSE)$wide
  expect_equal(max(one$Plasma), 20.0, tolerance = 0.01)
  # U/kg doses are sized in U: 0.25 U/kg at 80 kg is 20 U
  kg <- simCpCe(data.frame(Drug = "insulinRegular", Time = 0, Dose = 0.25, Units = "U/kg SC"),
                noEvents, PK, 600, FALSE)$wide
  expect_equal(kg$Plasma, bateman(20, 0, kg$Time), tolerance = 1e-9)
  units <- getDrugDefaults("insulinRegular")$Units[[1]]
  expect_false(any(units %in% c(insulinRateUnits, bolusUnits, infusionUnits)))
})

test_that("pregnancy: CL/F x 1.25, absorption and V/F unchanged", {
  # A woman of 30 years, 70 kg, 165 cm: CL/F x 1.25 (Catalano 1998), in
  # thamInsulin() (R/insulinTham.R, PREGNANCY); V/F and absorption unchanged.
  on  <- getDrugPK("insulinRegular", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("insulinRegular", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("insulinRegular", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  same <- c("v1", "v2", "v3", "cl2", "cl3", "ka_RA", "bioavailability_RA",
            "tlag_RA")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  # Each dose size is its own system (doseSizePK()); the flag reaches it
  expect_equal(doseSizePK(on, 40)$PK[[1]]$cl1 / doseSizePK(off, 40)$PK[[1]]$cl1,
               INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_match(on$reference, "Catalano", fixed = TRUE)
  expect_no_match(off$reference, "Catalano", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- insulinRegular(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(101.1157443, actual$v1)
  expect_equal_rounded(1, actual$v2)
  expect_equal_rounded(1, actual$v3)
  # 2.645833 x 0.8507260, the fat-free-mass clearance factor of this woman
  # against the reference man (CL/F scaled, 2026-11-19; R/insulinTham.R)
  expect_equal_rounded(2.250879, actual$cl1)
  expect_equal_rounded(0, actual$cl2)
  expect_equal_rounded(0, actual$cl3)
})

test_that("body size: CL/F scaled to fat-free mass from the reference man, V/F by the source's own weight term", {
  # Steven L. Shafer's first-principles rule, 2026-11-19 (R/insulinTham.R,
  # BODY SIZE): CL/F has no size term in Tham 2017 and is multiplied by the
  # fat-free-mass clearance factor, anchored at the 70 kg, 170 cm reference
  # man (Table 1 prints no sex split, so no cohort anchor).
  # (a) the reference man, the anchor, receives the published CL/F
  ref <- insulinRegular(70, 170, 35, "male")$PK$default
  expect_equal(ref$cl1, 127 / 60)
  pk <- 70
  expect_equal(ref$v1, 178 * (pk / 80)^1.62)
  # (b) a man of twice the reference fat-free mass: CL/F x 2^0.75; V/F keeps
  # its own weight term at the pharmacokinetic weight, 140 kg, held at the
  # studied 124 kg
  w <- uniroot(function(w) ffmAlSallami(w, 200, 35, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- insulinRegular(w, 200, 35, "male")$PK$default
  expect_equal(big$cl1, 127 / 60 * 2^0.75)
  # the dose term is kept, and each dose size is rebuilt with both
  expect_equal(insulinRegular(w, 200, 35, "male", dose = 30)$PK$default$cl1,
               127 * 3^-0.282 / 60 * 2^0.75)
  PK <- getDrugPK("insulinRegular", w, 200, 35, "male")
  expect_equal(doseSizePK(PK, 30)$PK[[1]]$cl1, 127 * 3^-0.282 / 60 * 2^0.75)
  pk <- 124
  expect_equal(big$v1, 178 * (pk / 80)^1.62)
  # (c) the switch off: CL/F as published, whatever the size
  off <- insulinRegular(w, 200, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$cl1, 127 / 60)
})
