# insulinHumanMix70: see R/insulinTham.R (Tham 2017: 7 U NPH + 3 U regular per 10 U).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
sim <- function(drug, dose) {
  PK <- getDrugPK(drug, 70, 170, 35, "male")
  simCpCe(data.frame(Drug = drug, Time = 0, Dose = dose, Units = "U SC"), noEvents, PK, 1440, FALSE)
}
test_that("two depots: regular fast with no lag, NPH slow with its lag", {
  pk <- insulinHumanMix70(80, 175, 35, "male", FALSE)$PK$default
  expect_equal(pk$ka_SC, 0.67 / 60); expect_equal(pk$tlag_SC, 0)
  expect_equal(pk$ka_SC_slow, 0.129 / 60); expect_equal(pk$tlag_SC_slow, 0.378 * 60)
  expect_equal(pk$fraction_SC_slow, 0.7)
})
test_that("10 U of 70/30 is 7 U NPH plus 3 U regular, cleared as one 10 U dose", {
  PK <- getDrugPK("insulinHumanMix70", 80, 175, 40, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "insulinHumanMix70", Time = 0, Dose = 10, Units = "U SC"),
               noEvents, PK, 1440, FALSE)$wide
  # CL/F at the whole human-insulin dose; regular ka at its 3 U share
  k <- 127 / 60 / 178
  bateman <- function(D, ka, lag, t) {
    s <- pmax(t - lag, 0)
    D * 1000 * ka / (178 * (ka - k)) * (exp(-k * s) - exp(-ka * s))
  }
  exact <- bateman(7, 0.129 / 60, 0.378 * 60, w$Time) +
    bateman(3, 0.67 * 0.3^-0.275 / 60, 0, w$Time)
  expect_equal(w$Plasma, exact, tolerance = 1e-9)
})

test_that("pregnancy: CL/F x 1.25, absorption and V/F unchanged", {
  # A woman of 30 years, 70 kg, 165 cm: CL/F x 1.25 (Catalano 1998), in
  # thamInsulin() (R/insulinTham.R, PREGNANCY); V/F and absorption unchanged.
  on  <- getDrugPK("insulinHumanMix70", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("insulinHumanMix70", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("insulinHumanMix70", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  same <- c("v1", "v2", "v3", "cl2", "cl3", "ka_RA", "bioavailability_RA",
            "tlag_RA", "ka_RAslow", "bioavailability_RAslow", "tlag_RAslow")
  expect_true(all(same %in% names(a)))
  expect_identical(a[same], b[same])
  # Each dose size is its own system (doseSizePK()); the flag reaches it
  expect_equal(doseSizePK(on, 40)$PK[[1]]$cl1 / doseSizePK(off, 40)$PK[[1]]$cl1,
               INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_match(on$reference, "Catalano", fixed = TRUE)
  expect_no_match(off$reference, "Catalano", fixed = TRUE)
  # The values the model gives with the flag on
  actual <- insulinHumanMix70(70, 165, 30, "female", pregnant = TRUE)$PK$default
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
  ref <- insulinHumanMix70(70, 170, 35, "male")$PK$default
  expect_equal(ref$cl1, 127 / 60)
  pk <- 70
  expect_equal(ref$v1, 178 * (pk / 80)^1.62)
  # (b) a man of twice the reference fat-free mass: CL/F x 2^0.75; V/F keeps
  # its own weight term at the pharmacokinetic weight, 140 kg, held at the
  # studied 124 kg
  w <- uniroot(function(w) ffmAlSallami(w, 200, 35, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- insulinHumanMix70(w, 200, 35, "male")$PK$default
  expect_equal(big$cl1, 127 / 60 * 2^0.75)
  pk <- 124
  expect_equal(big$v1, 178 * (pk / 80)^1.62)
  # (c) the switch off: CL/F as published, whatever the size
  off <- insulinHumanMix70(w, 200, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$cl1, 127 / 60)
})
