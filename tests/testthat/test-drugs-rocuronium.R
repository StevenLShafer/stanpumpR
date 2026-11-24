# rocuronium: Kleijn 2011's rocuronium arm; see the header of
# R/drugs_rocuronium.R.  Pins were worked out from Table 4, the Al-Sallami
# fat-free mass and Cockcroft-Gault by hand (Python), not from the code
# under test.

test_that("returns Kleijn's parameters with total-body-weight scaling", {
  # 70 kg, 171 cm, 50 y man, switch off: W = 70 kg, assumed creatinine
  # 0.9 mg/dL, Cockcroft-Gault 97.22 mL/min.
  actual <- rocuronium(70, 171, 50, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.8796204,
        v2 = 7.0563851,
        v3 = 1,
        cl1 = 0.25623326,
        cl2 = 0.279,
        cl3 = 0
      )
    ),
    tPeak = 4.5,
    MEAC = 0,
    typical = 1.5,
    upperTypical = 2.2,
    lowerTypical = 1,
    reference = actual$reference
  )

  expect_equal_rounded(actual, expected)
  expect_match(actual$reference, "Kleijn HJ et al., Br J Clin Pharmacol 2011;72:415-433",
               fixed = TRUE)
  expect_match(actual$reference, "Cortinez", fixed = TRUE)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y man: FFM 71.09 kg against the 54.48 kg reference,
  # so the allometric terms see the pharmacokinetic weight 91.34 kg, as
  # sugammadex's do; Cockcroft-Gault runs at that weight too.
  # v3 = 1 and cl3 = 0 are placeholders for a missing compartment.
  actual <- rocuronium(120, 170, 50, "male")
  expected <- list(
    v1 = 6.1031708,
    v2 = 9.2079238,
    v3 = 1,
    cl1 = 0.31283842,
    cl2 = 0.34063462,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the reference man receives Kleijn's values in both switch positions", {
  # 70 kg, 170 cm, 35 y: Cockcroft-Gault 113.43 mL/min at 0.9 mg/dL.
  # CL 0.2836 L/min = 4.05 mL/kg/min, against Plaud's 0.684 (9.78 mL/kg/min).
  expected <- list(v1 = 4.7678531, v2 = 6.4364870, cl1 = 0.28359056, cl2 = 0.279)
  for (ffm in c(TRUE, FALSE))
    expect_equal_rounded(rocuronium(70, 170, 35, "male", adjustToFFM = ffm)$PK$default[names(expected)],
                         expected)
})

test_that("an entered creatinine reaches the central volume, and a woman is scaled by her fat-free mass", {
  # 60 kg, 160 cm, 70 y woman, creatinine 1.2 mg/dL, switch on: W 49.70 kg,
  # Cockcroft-Gault 34.23 mL/min.  No sex term of its own (none found, p. 423).
  actual <- rocuronium(60, 160, 70, SEX_FEMALE, creatinine = 1.2)
  expected <- list(v1 = 3.7910798, v2 = 5.6634407, cl1 = 0.16997405, cl2 = 0.21579650)
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
  # Creatinine clearance enters only V1
  blank <- rocuronium(60, 160, 70, SEX_FEMALE)$PK$default
  expect_equal(blank[c("v2", "cl1", "cl2")], actual$PK$default[c("v2", "cl1", "cl2")])
  expect_false(isTRUE(all.equal(blank$v1, actual$PK$default$v1)))
})

test_that("the age term floors at the youngest age Kleijn studied, 1 year", {
  expect_equal(ROCURONIUM_KLEIJN_MIN_AGE, 1)
  at0 <- rocuronium(10, 75, 0, "male", adjustToFFM = FALSE)$PK$default
  at1 <- rocuronium(10, 75, 1, "male", adjustToFFM = FALSE)$PK$default
  # Cockcroft-Gault itself reads the true age, so only the age terms are
  # compared: clearance and peripheral volume
  expect_equal(at0[c("cl1", "v2", "cl2")], at1[c("cl1", "v2", "cl2")])
  # As printed, clearance rises below 43 years: 28.5% at 1 year
  expect_equal(at1$cl1 / (0.269 * (10 / 70)^0.75), 1 + 0.00678 * 42, tolerance = 1e-12)
  # and stays positive at the oldest legal age
  expect_gt(rocuronium(70, 170, MAX_AGE, "male")$PK$default$cl1, 0)
})

test_that("creatinine clearance is held within the range Kleijn studied", {
  # A 500 kg man with a creatinine of 0.2 mg/dL: Cockcroft-Gault in the
  # thousands, held at 239 mL/min, so V1 is not driven towards zero.
  big <- rocuronium(500, 180, 20, "male", adjustToFFM = FALSE, creatinine = 0.2)$PK$default
  expect_equal(big$v1, 4.73 * exp(-0.00143 * (239 - 119)) * 500 / 70, tolerance = 1e-12)
  # and a creatinine clearance below 4.30 mL/min is held at 4.30
  small <- rocuronium(40, 160, 89, SEX_FEMALE, adjustToFFM = FALSE, creatinine = 15)$PK$default
  expect_equal(small$v1, 4.73 * exp(-0.00143 * (4.30 - 119)) * 40 / 70, tolerance = 1e-12)
})

test_that("rocuronium and sugammadex read one Kleijn patient", {
  # The same paper's two drugs see the same weight and creatinine clearance
  for (ffm in c(TRUE, FALSE)) {
    p <- kleijn2011Patient(95, 175, 62, SEX_FEMALE, ffm, 1.1)
    size <- pkSizeFactors(95, 175, 62, SEX_FEMALE, ffm)
    expect_equal(p$W, 70 * size$volume)
    expect_equal(p$CR, creatinineClearanceCG(p$W, 62, SEX_FEMALE, 1.1))
    roc <- rocuronium(95, 175, 62, SEX_FEMALE, ffm, creatinine = 1.1)$PK$default
    expect_equal(roc$cl2, 0.279 * (p$W / 70)^0.75)
    sug <- sugammadex(95, 175, 62, SEX_FEMALE, ffm, creatinine = 1.1)$PK$default
    expect_equal(sug$cl2, 12.36 * (p$W / 70)^0.75 / 60)
  }
})

test_that("the time to peak effect is Cortinez's median, and its ke0 on Kleijn's kinetics", {
  # Cortinez 2007: median time to maximum block 4.5 min at the adductor
  # pollicis.  On Kleijn's slower-falling plasma curve that needs a ke0 of
  # about 0.40 /min, against Cortinez's own 0.19-0.20 and Kleijn's 0.134
  # (T4/T1 ratio, no sevoflurane), which on this plasma model would put the
  # peak at about 8.6 min; see EFFECT SITE in the header.
  defaults <- getDrugDefaultsGlobal()
  PK <- getDrugPK("rocuronium", 70, 170, 35, "male", defaults[defaults$Drug == "rocuronium", ])
  expect_equal(PK$tPeak, 4.5)
  expect_gt(PK$PK$default$ke0, 0.38)
  expect_lt(PK$PK$default$ke0, 0.41)
})
