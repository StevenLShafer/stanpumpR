test_that("returns the published parameters with the switch off", {
  actual <- ropivacaine(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 7,
        v2 = 12.6,
        v3 = 28.7,
        cl1 = 0.3,
        cl2 = 0.9666666667,
        cl3 = 0.4,
        ka_RA = 0.00612,         # fitted on the published disposition (A2-F2)
        bioavailability_RA = 1,
        tlag_RA = 0
      )
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = ropivacaine(70, 171, 50, "male")$reference,
    # PPX formed from all of ropivacaine's clearance (k10 = 0.3 / 7), mass
    # for mass as Sunder 2026 fitted it, through one transit compartment
    # with its 4.9 min mean delay.
    metabolite = list(
      name = "PPX",
      kFormation = 0.3 / 7,
      firstPassFraction = 0,
      mwRatio = 1,
      kTransit = 1 / 4.9
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # The pins from before the anchoring, divided by Simon's cohort factors.
  kc <- cohortFactorsIndependent(ROPIVACAINE_COHORT)
  actual <- ropivacaine(120, 170, 50, "male")
  expected <- list(v1 = 9.1343469 / kc$volume, v2 = 16.44182442 / kc$volume,
                   v3 = 37.45082229 / kc$volume,
                   cl1 = 0.36627378 / kc$clearance, cl2 = 1.180215513 / kc$clearance,
                   cl3 = 0.48836504 / kc$clearance)
  expect_equal(actual$PK$default[names(expected)], expected, tolerance = 1e-7)
})

test_that("anchors at Simon's oldest group and divides the 120 kg man by it", {
  # The published values belong to Simon 2006's group 3 (85 kg, 178 cm, 74
  # y, 8 men, 1 woman); the factors are recomputed by hand here.
  kc <- cohortFactorsIndependent(ROPIVACAINE_COHORT)
  expect_equal(kc$volume, 1.134251, tolerance = 1e-6)
  expect_equal(kc$clearance, 1.099086, tolerance = 1e-6)
  r <- ropivacaine(85, 178, 74, "male")$PK$default
  m <- ffmRatioMaleIndependent(85, 178, 74)
  expect_equal(r$v1, 7 * m / kc$volume, tolerance = 1e-10)
  expect_equal(r$cl1, 0.3 * m^0.75 / kc$clearance, tolerance = 1e-10)
})

test_that("ka_RA reproduces Vainionpaa's 1.28 mcg/mL peak in that study's typical patient", {
  # Vainionpaa 1995's pharmacokinetic subgroup (75 kg, 168 cm, 37 y, 6 men,
  # 4 women) has the anchored disposition scaled by its cohort factors over
  # Simon's; 175 mg of plain ropivacaine peaks at 1.28 mcg/mL.
  p <- ropivacaine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  kv <- cohortFactorsIndependent(ROPIVACAINE_KA_COHORT)
  ks <- cohortFactorsIndependent(ROPIVACAINE_COHORT)
  set <- p
  for (n in c("v1", "v2", "v3")) set[[n]] <- p[[n]] * kv$volume / ks$volume
  for (n in c("cl1", "cl2", "cl3")) set[[n]] <- p[[n]] * kv$clearance / ks$clearance
  ka <- ropivacaine(70, 170, 35, "male")$PK$default$ka_RA   # the switch on
  peak <- depotPeakReference(set, 175, ka)
  expect_equal(peak[["peak"]], 1.28, tolerance = 1e-4)
  expect_equal(peak[["time"]], 133.3, tolerance = 2e-3)
  # With the switch off everyone has the published disposition, on which the
  # switch-off rate was fitted to the same peak (Carbon's second repository
  # audit, A2-F2)
  peak <- depotPeakReference(p, 175, p$ka_RA)
  expect_equal(peak[["peak"]], 1.28, tolerance = 1e-3)
})
