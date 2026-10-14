# diamorphine: see the header of R/drugs_diamorphine.R for what the model is
# (Cai 2025 parent diamorphine plasma, IN/IM, metabolites not modelled) and
# what it leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand, not from the code under test.

test_that("returns the published parent parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- diamorphine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 8.21,
        v2 = 1,
        v3 = 1,
        cl1 = 14.09383333,   # K12 * V1 / 60 = 103 * 8.21 / 60
        cl2 = 0,
        cl3 = 0,
        ka_IM = 0.050666667, # 3.04 / 60
        bioavailability_IM = 1,
        tlag_IM = 0,
        ka_IN = 0.050666667,
        bioavailability_IN = 0.519,
        tlag_IN = 0
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales with Cai's allometry to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: the pharmacokinetic (fat-free-mass) weight is
  # 91.3435 kg (pkSizeFactors()).  Volumes x (pkWeight/70)^1, clearance x
  # (pkWeight/70)^0.75, first-order rates x (pkWeight/70)^-0.25.
  actual <- diamorphine(120, 170, 50, "male")
  expected <- list(
    v1 = 10.71328356,
    v2 = 1,
    v3 = 1,
    cl1 = 17.20733896,
    cl2 = 0,
    cl3 = 0,
    ka_IM = 0.047405363,
    ka_IN = 0.047405363
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the parent is one-compartment: diamorphine's only loss is conversion to 6-MAM", {
  x <- diamorphine(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # cl1 = K12 * v1, so the elimination rate constant is exactly Cai's K12
  expect_equal(x$cl1 / x$v1 * 60, 103)            # 1/h
  expect_equal(x$v2, 1); expect_equal(x$v3, 1)
  expect_equal(x$cl2, 0); expect_equal(x$cl3, 0)
  # IM is the bioavailability reference; IN is relative to it
  expect_equal(x$bioavailability_IM, 1)
  expect_equal(x$bioavailability_IN, 0.519)
  # Plasma only: no effect site in the model
  PK <- getDrugPK("diamorphine", 70, 171, 50, "male")
  expect_equal(PK$PK$default$ke0, 0)
})

test_that("diamorphine is offered intranasally and intramuscularly only, plasma only", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "diamorphine"], ",")[[1]]
  expect_setequal(doseRoute(units), c(ROUTE_IN, ROUTE_IM))
  expect_true(is.na(dd$Bolus.Units[dd$Drug == "diamorphine"]))
  # No band, no MEAC, no recovery threshold
  expect_equal(dd$MEAC[dd$Drug == "diamorphine"], 0)
  expect_equal(dd$endCe[dd$Drug == "diamorphine"], 0)
})
