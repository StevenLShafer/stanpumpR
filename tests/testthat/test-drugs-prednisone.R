# prednisone: see the header of R/drugs_prednisone.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- prednisone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 99.325,
        v2 = 17.187343,
        v3 = 1,
        cl1 = 0.61388655,
        cl2 = 0.18461345,
        cl3 = 0,
        ka_PO = 0.018,
        bioavailability_PO = 0.42029304,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 30, upperTypical = 80, lowerTypical = 10,
    reference = actual$reference,
    metabolite = list(
      name              = "prednisolone",
      kFormation        = 0.002923232,
      firstPassFraction = 0.4958758,
      mwRatio           = 360.44 / 358.43
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- prednisone(120, 170, 50, "male")
  expected <- list(
    v1 = 129.60985,
    v2 = 22.427878,
    v3 = 1,
    cl1 = 0.74950184,
    cl2 = 0.22539689,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("prednisone is a prodrug whose row is total prednisone", {
  x <- prednisone(70, 171, 50, "male", adjustToFFM = FALSE)
  # Total = free / 0.25, so volumes and clearances are a quarter of the free ones
  sys <- prednisolonePairSystem()
  expect_equal(x$PK$default$v1, sys$PN$v1 * 0.25)
  expect_equal(x$PK$default$cl1 * 60, sys$PN$cl1 * 0.25)
  expect_equal(x$metabolite$name, "prednisolone")
  expect_equal(x$metabolite$mwRatio, 360.44 / 358.43)
  PK <- getDrugPK("prednisone", 70, 171, 50, "male")
  expect_equal(PK$PK$default$ke0, 0)
  expect_equal(PK$metaboliteName, "prednisolone")
})

test_that("prednisone is oral only", {
  dd <- getDrugDefaultsGlobal(FALSE)
  expect_true(all(grepl("PO", strsplit(dd$Units[dd$Drug == "prednisone"], ",")[[1]])))
})

test_that("oral prednisone forms prednisolone on the prednisolone row", {
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "prednisone", Time = 0, Dose = 20, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 1440, FALSE)
  expect_equal(o$prednisolone$formedFrom, "prednisone")
  pl <- o$prednisolone$wide
  pn <- o$prednisone$wide
  expect_gt(max(pl$Plasma, na.rm = TRUE), 30)      # free prednisolone, ng/mL
  expect_gt(max(pn$Plasma, na.rm = TRUE), 20)      # total prednisone, ng/mL
  # Free prednisolone exposure exceeds total prednisone exposure (exact
  # ratio 255/228 from the source identities; the plotted grid is coarse)
  trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)
  expect_gt(trapz(pl$Time, pl$Plasma), trapz(pn$Time, pn$Plasma))
  # No effect site on the prodrug row
  expect_true(all(is.na(pn$"Effect Site")))
})

test_that("exposures after oral prednisone match the source's AUC identities", {
  # Closed-form AUCs from the coefficients, not the plotted grid.  A unit
  # oral dose of prednisone (mass units) gives total prednisone AUC
  # 4 x 0.00285268 x unit and free prednisolone AUC 0.01269946 x (360.44/358.43)
  # x unit, from solving the two-species AUC identity.
  PK <- getDrugPK("prednisone", 70, 170, 35, "male", getDrugDefaults("prednisone"))$PK$default
  aucPN <- PK$p_coef_PO_l1 / PK$lambda_1 + PK$p_coef_PO_l2 / PK$lambda_2 + PK$p_coef_PO_ka / PK$ka_PO
  expect_equal(aucPN / 60, 4 * 0.00285268, tolerance = 1e-5)      # min/L -> h/L
  m <- PK$metabolite$coefs
  aucPL <- sum(m$PO / m$lambda)
  expect_equal(aucPL / 60, 0.01269946 * 360.44 / 358.43, tolerance = 1e-5)
})
