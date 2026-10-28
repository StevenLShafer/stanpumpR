# Warfarin: Xue 2017 S- and R-enantiomer PK driving the INR through a
# prothrombin-complex turnover model.  See the headers of R/drugs_warfarin.R
# and R/inr.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published enantiomer parameters at the standard subject", {
  # The standard subject (70 kg, 176 cm male) has Janmahasatian FFM equal to
  # the model's reference, so the size factors are 1 and the pins are the
  # published Table 3 / Table 5 values.  CYP4F2 TT is the source's reference
  # genotype (the app's default is CC; see the test below).
  actual <- warfarin(70, 176, 50, "male", cyp4f2 = "TT")
  ka <- log(2) / 0.385 / 60
  expect_equal_rounded(actual$PK$default, list(
    v1 = 24.0, v2 = 1, v3 = 1,
    cl1 = 0.224 / 60, cl2 = 0, cl3 = 0,
    ka_PO = ka, tlag_PO = 0, bioavailability_PO = 1
  ))
  expect_equal_rounded(actual$parallelSystems[[1]]$PK$default, list(
    v1 = 16.2, v2 = 1, v3 = 1,
    cl1 = 0.124 / 60, cl2 = 0, cl3 = 0,
    ka_PO = ka, tlag_PO = 0, bioavailability_PO = 1
  ))
  expect_equal(actual$doseFraction, 0.5)
  expect_equal_rounded(actual$inr, list(
    pca0 = 0.979, c50 = 0.378, hill = 2.720, ric50 = 2.387,
    halfLife = 11.4 * 60))
})

test_that("covariates act on the named parameter only", {
  base <- warfarin(70, 176, 50, "male")
  ami  <- warfarin(70, 176, 50, "male", amiodarone = TRUE)
  expect_equal(ami$PK$default$cl1, base$PK$default$cl1 * 0.760)
  expect_equal(ami$parallelSystems[[1]]$PK$default$cl1,
               base$parallelSystems[[1]]$PK$default$cl1 * 0.872)

  flu <- warfarin(70, 176, 50, "male", fluconazole = TRUE)
  expect_equal(flu$PK$default$cl1, base$PK$default$cl1 * 0.549)
  expect_equal(flu$parallelSystems[[1]]$PK$default$cl1,
               base$parallelSystems[[1]]$PK$default$cl1)   # not on CL_R

  c93 <- warfarin(70, 176, 50, "male", cyp2c9 = "*1/*3")
  expect_equal(c93$PK$default$cl1, base$PK$default$cl1 * 0.830)
  expect_equal(c93$parallelSystems[[1]]$PK$default$cl1,
               base$parallelSystems[[1]]$PK$default$cl1 * 1.230)

  bio <- warfarin(70, 176, 50, "male", bioprosthetic = TRUE)
  expect_equal(bio$PK$default$cl1, base$PK$default$cl1 * 0.775)
  expect_equal(bio$parallelSystems[[1]]$PK$default$cl1,
               base$parallelSystems[[1]]$PK$default$cl1 * 0.775)

  fem <- warfarin(70, 176, 50, "female")
  # female carries the volume terms (and the female FFM, so compare the ratio)
  expect_equal(fem$PK$default$v1 / warfarinFFM(70, 176, "female"),
               24.0 * 1.33 / WARFARIN_FFM_STD, tolerance = 1e-8)

  vk <- warfarin(70, 176, 50, "male", vkorc1 = "AA", cyp4f2 = "TT")
  expect_equal(vk$inr$c50, 0.378 * 0.725)
  c4 <- warfarin(70, 176, 50, "male", cyp4f2 = "CC")
  expect_equal(c4$inr$c50, 0.378 * 0.770)
  expect_equal(warfarin(70, 176, 50, "male", cyp4f2 = "CT")$inr$c50, 0.378 * 0.770)
})

test_that("CYP4F2 defaults to CC, the most sensitive genotype", {
  # Shafer, 2026-10-27: an unentered genotype should predict the LARGER INR
  # rise, so a dose read from the curve errs toward underdosing.
  expect_identical(CYP4F2_DEFAULT, "CC")
  expect_equal(warfarin(70, 176, 50, "male")$inr$c50, 0.378 * 0.770)
  expect_lt(warfarin(70, 176, 50, "male")$inr$c50,
            warfarin(70, 176, 50, "male", cyp4f2 = "TT")$inr$c50)
  # and it reaches the model through getDrugPK(), which validates it
  expect_equal(getDrugPK("warfarin", 70, 176, 50, "male", cyp4f2 = "TT")$inr$c50, 0.378)
  expect_error(getDrugPK("warfarin", 70, 176, 50, "male", cyp4f2 = "XX"), "Invalid cyp4f2")
})

test_that("clearance scales with fat-free mass as FFM^0.75, volume as FFM", {
  a <- warfarin(120, 180, 50, "male")
  ffm <- warfarinFFM(120, 180, "male")
  expect_equal(a$PK$default$cl1, 0.224 / 60 * (ffm / WARFARIN_FFM_STD)^0.75,
               tolerance = 1e-8)
  expect_equal(a$PK$default$v1, 24.0 * (ffm / WARFARIN_FFM_STD), tolerance = 1e-8)
  # The model's own covariate: the switch does not change it.
  off <- warfarin(120, 180, 50, "male", adjustToFFM = FALSE)
  expect_equal(off$PK$default$cl1, a$PK$default$cl1)
})

test_that("the INR starts at 1/PCA0 and rises on daily dosing", {
  dose <- data.frame(Drug = "warfarin", Time = (0:13) * 1440, Dose = 5, Units = "mg PO")
  r <- simulateDrugsWithCovariates(dose, noEvents, 70, 176, 60, "male",
                                   14 * 1440, TRUE)$warfarin
  pl <- r$results[r$results$Site == "Plasma", ]
  expect_equal(pl$Y[1], 1 / 0.979, tolerance = 1e-6)      # baseline INR
  expect_gt(max(pl$Y), 1.4)                               # rises into range
  expect_true(all(is.na(r$results$Y[r$results$Site == "Effect Site"])))  # no Ce
})

test_that("R-warfarin raises the apparent C50, lowering inhibition", {
  # The worked example is at the source's reference genotype, CYP4F2 TT.
  inr <- warfarin(70, 176, 50, "male", cyp4f2 = "TT")$inr
  # At a fixed S concentration, more R competes and reduces inhibition.
  i0 <- inrInhibition(0.5, 0,   inr)
  iR <- inrInhibition(0.5, 2.0, inr)
  expect_lt(iR, i0)
  # The paper's worked example, as the headers of R/drugs_warfarin.R and
  # R/inr.R state it: at the C_R of about 0.86 mg/L a maintenance dose gives,
  # the apparent C50 is 0.378 x (1 + 0.86 / 2.387) = 0.514 mg/L, a 36% rise.
  c50app <- inr$c50 * (1 + 0.86 / inr$ric50)
  expect_equal(c50app, 0.5142, tolerance = 1e-3)
  expect_equal(c50app / inr$c50 - 1, 0.360, tolerance = 1e-2)
  # And inrInhibition() applies exactly that apparent C50 (Hill form)
  cS <- 0.7
  expect_equal(inrInhibition(cS, 0.86, inr),
               cS^inr$hill / (c50app^inr$hill + cS^inr$hill), tolerance = 1e-12)
})

test_that("INR turnover is solved exactly against a fine numerical reference", {
  inr <- warfarin(70, 176, 50, "male")$inr
  # A ramp of inhibition over 24 h, solved on a coarse and a fine grid.
  f <- function(t) 0.6 * pmin(t / (12 * 60), 1)
  coarse <- seq(0, 24 * 60, by = 60)
  fine   <- seq(0, 24 * 60, by = 1)
  pcaCoarse <- inrPCA(coarse, f(coarse), inr)
  pcaFine   <- inrPCA(fine, f(fine), inr)
  expect_equal(tail(pcaCoarse, 1), tail(pcaFine, 1), tolerance = 1e-4)
})
