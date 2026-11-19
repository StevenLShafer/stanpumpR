# liraglutideOvergaard: see R/drugs_liraglutideOvergaard.R (Overgaard 2016,
# Online Resource Table S1 and Fig. 2).

test_that("Table S1 at the reference subject: a 100 kg woman under 70", {
  p <- liraglutideOvergaard(100, 165, 45, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(p$cl1 * 60, 0.86)
  expect_equal(p$v1, 24.6)
  expect_equal(p$ka_SC * 60, 0.0806)
  expect_equal(p$bioavailability_SC, 1)
  expect_equal(p$tlag_SC, 0)
})

test_that("Fig. 2's ratios of exposure, which are 1 / the CL/F ratios", {
  cl <- function(w, a, s) liraglutideOvergaard(w, 170, a, s, FALSE)$PK$default$cl1
  ref <- cl(100, 45, "female")
  expect_equal(ref / cl(60, 45, "female"), 1.41, tolerance = 0.01)   # 1.415
  expect_equal(round(ref / cl(234, 45, "female"), 2), 0.56)
  expect_equal(round(ref / cl(100, 45, "male"), 2), 0.76)
  expect_equal(round(ref / cl(100, 75, "female"), 2), 1.11)    # Fig. 2: 1.10 (1.03-1.18)
  # section 3.1.1: 0.9 L/h in a woman, 1.1 in a man without diabetes (up to 1.4 with it)
  expect_equal(round(ref * 60, 1), 0.9)
  expect_equal(round(cl(100, 45, "male") * 60, 1), 1.1)
  expect_equal(cl(100, 45, "male") * 60 * exp(0.18), 1.35, tolerance = 0.01)   # "1.1-1.4"
})

test_that("switch on: the pharmacokinetic weight in the weight term; V/F scaled", {
  size <- pkSizeFactors(130, 170, 50, "male", TRUE)
  p <- liraglutideOvergaard(130, 170, 50, "male")$PK$default
  expect_equal(p$cl1 * 60, 0.86 * (size$pkWeight / 100)^0.68 * exp(0.27))
  expect_equal(p$v1, 24.6 * size$volume)
  expect_equal_rounded(liraglutideOvergaard(70, 170, 40, "male")$PK$default$cl1 * 60, 0.8839440)
})

test_that("units: subcutaneous, daily", {
  expect_equal(getDrugDefaults("liraglutideOvergaard")$Units[[1]], c("mg SC", "mg SC qd"))
})

test_that("V/F takes the library's volume factor with the switch on (no size term)", {
  size <- pkSizeFactors(130, 180, 50, "male", TRUE)
  expect_equal(liraglutideOvergaard(130, 180, 50, "male")$PK$default$v1, 24.6 * size$volume)
  expect_equal_rounded(liraglutideOvergaard(130, 180, 50, "male")$PK$default$v1, 35.4605322)  # pinned
  expect_equal(liraglutideOvergaard(130, 180, 50, "male", FALSE)$PK$default$v1, 24.6)
})
