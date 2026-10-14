# tiagabine: see R/drugs_tiagabine.R (Ingwersen 2000, monotherapy, final
# model with its height covariates; the abstract's 62 L is a misprint for 52).
test_that("reference subject of 170 cm: the paper's values and half-life", {
  a <- tiagabine(70, 170, 40, "male", adjustToFFM = FALSE)
  expect_equal(a$PK$default$v1, 52.0)
  expect_equal(a$PK$default$cl1, 6.10 / 60)
  expect_equal(a$PK$default$ka_PO, 1.25 / 60)
  # ln 2 x 52 / 6.10 = 5.91 h, the paper's population half-life of 5.72 h;
  # the abstract's 62 L would give 7.0 h
  expect_equal(log(2) * a$PK$default$v1 / (a$PK$default$cl1 * 60), 5.91, tolerance = 1e-3)
})
test_that("clearance and volume follow height by the published exponents, whatever the switch", {
  for (ffm in c(TRUE, FALSE)) {
    a <- tiagabine(20, 110, 5, "male", adjustToFFM = ffm)
    expect_equal(a$PK$default$v1, 52.0 * (110 / 170)^3.44)
    expect_equal(a$PK$default$cl1, 6.10 * (110 / 170)^2.13 / 60)
    b <- tiagabine(120, 190, 50, "male", adjustToFFM = ffm)
    expect_equal(b$PK$default$v1, 52.0 * (190 / 170)^3.44)
    expect_equal(b$PK$default$cl1, 6.10 * (190 / 170)^2.13 / 60)
  }
  # Weight does not enter: height is the model's size covariate
  expect_equal(tiagabine(50, 170, 40, "female")$PK$default, tiagabine(120, 170, 40, "male")$PK$default)
})
