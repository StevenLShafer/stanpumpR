# risperidone and its metabolite hydroxyrisperidone: see R/drugs_risperidone.R
# (Storset 2024).  Hand-worked pins from the published equations.

noEvents <- data.frame(Time = numeric(0), Event = character(0))
trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

test_that("a normal metaboliser of 30 receives the published values", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- risperidone(70, 170, 30, "male", adjustToFFM = adjust)
    cl1 <- (4.2 + 23.2 * 2) / 60    # *1/*1, 50.6 L/h; no age term below 34
    expected <- list(
      PK = list(default = list(
        v1 = 333, v2 = 1, v3 = 1,
        cl1 = cl1, cl2 = 0, cl3 = 0,
        ka_PO = 2.01 / 60, bioavailability_PO = 1, tlag_PO = 0
      )),
      tPeak = 0, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference,
      prodrug = FALSE,
      metabolite = list(
        name = "hydroxyrisperidone", kFormation = cl1 / 333,
        firstPassFraction = 0, mwRatio = 1
      )
    )
    if (adjust) next   # the FFM reference patient is 35, not 30: same values
    expect_equal_rounded(actual, expected)
  }
})

test_that("CYP2D6 phenotype maps to Storset's example genotypes", {
  cl <- function(p) risperidone(70, 170, 30, "male", cyp2d6 = p, adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(cl("poor"), 4.2)           # two deficient alleles
  expect_equal(cl("intermediate"), 27.4)  # *1/deficient
  expect_equal(cl("normal"), 50.6)        # *1/*1
  expect_equal(cl("ultrarapid"), 73.8)    # *1/*1xN, extrapolated
  expect_error(risperidone(70, 170, 30, "male", cyp2d6 = "fast"), "Invalid cyp2d6")
})

test_that("the age terms are Storset's, linear above 34 and 39", {
  x <- risperidone(70, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 50.6 * (1 - 0.009 * 16))
  m <- hydroxyrisperidone(70, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(m$cl1 * 60, 8.0 * (1 - 0.013 * 11))
  expect_equal(m$v1, 96)
  # Positive up to the app's maximum age; refuses beyond the fitted range
  expect_gt(hydroxyrisperidone(70, 170, MAX_AGE, "male")$PK$default$cl1, 0)
  expect_error(hydroxyrisperidone(70, 170, 200, "male"), "beyond the fitted range")
})

test_that("both species scale to fat-free mass with the switch on", {
  # 120 kg, 170 cm, 50 y man, with the age terms of a 50-year-old
  x <- risperidone(120, 170, 50, "male")$PK$default
  expect_equal_rounded(x$v1, 333 * 1.3049067)
  expect_equal_rounded(x$cl1, 50.6 * (1 - 0.009 * 16) / 60 * 1.2209126)
  m <- hydroxyrisperidone(120, 170, 50, "male")$PK$default
  expect_equal_rounded(m$v1, 96 * 1.3049067)
  expect_equal_rounded(m$cl1, 8 * (1 - 0.013 * 11) / 60 * 1.2209126)
})

test_that("the metabolite-to-parent exposure ratio is CL/F over CLm/(F fmet)", {
  # Whole apparent clearance forms the scaled metabolite, so the AUC ratio of a
  # single dose is 50.6 / 8.0 = 6.325 at 30 years
  r <- simulateDrugsWithCovariates(
    data.frame(Drug = "risperidone", Time = 0, Dose = 2, Units = "mg PO"),
    noEvents, 70, 170, 30, "male", 14400, FALSE, adjustToFFM = FALSE
  )
  p <- r$risperidone$wide
  m <- r$hydroxyrisperidone$wide
  ratio <- trapz(m$Time, m$Plasma) / trapz(p$Time, p$Plasma)
  expect_equal(ratio, 50.6 / 8.0, tolerance = 0.01)
  # Parent AUC is dose / (CL/F): 2 mg / 50.6 L/h = 39.5 ng.h/mL
  expect_equal(trapz(p$Time, p$Plasma) / 60, 2000 / 50.6, tolerance = 0.01)
})

test_that("hydroxyrisperidone is never dosed directly", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "hydroxyrisperidone", ]
  expect_true(all(is.na(row$Units[[1]])))
  expect_true(is.na(row$Category))
})
