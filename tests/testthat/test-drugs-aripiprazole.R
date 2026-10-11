# aripiprazole and its metabolite dehydroaripiprazole: see
# R/drugs_aripiprazole.R (Kim 2008; effect site Kim 2012).  Hand-worked pins.

noEvents <- data.frame(Time = numeric(0), Event = character(0))
trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

test_that("a normal metaboliser receives the published values", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- aripiprazole(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 192, v2 = 1, v3 = 1,
        cl1 = 3.15 / 60, cl2 = 0, cl3 = 0,
        ka_PO = 1.06 / 60, bioavailability_PO = 1, tlag_PO = 0
      )),
      tPeak = 0, ke0 = 0.725 / 60, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference,
      metabolite = list(
        name = "dehydroaripiprazole", kFormation = 3.15 / 60 / 192,
        firstPassFraction = 0, mwRatio = 1
      )
    )
    expect_equal_rounded(actual, expected)
  }
  m <- dehydroaripiprazole(70, 170, 35, "male")$PK$default
  expect_equal(m$v1, 587)
  expect_equal(m$cl1, 8.02 / 60)
})

test_that("CYP2D6 uses Kim's two named groups; unstudied phenotypes take the nearest", {
  cl <- function(p) aripiprazole(70, 170, 35, "male", cyp2d6 = p)$PK$default$cl1 * 60
  expect_equal(cl("normal"), 3.15)
  expect_equal(cl("intermediate"), 1.83)
  expect_equal(cl("poor"), 1.83)
  expect_equal(cl("ultrarapid"), 3.15)
  # The abstract: intermediate metabolisers' CL/F about 60% of normal
  expect_equal(1.83 / 3.15, 0.58, tolerance = 0.01)
})

test_that("both species scale to fat-free mass with the switch on", {
  x <- aripiprazole(120, 170, 50, "male")$PK$default
  expect_equal_rounded(x$v1, 192 * 1.3049067)
  expect_equal_rounded(x$cl1, 3.15 / 60 * 1.2209126)
  m <- dehydroaripiprazole(120, 170, 50, "male")$PK$default
  expect_equal_rounded(m$v1, 587 * 1.3049067)
  expect_equal_rounded(m$cl1, 8.02 / 60 * 1.2209126)
})

test_that("ke0 is supplied from Kim 2012, not solved", {
  pk <- getDrugPK("aripiprazole", 70, 170, 35, "male", getDrugDefaults("aripiprazole"))
  expect_equal(pk$PK$default$ke0, 0.725 / 60)
  expect_equal(pk$PK$default$ke0, antipsychoticProfile("aripiprazole")$pd[[1]]$ke0)
})

test_that("the metabolite ratio is CL/F over CLm/fm, near the 0.20-0.34 observed", {
  for (p in c("normal", "intermediate")) {
    r <- simulateDrugsWithCovariates(
      data.frame(Drug = "aripiprazole", Time = 0, Dose = 10, Units = "mg PO"),
      noEvents, 70, 170, 35, "male", 40320, FALSE, cyp2d6 = p, adjustToFFM = FALSE
    )
    a <- r$aripiprazole$wide
    m <- r$dehydroaripiprazole$wide
    clp <- if (p == "normal") 3.15 else 1.83
    expect_equal(trapz(m$Time, m$Plasma) / trapz(a$Time, a$Plasma), clp / 8.02,
                 tolerance = 0.01)
    # Metabolite exposure per dose does not depend on genotype (Kim's abstract)
    expect_equal(trapz(m$Time, m$Plasma) / 60, 10000 / 8.02, tolerance = 0.01)
  }
})
