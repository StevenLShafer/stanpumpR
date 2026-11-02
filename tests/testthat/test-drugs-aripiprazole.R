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
      typical = 195, upperTypical = 270, lowerTypical = 120,
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

test_that("pregnancy: parent clearance x 2.1, the metabolite's own clearance unchanged (Westin 2017)", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  The whole
  # parent clearance forms the metabolite, so kFormation = cl1 / v1 rises with
  # it; dehydroaripiprazole's CLm/fm and Vm/fm do not move.
  on  <- getDrugPK("aripiprazole", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("aripiprazole", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_equal(off, getDrugPK("aripiprazole", 70, 165, 30, SEX_FEMALE))
  a <- on$PK$default
  b <- off$PK$default
  expect_equal(ARIPIPRAZOLE_PREGNANCY_CL_FACTOR, 2.1)
  expect_equal(a$cl1 / b$cl1, ARIPIPRAZOLE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  for (p in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO", "ke0")) {
    expect_identical(a[[p]], b[[p]], info = p)
  }
  expect_equal_rounded(0.09379253848, a$cl1)
  expect_equal_rounded(154.7702502, a$v1)
  # the formation rate constant follows the parent clearance
  xOn  <- aripiprazole(70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  xOff <- aripiprazole(70, 165, 30, SEX_FEMALE)
  expect_equal(xOn$metabolite$kFormation / xOff$metabolite$kFormation,
               ARIPIPRAZOLE_PREGNANCY_CL_FACTOR, tolerance = 1e-12)
  # the metabolite's own disposition is untouched: its eigenvalue is the same,
  # and it does not read the flag
  expect_false("pregnant" %in% names(formals(dehydroaripiprazole)))
  expect_identical(a$metabolite$coefs$lambda[2], b$metabolite$coefs$lambda[2])
  expect_equal(a$metabolite$coefs$lambda[2],
               dehydroaripiprazole(70, 165, 30, SEX_FEMALE)$PK$default$cl1 /
                 dehydroaripiprazole(70, 165, 30, SEX_FEMALE)$PK$default$v1,
               tolerance = 1e-6)
  # so at steady state the parent falls by 1 - 1/2.1 = 52% and the metabolite,
  # dose rate / (CLm/fm), does not move (infusion steady state = sum of the
  # infusion coefficients)
  expect_equal(sum(a$p_coef_infusion_l1) / sum(b$p_coef_infusion_l1),
               1 / ARIPIPRAZOLE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(sum(a$metabolite$coefs$infusion), sum(b$metabolite$coefs$infusion),
               tolerance = 1e-6)
  expect_match(on$reference, "Westin", fixed = TRUE)
  expect_match(on$reference, "pregnancy:", fixed = TRUE)
  expect_no_match(off$reference, "Westin")
})

test_that("ke0 0.725 /h carries Kim 2012's oral hysteresis onto the one-compartment PK", {
  # Kim 2012's own model (Table 1): two compartments, ka 0.293 /h, ke0 0.725
  # /h, solved by eigen-decomposition (hours, mg, L; ng/mL).  Its only PET
  # scan before equilibrium is at 3 h, so Ce/Cp there is what the data
  # identify; see the header of R/drugs_aripiprazole.R.
  kim2012 <- function(t, dose = 10) {
    k10 <- 5.8 / 130; k12 <- 37.1 / 130; k21 <- 37.1 / 274; ka <- 0.293; ke0 <- 0.725
    A <- matrix(c(-ka, ka, 0, 0,
                  0, -(k10 + k12), k12, ke0 / 130 * 1000,
                  0, k21, -k21, 0,
                  0, 0, 0, -ke0), 4, 4)
    e <- eigen(A); c0 <- solve(e$vectors, c(dose, 0, 0, 0))
    x <- Re(e$vectors %*% (exp(e$values * t) * c0))
    c(Cp = x[2] / 130 * 1000, Ce = x[4])
  }
  k <- kim2012(3)
  expect_equal_rounded(unname(k["Ce"] / k["Cp"]), 0.7280635)
  r <- simulateDrugsWithCovariates(
    data.frame(Drug = "aripiprazole", Time = 0, Dose = 10, Units = "mg PO"),
    noEvents, 70, 170, 35, "male", 1440, FALSE, adjustToFFM = FALSE)$aripiprazole$wide
  at3 <- function(y) stats::approx(r$Time, y, 180)$y
  expect_equal(at3(r$`Effect Site`) / at3(r$Plasma), unname(k["Ce"] / k["Cp"]), tolerance = 0.06)
})
