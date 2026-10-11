# bupropion and hydroxybupropion: see the headers of R/drugs_bupropion.R and
# R/drugs_hydroxybupropion.R for what the models are and what they leave out.
#
# Every expectation is derived independently of the code under test: the pins
# are Ghimire et al.'s (2026) published estimates converted by hand from L/h
# and 1/h to per minute, the fat-free-mass factors are those worked out by
# hand for the other drug tests, and the half-lives, concentrations and
# exposure ratios come from the compartmental rate equations written out
# below, solved with a matrix exponential (eigendecomposition), sharing no
# code with the package's closed form.  Engine values are read at the end of
# a run, where every grid has a point.
#
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

GHIMIRE_REFERENCE <- paste0(
  "Ghimire et al. Bupropion and hydroxybupropion population ",
  "pharmacokinetics after a single 150 mg sustained-release dose. ",
  "J Clin Pharm Ther 2026. https://doi.org/10.1155/jcpt/3265655"
)

# --- Independent reference: the five-compartment rate equations --------------
#
# Amounts (mg) in the gut, bupropion's central and peripheral compartments
# and hydroxybupropion's, from the published values (L, L/h, 1/h; converted
# to minutes).  Bupropion's total CL/F eliminates from its central
# compartment, and 0.1 of it (fm, fixed in the source) is input to the
# metabolite, mass for mass.  The apparent scale carries F, so the whole dose
# enters the gut.
ghimireSystem <- function() {
  h <- function(x) x / 60
  ka <- h(0.29)
  v1 <- 524.6; v2 <- 5006.9; cl <- h(148.4); q <- h(257)
  m1 <- 2.2;   m2 <- 17.7;   mcl <- h(0.9);  mq <- h(3.1)
  K <- matrix(0, 5, 5)
  K[1, 1] <- -ka
  K[2, 1] <- ka; K[2, 2] <- -(cl + q) / v1; K[2, 3] <- q / v2
  K[3, 2] <- q / v1; K[3, 3] <- -q / v2
  K[4, 2] <- 0.1 * cl / v1; K[4, 4] <- -(mcl + mq) / m1; K[4, 5] <- mq / m2
  K[5, 4] <- mq / m1; K[5, 5] <- -mq / m2
  e <- eigen(K)
  list(K = K, V = e$vectors, Vi = solve(e$vectors), lambda = e$values,
       v1 = v1, m1 = m1)
}

# Concentrations (ng/mL) at `at` minutes after oral doses `doses` (mg) at
# `times` (min).
ghimireConcentrations <- function(times, doses, at) {
  S <- ghimireSystem()
  x <- rep(0, 5)
  for (i in seq_along(times)) {
    if (times[i] < at) {
      dt <- at - times[i]
      E <- Re(S$V %*% diag(exp(S$lambda * dt)) %*% S$Vi)
      x <- x + as.vector(E %*% c(doses[i], 0, 0, 0, 0))
    }
  }
  c(parent = x[2] / S$v1, metabolite = x[4] / S$m1) * 1000
}

# The package's answer at the end of a run: always a grid point.
engineAt <- function(DT, at, adjustToFFM = FALSE) {
  o <- simulateDrugsWithCovariates(DT, noEvents, 70, 170, 50, "male",
                                   at, FALSE, adjustToFFM = adjustToFFM)
  a <- o$bupropion$wide
  m <- o$hydroxybupropion$wide
  c(parent = a$Plasma[nrow(a)], metabolite = m$Plasma[nrow(m)])
}


# --- The models ---------------------------------------------------------------

test_that("bupropion returns Ghimire's estimates with the fat-free-mass switch off", {
  actual <- bupropion(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 524.6,
        v2 = 5006.9,
        v3 = 1,
        cl1 = 2.473333333,      # 148.4 L/h
        cl2 = 4.283333333,      # 257 L/h
        cl3 = 0,
        ka_PO = 0.004833333333, # 0.29 /h
        bioavailability_PO = 1,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = GHIMIRE_REFERENCE,
    prodrug = FALSE,
    metabolite = list(
      name              = "hydroxybupropion",
      kFormation        = 4.714703266e-4,   # 0.1 x 148.4 / 524.6 per hour, per minute
      firstPassFraction = 0,
      mwRatio           = 1
    )
  )
  expect_equal_rounded(actual, expected)
  # No covariate was retained: with the switch off size changes nothing.
  expect_equal(bupropion(120, 160, 80, "female", adjustToFFM = FALSE), actual)
})

test_that("hydroxybupropion returns Ghimire's estimates with the fat-free-mass switch off", {
  actual <- hydroxybupropion(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 2.2,
        v2 = 17.7,
        v3 = 1,
        cl1 = 0.015,            # 0.9 L/h
        cl2 = 0.05166666667,    # 3.1 L/h
        cl3 = 0
      )
    ),
    # AGNP 2018 range for bupropion + hydroxybupropion, mirrored from the CSV
    tPeak = 0, MEAC = 0, typical = 1200, upperTypical = 1500, lowerTypical = 850,
    reference = GHIMIRE_REFERENCE
  )
  expect_equal_rounded(actual, expected)
  expect_equal(hydroxybupropion(120, 160, 80, "female", adjustToFFM = FALSE), actual)
})

test_that("the pair scales to fat-free mass identically for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126,
  # the same factors for both members.
  b <- bupropion(120, 170, 50, "male")
  h <- hydroxybupropion(120, 170, 50, "male")
  expect_equal_rounded(b$PK$default, list(
    v1 = 684.5540548, v2 = 6533.537356, v3 = 1,
    cl1 = 3.019723831, cl2 = 5.229575637, cl3 = 0,
    ka_PO = 0.004833333333, bioavailability_PO = 1, tlag_PO = 0
  ))
  expect_equal_rounded(h$PK$default, list(
    v1 = 2.87079474, v2 = 23.09684859, v3 = 1,
    cl1 = 0.018313689, cl2 = 0.06308048433, cl3 = 0
  ))
  # Formation stays fm x the scaled parent's k10: x 1.2209126 / 1.3049067
  expect_equal_rounded(b$metabolite$kFormation, 4.411227732e-4)
})

test_that("both drugs give the same, single citation", {
  expect_identical(bupropion(70, 170, 50, "male")$reference, GHIMIRE_REFERENCE)
  expect_identical(hydroxybupropion(70, 170, 50, "male")$reference, GHIMIRE_REFERENCE)
})


# --- Half-lives, formation and exposure ---------------------------------------

test_that("the half-lives are those of the published parameters", {
  # From the two-compartment quadratic, by hand: bupropion 0.8599 h and
  # 38.48 h, hydroxybupropion 0.3542 h and 18.93 h.
  PK <- getDrugPK("bupropion", 70, 170, 50, "male", getDrugDefaults("bupropion"),
                  adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / PK$lambda_1 / 60, 0.8598821679, tolerance = 1e-6)
  expect_equal(log(2) / PK$lambda_2 / 60, 38.48062869, tolerance = 1e-6)
  expect_equal(PK$lambda_3, 0)
  expect_equal(PK$ke0, 0)

  M <- getDrugPK("hydroxybupropion", 70, 170, 50, "male",
                 getDrugDefaults("hydroxybupropion"), adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / M$lambda_1 / 60, 0.3542418514, tolerance = 1e-6)
  expect_equal(log(2) / M$lambda_2 / 60, 18.92965928, tolerance = 1e-6)
  expect_equal(M$ke0, 0)

  # The formation flux kFormation x Vc x Cp is fm x CL x Cp
  expect_equal(PK$metabolite$name, "hydroxybupropion")
  expect_equal(PK$metabolite$coefs$K, 0.1 * 148.4 / 60, tolerance = 1e-12)

  # The independent system has the same eigenvalues as the package's
  # convolution: absorption, the parent's two and the metabolite's two.
  S <- ghimireSystem()
  expect_equal(sort(-S$lambda), sort(PK$metabolite$coefs$lambda), tolerance = 1e-10)
})

test_that("a single 150 mg dose follows the rate equations", {
  DT <- data.frame(Drug = "bupropion", Time = 0, Dose = 150, Units = "mg PO")
  for (hours in c(1, 3, 6, 24, 72, 168)) {
    at <- hours * 60
    expect_equal(engineAt(DT, at), ghimireConcentrations(0, 150, at),
                 tolerance = 1e-7, label = paste(hours, "h"))
  }
})

test_that("the formed exposure ratio is fm x CL / CL_met", {
  # AUC(metabolite) / AUC(parent) = 0.1 x 148.4 / 0.9 = 16.48888889, after
  # any oral dose; from the oral coefficients, integrated analytically ...
  ratio <- 0.1 * 148.4 / 0.9
  PK <- getDrugPK("bupropion", 70, 170, 50, "male", getDrugDefaults("bupropion"),
                  adjustToFFM = FALSE)$PK$default
  lam <- c(PK$lambda_1, PK$lambda_2, PK$ka_PO)
  aucP <- sum(c(PK$p_coef_PO_l1, PK$p_coef_PO_l2, PK$p_coef_PO_ka) / lam)
  co <- PK$metabolite$coefs
  aucM <- sum(co$PO / co$lambda)
  expect_equal(aucM / aucP, ratio, tolerance = 1e-8)

  # ... and through the engine: a single dose followed for three weeks (the
  # slowest exponential down by e^-9), by the trapezoid on the engine's grid.
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "bupropion", Time = 0, Dose = 150, Units = "mg PO"),
    noEvents, 70, 170, 50, "male", 21 * MINS_PER_DAY, FALSE, adjustToFFM = FALSE)
  expect_setequal(names(o), c("bupropion", "hydroxybupropion"))
  expect_equal(o$hydroxybupropion$formedFrom, "bupropion")
  trap <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
  expect_equal(trap(o$hydroxybupropion$wide) / trap(o$bupropion$wide), ratio,
               tolerance = 0.01)
})

test_that("150 mg SR bid at steady state sits in the AGNP range for the sum", {
  # Average steady state: 300 mg/day over CL/F (148.4 L/h x 24) = 84.23 ng/mL
  # of bupropion, and 16.49 times that, 1388.9 ng/mL, of hydroxybupropion:
  # a sum of 1473 ng/mL, inside 850 to 1500.
  avgParent <- 300 / (148.4 * 24) * 1000
  expect_equal(avgParent, 84.23180593, tolerance = 1e-9)
  expect_equal(avgParent * 0.1 * 148.4 / 0.9, 1388.888889, tolerance = 1e-9)

  # Through the engine: 150 mg PO bid for four weeks, read at the trough
  # before the next dose and checked against the rate equations.
  four <- 28 * MINS_PER_DAY
  DT <- data.frame(Drug = "bupropion", Time = 0, Dose = 150, Units = "mg PO bid")
  times <- seq(0, four - 720, by = 720)
  expected <- ghimireConcentrations(times, rep(150, length(times)), four)
  expect_equal(engineAt(DT, four), expected, tolerance = 1e-7)
  expect_gt(sum(expected), 850)
  expect_lt(sum(expected), 1500)
})

test_that("neither drug has an effect site, and the metabolite row is formed", {
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "bupropion", Time = 0, Dose = 150, Units = "mg PO"),
    noEvents, 70, 170, 50, "male", 3 * MINS_PER_DAY, FALSE)
  expect_setequal(names(o), c("bupropion", "hydroxybupropion"))
  expect_true(all(is.na(o$bupropion$wide$"Effect Site")))
  expect_true(all(is.na(o$hydroxybupropion$wide$"Effect Site")))
  expect_equal(max(o$bupropion$equiSpace$MEAC), 0)
  expect_equal(max(o$hydroxybupropion$equiSpace$MEAC), 0)
})


# --- The library rows ---------------------------------------------------------

test_that("the library offers bupropion orally only and hydroxybupropion not at all", {
  dd <- getDrugDefaultsGlobal()
  b <- dd[dd$Drug == "bupropion", ]
  units <- b$Units[[1]]
  # Apparent parameters: oral only, with the scheduled frequencies
  expect_setequal(units, c("mg PO", "mg PO qd", "mg PO bid"))
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_equal(b$Default.Units, "mg PO")
  expect_equal(b$Concentration.Units, "ng")
  m <- bupropion(70, 170, 50, "male")
  expect_equal(c(b$Lower, b$Upper, b$Typical), c(m$lowerTypical, m$upperTypical, m$typical))
  expect_equal(c(b$Lower, b$Upper, b$Typical, b$MEAC, b$endCe), rep(0, 5))

  h <- dd[dd$Drug == "hydroxybupropion", ]
  expect_length(helpDrugUnits(h), 0)
  expect_equal(h$Concentration.Units, "ng")
  mh <- hydroxybupropion(70, 170, 50, "male")
  expect_equal(c(h$Lower, h$Upper, h$Typical), c(mh$lowerTypical, mh$upperTypical, mh$typical))
  expect_equal(c(h$Lower, h$Upper, h$Typical), c(850, 1500, 1200))
  expect_equal(c(h$MEAC, h$endCe), c(0, 0))
  expect_false(identical(b$Color, h$Color))
})

test_that("a dose table of scheduled oral rows validates", {
  rows <- data.frame(Drug = "bupropion", Time = c("0", "720"),
                     Dose = c("150", "150"), Units = c("mg PO", "mg PO bid"))
  expect_true(validateDoseTableInput(rows))
})
