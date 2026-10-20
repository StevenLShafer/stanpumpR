# Nitroglycerin (R/drugs_nitroglycerin.R): Armstrong 1983's one-compartment
# fit in congestive cardiac failure.  Drafted by Claude Code, 2026-10-18.

# Armstrong PW et al., Br J Clin Pharmacol 1983;16:385-390
CL  <- 6.2     # L/min
TAU <- 8.1     # min, time constant of the approach to steady state

events <- data.frame(Time = double(), Event = character())

simulateGTN <- function(dose, maximum = 120)
  simulateDrugsWithCovariates(dose, events, 70, 175, 60, "male", maximum,
                              FALSE, adjustToFFM = FALSE)$nitroglycerin$wide

test_that("one compartment with the published clearance and V = CL x time constant", {
  PK <- getDrugPK("nitroglycerin", 70, 175, 60, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CL, set$cl1)
  expect_equal_rounded(CL * TAU, set$v1)
  expect_equal_rounded(50.22, round(set$v1, 2))
  expect_equal(set$cl2, 0)
  expect_equal_rounded(TAU, 1 / set$lambda_1)
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
})

test_that("the study's 21 mcg/min approaches its plateau with an 8.1 min time constant", {
  wide <- simulateGTN(data.frame(Drug = "nitroglycerin", Time = c(0, 90),
                                 Dose = c(21, 0), Units = "mcg/min"))
  css <- 21 / CL                                  # ng/mL
  expect_equal_rounded(3.387, round(css, 3))
  # The abstract: "a peak of approximately 4 ng/ml at 30 min ... maintained"
  at30 <- stats::approx(wide$Time, wide$Plasma, 30)$y
  expect_gt(at30, 3)
  expect_lt(at30, 4.5)
  # Exactly the one-compartment approach, css (1 - exp(-t / tau)), at every
  # point the simulation draws during the infusion
  during <- wide$Time > 0 & wide$Time <= 90
  expect_equal_rounded(css * (1 - exp(-wide$Time[during] / TAU)),
                       wide$Plasma[during], tolerance = 1e-6)
})

test_that("fixed parameters stand with the switch off and scale with it on", {
  off <- nitroglycerin(110, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL, off$cl1)
  size <- pkSizeFactors(110, 175, 50, "male", TRUE)
  on <- nitroglycerin(110, 175, 50, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(CL * size$clearance, on$cl1)
  expect_equal_rounded(CL * TAU * size$volume, on$v1)
})

test_that("nitroglycerin is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "nitroglycerin", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
})
