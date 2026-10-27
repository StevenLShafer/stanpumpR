# Nitroglycerin (R/drugs_nitroglycerin.R): Armstrong 1983's clearance in
# congestive cardiac failure, with the volume from the 1.9-min half-life after
# a stop.  Drafted by Claude Code, 2026-10-18; volume revised 2026-10-27.

# Armstrong PW et al., Br J Clin Pharmacol 1983;16:385-390
CL    <- 6.2     # L/min
# Armstrong 1980 (Circulation 62:160), quoted by Armstrong 1983
THALF <- 1.9     # min, after an infusion stops
TAU   <- THALF / log(2)

events <- data.frame(Time = double(), Event = character())

simulateGTN <- function(dose, maximum = 120)
  simulateDrugsWithCovariates(dose, events, 70, 175, 60, "male", maximum,
                              FALSE, adjustToFFM = FALSE)$nitroglycerin$wide

test_that("one compartment with the published clearance and V from the 1.9-min half-life", {
  PK <- getDrugPK("nitroglycerin", 70, 175, 60, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CL, set$cl1)
  expect_equal_rounded(CL * TAU, set$v1)
  expect_equal_rounded(16.99, round(set$v1, 2))
  expect_equal(NITROGLYCERIN_HALF_LIFE, THALF)
  expect_equal(set$cl2, 0)
  expect_equal_rounded(TAU, 1 / set$lambda_1)
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
})

test_that("the study's 21 mcg/min reaches Armstrong's plateau, quickly", {
  wide <- simulateGTN(data.frame(Drug = "nitroglycerin", Time = c(0, 90),
                                 Dose = c(21, 0), Units = "mcg/min"))
  css <- 21 / CL                                  # ng/mL
  expect_equal_rounded(3.387, round(css, 3))
  # The abstract: "a peak of approximately 4 ng/ml at 30 min ... maintained"
  at30 <- stats::approx(wide$Time, wide$Plasma, 30)$y
  expect_gt(at30, 3)
  expect_lt(at30, 4.5)
  # Kim 2018: steady state "after approximately 5 minutes"; 90% by 6.3 min
  expect_equal_rounded(6.31, round(-log(0.1) * TAU, 2))
  at10 <- stats::approx(wide$Time, wide$Plasma, 10)$y
  expect_gt(at10 / css, 0.97)
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

test_that("a bolus is beyond the model, so only infusion rates are offered", {
  defaults <- getDrugDefaultsGlobal()
  row <- defaults[defaults$Drug == "nitroglycerin", ]
  units <- row$Units[[1]]
  expect_setequal(units, c("mcg/min", "mcg/kg/min"))
  expect_true(is.na(row$Bolus.Units) || row$Bolus.Units == "")
})

test_that("the offset after a stop: 16% of the plateau remains at 5 min, as in Sun 1995", {
  # Sun 1995 Figure 1: about 370 to about 70 pg/mL in the 5 min after a
  # 14-h infusion stopped (0.19); Kim 2018: mostly below 0.1 ng/mL by 3-10 min
  wide <- simulateGTN(data.frame(Drug = "nitroglycerin", Time = c(0, 60),
                                 Dose = c(21, 0), Units = "mcg/min"))
  css <- 21 / CL
  at60 <- stats::approx(wide$Time, wide$Plasma, 60)$y
  at65 <- stats::approx(wide$Time, wide$Plasma, 65)$y
  expect_equal(at60 / css, 1, tolerance = 1e-6)
  expect_equal_rounded(0.161, round(exp(-5 / TAU), 3))
  expect_gt(at65 / at60, 0.10)
  expect_lt(at65 / at60, 0.25)
  # Exactly the one-compartment fall at every point drawn after the stop
  after <- wide$Time > 60 & wide$Time <= 90
  expect_equal_rounded(css * exp(-(wide$Time[after] - 60) / TAU),
                       wide$Plasma[after], tolerance = 1e-6)
})
