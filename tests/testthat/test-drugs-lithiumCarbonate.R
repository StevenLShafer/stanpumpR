# Lithium carbonate (R/drugs_lithiumCarbonate.R): Findling 2010's
# two-compartment model, scaled by fat-free mass, plotted as serum lithium in
# mmol/L.  Drafted by Claude Code, 2026-11-12.
#
# The expectations are built from Table 2 of the source (PMC2967308) and the
# derivation in the drug header, not from the drug file.

events <- data.frame(Time = double(), Event = character())

simulateLi <- function(dose, weight = 70, height = 170, age = 35, sex = "male",
                       maximum = 14 * 1440) {
  simulateDrugsWithCovariates(dose, events, weight, height, age, sex, maximum,
                              FALSE)$lithiumCarbonate$wide
}

test_that("the derived two compartments reproduce Table 2's volume and half-lives", {
  # at 53 kg of fat-free mass: CL 1.79 L/h, V1 + V2 = 56.4 L, T1/2a 2.43 h,
  # T1/2b 27.4 h
  V1 <- 21.52; Vss <- 56.4; Q <- 3.026; CL <- 1.79
  k10 <- CL / V1; k12 <- Q / V1; k21 <- Q / (Vss - V1)
  s <- k10 + k12 + k21
  root <- sqrt(s^2 - 4 * k10 * k21)
  expect_equal(log(2) / ((s + root) / 2), 2.43, tolerance = 2e-3)
  expect_equal(log(2) / ((s - root) / 2), 27.4, tolerance = 2e-3)
})

test_that("parameters scale with fat-free mass from 53 kg", {
  for (p in list(c(70, 170, 35, "male"), c(60, 160, 70, "female"), c(40, 150, 12, "female"))) {
    w <- as.numeric(p[1]); h <- as.numeric(p[2]); a <- as.numeric(p[3]); s <- p[4]
    r <- ffmAlSallami(w, h, a, s) / 53
    set <- lithiumCarbonate(w, h, a, s)$PK$default
    expect_equal(set$cl1, 1.79 / 60 * r^0.75)
    expect_equal(set$v1, 21.52 * r)
    expect_equal(set$v2, (56.4 - 21.52) * r)
    expect_equal(set$cl2, 3.026 / 60 * r^0.75)
    # the switch is ignored: the model carries its own size covariate
    expect_equal(lithiumCarbonate(w, h, a, s, adjustToFFM = FALSE)$PK$default, set)
  }
  set <- lithiumCarbonate(70, 170, 35, "male")$PK$default
  expect_equal(set$ka_PO, log(2) / 29.9)
  expect_equal(set$tlag_PO, 17.4)
  expect_equal(set$bioavailability_PO, 1)
})

test_that("a dose of carbonate is millimoles of lithium", {
  expect_equal(LITHIUM_CARBONATE_MW_PER_LI, 73.89 / 2, tolerance = 1e-3)
  # the label: 300 mg of lithium carbonate is about 8 mEq of lithium
  expect_equal(300 / LITHIUM_CARBONATE_MW_PER_LI, 8.12, tolerance = 1e-3)
  pk <- getDrugPK("lithiumCarbonate", 70, 170, 35, "male")
  expect_identical(pk$Concentration.Units, "mmol")
})

test_that("24 mmol a day gives Westin 2017's measured 0.53 mmol/L", {
  # Westin 2017 (not used in the fit): nonpregnant WOMEN's serum lithium,
  # sampled 10-16 h after the dose, normalised to 24 mmol/day, 0.53 mmol/L
  # (95% CI 0.48-0.59).  24 mmol a day is 886.7 mg of carbonate; given once
  # daily for two weeks to a 65 kg, 165 cm woman and sampled 12 h after the
  # dose (the guideline's sampling time), the model is inside that interval.
  # Its average over the day, dose rate over clearance, is 0.67 mmol/L in her
  # and 0.55 mmol/L in the reference man.
  mg <- 24 * LITHIUM_CARBONATE_MW_PER_LI
  wide <- simulateLi(data.frame(Drug = "lithiumCarbonate", Time = 0, Dose = mg,
                                Units = "mg PO qd"),
                     weight = 65, height = 165, sex = "female", maximum = 15 * 1440)
  at12 <- approx(wide$Time, wide$Plasma, 14 * 1440 + 720)$y
  expect_true(at12 > 0.48 && at12 < 0.59)
  man <- lithiumCarbonate(70, 170, 35, "male")$PK$default
  expect_equal(24 / 24 / (man$cl1 * 60), 0.547, tolerance = 2e-3)
})

test_that("pregnancy raises clearance by Westin's 1.52, nothing else", {
  on  <- lithiumCarbonate(70, 165, 30, "female", pregnant = TRUE)$PK$default
  off <- lithiumCarbonate(70, 165, 30, "female")$PK$default
  expect_equal(on$cl1 / off$cl1, 1 / 0.66)
  for (p in c("v1", "v2", "cl2", "ka_PO")) expect_equal(on[[p]], off[[p]])
  expect_match(lithiumCarbonate(70, 165, 30, "female", pregnant = TRUE)$reference,
               "pregnancy:", fixed = TRUE)
})

test_that("the band is the AGNP 2026 maintenance range", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "lithiumCarbonate", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$endCe), c(0.4, 0.8, 0.6, 1.2))
  expect_false(any(unlist(row$Units) %in% c("mg", "g")))
})
