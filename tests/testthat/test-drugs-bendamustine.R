# Bendamustine (R/drugs_bendamustine.R): Owen 2010, three compartments, no
# covariates; doses as the hydrochloride, the model in base.  Drafted by
# Claude Code, 2026-11-07.

noEvents <- data.frame(Time = double(), Event = character())
f <- 358.26 / 394.72

test_that("the model is Table 2, run in base for a hydrochloride dose", {
  X <- bendamustine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(X$v1 * f, 14.1)
  expect_equal_rounded(X$v2 * f, 0.92)
  expect_equal_rounded(X$v3 * f, 25.2)
  expect_equal_rounded(X$cl1 * 60 * f, 31.7)
  expect_equal_rounded(X$cl2 * 60 * f, 0.989)
  expect_equal_rounded(X$cl3 * 60 * f, 0.159)
  # the label's molecular weight (394.7) and the base fraction
  expect_equal(round(394.72, 1), 394.7)
  expect_equal(round(BENDAMUSTINE_BASE_FRACTION, 4), 0.9076)
})

test_that("the paper's half-lives and Vss", {
  PK <- getDrugPK("bendamustine", 70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  halfLives <- log(2) / c(PK$lambda_1, PK$lambda_2, PK$lambda_3) / 60
  expect_equal(round(halfLives, c(2, 1, 0)), c(0.29, 0.7, 110))
  X <- bendamustine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round((X$v1 + X$v2 + X$v3) * f, 1), 40.2)
})

test_that("120 mg/m2 x 2.0 m2 over 1 h on days 1 and 2: Cmax and AUC near the paper's medians", {
  mg <- 120 * 2.0
  dose <- data.frame(Drug = "bendamustine", Time = c(0, 60, 1440, 1500),
                     Dose = c(mg, 0, mg, 0), Units = "mg/hr")
  wide <- simulateDrugsWithCovariates(dose, noEvents, 70, 170, 35, "male", 4 * 1440,
                                      FALSE, adjustToFFM = FALSE)$bendamustine$wide
  first <- wide[wide$Time <= 1440, ]
  expect_equal(max(first$Plasma), 6038, tolerance = 0.005)     # paper's median 5839
  auc <- sum(diff(wide$Time) * (head(wide$Plasma, -1) + tail(wide$Plasma, -1)) / 2) / 60
  expect_equal(auc, 13750, tolerance = 0.01)                   # paper's median 13,635
})

test_that("both switch positions are pinned", {
  on <- bendamustine(70, 170, 35, "male")$PK$default
  off <- bendamustine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(on$cl1, off$cl1)
  big <- bendamustine(120, 170, 35, "male")$PK$default
  expect_gt(big$cl1, on$cl1)
  expect_equal(bendamustine(120, 170, 35, "male", adjustToFFM = FALSE)$PK$default$cl1,
               off$cl1)
})

test_that("bendamustine is a chemotherapy drug, plasma only, in ng/mL, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "bendamustine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("bendamustine", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Owen")
  expect_identical(therapeuticRange("bendamustine")$band_status, "not_applicable")
})
