# Cladribine (R/drugs_cladribine.R): Lindemalm 2005, three compartments,
# intravenous, subcutaneous and oral.  Drafted by Claude Code, 2026-11-05.

noEvents <- data.frame(Time = double(), Event = character())
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)

test_that("with the switch off the parameters are Table 3's, for everyone", {
  for (w in c(50, 70, 110)) {
    set <- getDrugPK("cladribine", w, 170, 60, "female", adjustToFFM = FALSE)$PK$default
    expect_equal_rounded(set$v1, 71.7)
    expect_equal_rounded(set$v2, 475)
    expect_equal_rounded(set$v3, 73.6)
    expect_equal_rounded(set$cl1, 39.3 / 60)
    expect_equal_rounded(set$cl2, 51.1 / 60)
    expect_equal_rounded(set$cl3, 105 / 60)
  }
})

test_that("the half-lives are the paper's 0.2, 1.3 and 16 hours", {
  set <- getDrugPK("cladribine", 70, 170, 60, "male", adjustToFFM = FALSE)$PK$default
  halfLives <- log(2) / c(set$lambda_1, set$lambda_2, set$lambda_3) / 60
  expect_equal(round(halfLives, 1), c(0.2, 1.3, 16.4))
})

test_that("the reference man is unchanged by the switch; others scale to fat-free mass", {
  on  <- getDrugPK("cladribine", 70, 170, 35, "male", adjustToFFM = TRUE)$PK$default
  off <- getDrugPK("cladribine", 70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(on$v1, off$v1, tolerance = 1e-6)
  expect_equal(on$cl1, off$cl1, tolerance = 1e-6)
  size <- pkSizeFactors(55, 160, 60, "female", TRUE)
  woman <- getDrugPK("cladribine", 55, 160, 60, "female")$PK$default
  expect_equal_rounded(woman$v1, 71.7 * size$volume)
  expect_equal_rounded(woman$cl1, 39.3 / 60 * size$clearance)
})

test_that("oral and subcutaneous exposures match the bioavailabilities 0.353 and 1", {
  sim <- function(dose, units, rows = 1)
    simulateDrugsWithCovariates(
      data.frame(Drug = "cladribine", Time = c(0, 120)[seq_len(rows)],
                 Dose = c(dose, 0)[seq_len(rows)], Units = units),
      noEvents, 70, 170, 60, "male", 14 * 1440, FALSE,
      adjustToFFM = FALSE)$cladribine$wide
  iv <- sim(10 / 120, "mg/min", 2)    # 10 mg over the paper's 2 h
  sc <- sim(10, "mg SC")
  po <- sim(10, "mg PO")
  # dose / CL, mg min/L -> ng min/mL
  expect_equal(auc(iv), 10 / (39.3 / 60) * 1000, tolerance = 0.01)
  expect_equal(auc(sc) / auc(iv), 1, tolerance = 0.01)
  expect_equal(auc(po) / auc(iv), 0.353, tolerance = 0.01)
  # first-order absorption: the subcutaneous peak comes before the oral one
  expect_lt(sc$Time[which.max(sc$Plasma)], po$Time[which.max(po$Plasma)])
  expect_identical(getDrugPK("cladribine", 70, 170, 60, "male")$Concentration.Units, "ng")
})

test_that("cladribine is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "cladribine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("cladribine", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Lindemalm")
})
