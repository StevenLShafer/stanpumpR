# Methotrexate, high dose, children and young adults
# (R/drugs_methotrexatePediatric.R): Taylor 2023, the MTXPK.org model.
# Drafted by Claude Code, 2026-11-06.

p <- METHOTREXATE_TAYLOR
noEvents <- data.frame(Time = double(), Event = character())

test_that("the model is equations 3-7 at the reference covariates", {
  w <- 30; h <- 120; age <- 8; scr <- 0.3
  m <- methotrexatePediatric(w, h, age, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  b <- sqrt(w * h / 3600) / 1.73
  expect_equal_rounded(m$cl1, 8.24 * b * (scr * SCR_UMOL_PER_MG_DL / 40)^-0.26 / 60)
  expect_equal_rounded(m$v1, 22.1 * b)
  expect_equal_rounded(m$cl2, 0.225 * b / 60)
  expect_equal_rounded(m$v2, 2.19 * b)
  expect_equal_rounded(m$cl3, 0.0342 * b / 60)
  expect_equal_rounded(m$v3, 6.07 * b)
  # at 1.73 m2 and 40 uM the parameters are Table 2's
  ref <- methotrexatePediatric(70, 170, 30, "male", adjustToFFM = FALSE,
                               creatinine = 40 / SCR_UMOL_PER_MG_DL)$PK$default
  bRef <- sqrt(70 * 170 / 3600) / 1.73
  expect_equal(ref$cl1 * 60 / bRef, 8.24)
})

test_that("a blank creatinine is the normal value for age, read as the patient's own", {
  blank <- methotrexatePediatric(20, 110, 5, "female")$PK$default$cl1
  entered <- methotrexatePediatric(20, 110, 5, "female",
                                   creatinine = assumedCreatinine(5, "female"))$PK$default$cl1
  expect_equal(blank, entered)
  # a doubled creatinine lowers clearance by 2^-0.26
  q <- assumedCreatinine(5, "female")
  expect_equal(methotrexatePediatric(20, 110, 5, "female", creatinine = 2 * q)$PK$default$cl1 / blank,
               2^-0.26)
})

test_that("5 g/m2 over 24 h reproduces Table 5's end-of-infusion concentration", {
  # A typical long-infusion child: BSA 1.0 m2 (cohort median 0.96), creatinine
  # 0.3 mg/dL (median).  Table 5, ALL, 5 g/m2 over 24 h: mean 84.8 uM at 24 h.
  # Its later means (2.46, 0.96, 0.48 uM at 36, 42, 48 h) are means of
  # individual predictions, pulled up by slow eliminators, so the typical
  # curve sits below them, by less than a factor of two.
  bsa <- sqrt(30 * 120 / 3600)
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "methotrexatePediatric", Time = c(0, 1440),
               Dose = c(5000 * bsa / 24, 0), Units = "mg/hr"),
    noEvents, 30, 120, 8, "male", 80 * 60, FALSE, adjustToFFM = FALSE,
    creatinine = 0.3)$methotrexatePediatric$wide
  uM <- function(h) wide$Plasma[which.min(abs(wide$Time - h * 60))] / (METHOTREXATE_MW / 1000)
  expect_equal(uM(24), 84.8, tolerance = 0.03)
  tableMeans <- c(`36` = 2.46, `42` = 0.96, `48` = 0.48, `60` = 0.19, `72` = 0.09)
  for (h in names(tableMeans)) {
    expect_lt(uM(as.numeric(h)), tableMeans[[h]], label = h)
    expect_gt(uM(as.numeric(h)), tableMeans[[h]] / 2, label = h)
  }
})

test_that("the size covariate takes the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(90, 160, 16, "female", TRUE)
  on <- methotrexatePediatric(90, 160, 16, "female")$PK$default
  expect_equal_rounded(on$v1, 22.1 * sqrt(size$pkWeight * 160 / 3600) / 1.73)
  for (args in list(list(2, 45, 0, "male"), list(250, 210, 90, "female"))) {
    m <- do.call(methotrexatePediatric, args)$PK$default
    expect_true(all(is.finite(unlist(m))) && m$cl1 > 0 && m$v1 > 0)
  }
})

test_that("methotrexatePediatric is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "methotrexatePediatric", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("methotrexatePediatric", 30, 120, 8, "male")
  expect_equal(PK$tPeak, 0)
  expect_identical(PK$Concentration.Units, "mcg")
  expect_match(PK$reference, "Taylor")
  expect_identical(therapeuticRange("methotrexatePediatric")$band_status, "not_applicable")
})
