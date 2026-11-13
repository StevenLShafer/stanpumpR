# Carboplatin (R/drugs_carboplatin.R): Duffull 1997, two compartments with
# lean body weight and an adjusted Cockcroft-Gault creatinine clearance.
# Drafted by Claude Code, 2026-11-06.

p <- CARBOPLATIN_DUFFULL

test_that("Pesola's LBW and the equations reproduce Table 2 at the cohort means", {
  # Table 1: mean LBW 56 kg, adjusted Cockcroft-Gault 74.2 mL/min
  lbw <- 56; clcr <- 74.2 * 60 / 1000
  expect_equal(round(p$clNonRenal * lbw^0.75 + p$clRenal * clcr, 1), 6.5)  # Table 2: 6.8
  expect_equal(p$v1 * lbw, 14.56)                                           # Table 2: 14.8
  expect_equal(p$vss * lbw, 21.28)                                          # Table 2: 21.6
  expect_equal(p$clic * (lbw / p$clicLBW)^0.75, 4.4)                        # Table 2: 4.4
  # Table 2's Clic range 3.9-5.0 is out of reach of the printed 0.15 LBW^0.75
  # over the study's LBW 48-64 kg (Table 1), which is why it was not used
  expect_lt(0.15 * 64^0.75, 3.9)
  expect_equal(leanBodyWeightPesola(160, "female"), 54)
  expect_equal(leanBodyWeightPesola(160, "male"), 59)
})

test_that("the model is the published equations at the patient's LBW and adjusted ClCr", {
  w <- 70; h <- 165; age <- 60
  m <- carboplatinDuffull(w, h, age, "female", adjustToFFM = FALSE)$PK$default
  lbw <- 0.9 * (h - 150) + 45
  scr <- max(adultEquivalentCreatinine(NULL, age, "female"), 60 / SCR_UMOL_PER_MG_DL)
  clcr <- (140 - age) * min(lbw, w) / (72 * scr) * 0.85 * 60 / 1000
  expect_equal_rounded(m$cl1, (0.15 * lbw^0.75 + 0.78 * clcr) / 60)
  expect_equal_rounded(m$v1, 0.26 * lbw)
  expect_equal_rounded(m$v2, 0.12 * lbw)
  expect_equal_rounded(m$cl2, 4.4 * (lbw / 56)^0.75 / 60)
})

test_that("a low creatinine is floored at 60 uM; a high one lowers clearance", {
  cl <- function(scr) carboplatinDuffull(70, 165, 60, "female", creatinine = scr)$PK$default$cl1
  expect_equal(cl(0.4), cl(60 / SCR_UMOL_PER_MG_DL))
  expect_lt(cl(2.0), cl(1.0))
  # the non-renal term remains however poor the kidneys
  lbw <- 0.9 * 15 + 45
  expect_gt(cl(20), 0.15 * lbw^0.75 / 60)
})

test_that("height is held at 152 cm, and a child takes its weight", {
  short <- carboplatinDuffull(50, 140, 40, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(short$v1, 0.26 * min(leanBodyWeightPesola(152, "female"), 50))
  child <- carboplatinDuffull(15, 100, 4, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(child$v1, 0.26 * 15)
  for (args in list(list(3, 50, 0, "female"), list(250, 210, 90, "male"))) {
    m <- do.call(carboplatin, args)$PK$default
    expect_true(all(is.finite(unlist(m[c("v1", "v2", "cl1", "cl2")]))))
    expect_true(m$cl1 > 0 && m$v1 > 0)
  }
})

test_that("the switch reaches only the weight Cockcroft-Gault compares with LBW", {
  # an adult whose weight exceeds LBW: the switch changes nothing
  on  <- carboplatinDuffull(80, 170, 50, "male", adjustToFFM = TRUE)$PK$default
  off <- carboplatinDuffull(80, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(on, off)
})

test_that("a dose over its AUC is the model's clearance (Calvert's identity)", {
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "carboplatin", Time = c(0, 30), Dose = c(500 / 30, 0),
               Units = "mg/min"),
    data.frame(Time = double(), Event = character()),
    62, 164, 60, "female", 3 * 1440, FALSE, adjustToFFM = FALSE)$carboplatin$wide
  auc <- sum(diff(wide$Time) * (head(wide$Plasma, -1) + tail(wide$Plasma, -1)) / 2)  # mg min/L
  cl <- getDrugPK("carboplatin", 62, 164, 60, "female", adjustToFFM = FALSE)$PK$default$cl1
  expect_equal(500 / auc, cl, tolerance = 0.01)
})

test_that("carboplatin is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "carboplatin", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("carboplatin", 62, 164, 60, "female")
  expect_equal(PK$tPeak, 0)
  expect_identical(PK$Concentration.Units, "mcg")
  expect_match(PK$reference, "Duffull")
  expect_identical(therapeuticRange("carboplatin")$band_status, "not_applicable")
})
