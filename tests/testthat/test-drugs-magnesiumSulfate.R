# Magnesium sulfate (R/drugs_magnesiumSulfate.R): Du 2019's two-compartment
# model of the rise in serum magnesium in preeclampsia.  Drafted by Claude
# Code, 2026-10-18.
#
# The expectations are built from the published numbers (Table 3 of the full
# text, PMC6518930) independently of the drug file, and the simulated curve is
# checked against the paper's own simulations (Table 4), which the fit itself
# does not contain.

# Du L et al., J Clin Pharmacol 2019;59:374-385
CL <- 3.72; Q <- 3.66; VC <- 15.4; VP <- 17.0     # L/h and L at 85 kg
CR_EXP <- -0.731
MW <- 246.47                                       # g MgSO4.7H2O per mol Mg
BASELINE <- 0.74                                   # mmol/L, the study median

events <- data.frame(Time = double(), Event = character())

simulateMg <- function(dose, weight = 85, creatinine = 0.8, maximum = 1800) {
  simulateDrugsWithCovariates(dose, events, weight, 161, 30, "female",
                              maximum, FALSE, adjustToFFM = FALSE,
                              creatinine = creatinine)$magnesiumSulfate$wide
}

# The Zuspan regimen: 4 g over 20 minutes, then the maintenance rate for 24 h
zuspan <- function(maintenance)
  data.frame(Drug = "magnesiumSulfate", Time = c(0, 20, 1460),
             Dose = c(12, maintenance, 0), Units = "g/hr")

# Average of the plotted rise over the first 24 hours, by the trapezoid rule
average24 <- function(wide) {
  s <- wide[wide$Time <= 1440, ]
  sum(diff(s$Time) * (utils::head(s$Plasma, -1) + utils::tail(s$Plasma, -1)) / 2) /
    1440
}

test_that("the reference woman has Table 3's parameters, in millimoles", {
  PK <- getDrugPK("magnesiumSulfate", 85, 161, 30, "female",
                  adjustToFFM = FALSE, creatinine = 0.8)
  set <- PK$PK$default
  expect_equal_rounded(CL / 60, set$cl1)
  expect_equal_rounded(Q / 60, set$cl2)
  expect_equal_rounded(VC, set$v1)
  expect_equal_rounded(VP, set$v2)
  expect_equal(set$cl3, 0)
  # Vss 32.4 L at 85 kg, as the Discussion says
  expect_equal(set$v1 + set$v2, 32.4)
  expect_identical(PK$Concentration.Units, "mmol")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
  expect_equal(PK$MEAC, 0)
})

test_that("weight and creatinine enter as published", {
  set <- magnesiumSulfate(60, 160, 30, "female", adjustToFFM = FALSE,
                          creatinine = 1.2)$PK$default
  w <- 60 / 85
  expect_equal_rounded(CL / 60 * w^0.75 * (1.2 / 0.8)^CR_EXP, set$cl1)
  expect_equal_rounded(Q / 60 * w^0.75, set$cl2)
  expect_equal_rounded(VC * w, set$v1)
  expect_equal_rounded(VP * w, set$v2)
  # A blank creatinine is the assumed adult normal: 0.8 for a woman, which is
  # the reference, and 0.9 for a man
  woman <- magnesiumSulfate(85, 161, 30, "female", adjustToFFM = FALSE)$PK$default
  man   <- magnesiumSulfate(85, 175, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL / 60, woman$cl1)
  expect_equal_rounded(CL / 60 * (0.9 / 0.8)^CR_EXP, man$cl1)
})

test_that("with the switch on, every parameter uses the pharmacokinetic weight", {
  size <- pkSizeFactors(120, 165, 30, "female", TRUE)
  set <- magnesiumSulfate(120, 165, 30, "female", adjustToFFM = TRUE,
                          creatinine = 0.8)$PK$default
  w <- size$pkWeight / 85
  expect_equal_rounded(VC * w, set$v1)
  expect_equal_rounded(CL / 60 * w^0.75, set$cl1)
})

test_that("a dose in grams of the salt becomes millimoles of magnesium", {
  # 4 g as a bolus: 4 / 246.47 mol = 16.229 mmol into 15.4 L, a rise of
  # 1.054 mmol/L at time zero.  Read as 4 g of elemental magnesium it would
  # be ten times that, which is what naming the drug for the salt prevents.
  wide <- simulateMg(data.frame(Drug = "magnesiumSulfate", Time = 0, Dose = 4,
                                Units = "g"), maximum = 60)
  expect_equal_rounded(1.0538, round(4 / MW * 1000 / VC, 4))
  expect_equal_rounded(4 / MW * 1000 / VC, wide$Plasma[wide$Time == 0][1],
                       tolerance = 1e-4)
})

test_that("the Zuspan regimen averages the paper's 1.6 mmol/L over 24 hours", {
  # Table 4, note: 4 g over 20 min then 1 g/h for 24 h gives a predicted
  # 24-hour average total magnesium of 1.6 mmol/L in a typical 85 kg woman
  # with creatinine 0.8 mg/dL.  The plotted rise plus the 0.74 mmol/L baseline
  # the paper's simulations used.
  total <- average24(simulateMg(zuspan(1))) + BASELINE
  expect_equal(round(total, 1), 1.6)
  expect_equal_rounded(1.633, round(total, 3), tolerance = 1e-3)
})

test_that("the paper's maintenance-dose adjustments are reproduced", {
  # Table 4: the Zuspan maintenance rate that matches the typical woman's
  # 24-hour average, by weight and creatinine.  Solving for the rate with
  # this model reproduces every corner of the table to its printed 0.1 g/h.
  target <- average24(simulateMg(zuspan(1)))
  solveRate <- function(weight, creatinine)
    stats::uniroot(function(m) average24(simulateMg(zuspan(m), weight,
                                                    creatinine)) - target,
                   c(0.2, 3), tol = 1e-6)$root
  printed <- rbind(
    c(65, 0.5, 1.0), c(65, 0.8, 0.8), c(65, 1.2, 0.6),
    c(105, 0.5, 1.6), c(105, 0.8, 1.2), c(105, 1.2, 1.0)
  )
  for (i in seq_len(nrow(printed))) {
    rate <- solveRate(printed[i, 1], printed[i, 2])
    expect_lt(abs(rate - printed[i, 3]), 0.05 + 1e-9,
              label = paste(printed[i, 1], "kg, creatinine", printed[i, 2]))
  }
})

test_that("magnesium sulfate is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "magnesiumSulfate", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "mmol")
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
  expect_equal(row$Upper, 0)
  # Intravenous only: the source did not study the intramuscular route
  expect_false(any(grepl("IM", unlist(row$Units))))
})
