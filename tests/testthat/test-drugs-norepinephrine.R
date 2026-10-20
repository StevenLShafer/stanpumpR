# Norepinephrine (R/drugs_norepinephrine.R): Li 2024's two-compartment model in
# healthy volunteers, awake.  Drafted by Claude Code, 2026-10-19.
#
# The expectations are built from Table 3, Equations 1-4 and the text of the
# published article, independently of the drug file.

# Li Y et al., Clin Pharmacokinet 2024;63:1597-1608, Table 3
CL <- 2.1; VC <- 2.4; Q <- 0.6; VP <- 3.6      # L/min and L at 70 kg
PROD <- 497.7                                  # ng/min
AGE_CL <- -0.344; CPROP_CL <- -3.57            # per 100
MW <- 169.18                                   # g/mol

events <- data.frame(Time = double(), Event = character())

test_that("the reference adult has Table 3's parameters", {
  PK <- getDrugPK("norepinephrine", 70, 175, 35, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CL, set$cl1)
  expect_equal_rounded(VC, set$v1)
  expect_equal_rounded(Q, set$cl2)
  expect_equal_rounded(VP, set$v2)
  expect_equal(set$cl3, 0)
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
  expect_equal(PK$MEAC, 0)
})

test_that("the covariate coefficients are per 100, as the text's own numbers require", {
  # "patients 10 years older have about 3% lower CL"
  expect_equal(round(1 - exp(AGE_CL / 100 * 10), 2), 0.03)
  # "an expected decrease in CL of 11.8%" at the median propofol, 3.53 mcg/mL
  expect_equal(round(1 - exp(CPROP_CL / 100 * 3.53), 3), 0.118)
  # Age is applied as published
  old <- norepinephrine(70, 175, 65, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL * exp(AGE_CL / 100 * 30), old$cl1)
})

test_that("production over clearance is the measured baseline", {
  # Equation 1: baseline = Prod / (CL / Vc) in amount, Prod / CL in
  # concentration.  497.7 / 2.1 ng/L is 1.40 nmol/L, the median baseline of
  # the 18-34-year-olds in Table 2.
  expect_equal(round(PROD / CL / MW, 2), 1.40)
  # About 0.24 ng/mL, which is not plotted
  expect_equal_rounded(0.237, round(PROD / CL / 1000, 3))
})

test_that("weight enters allometrically, or as the pharmacokinetic weight", {
  off <- norepinephrine(100, 180, 35, "male", adjustToFFM = FALSE)$PK$default
  w <- 100 / 70
  expect_equal_rounded(CL * w^0.75, off$cl1)
  expect_equal_rounded(Q * w^0.75, off$cl2)
  expect_equal_rounded(VC * w, off$v1)
  expect_equal_rounded(VP * w, off$v2)
  size <- pkSizeFactors(100, 180, 35, "male", TRUE)
  on <- norepinephrine(100, 180, 35, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(CL * (size$pkWeight / 70)^0.75, on$cl1)
  expect_equal_rounded(VC * size$pkWeight / 70, on$v1)
})

test_that("the paper's own simulation is reproduced", {
  # Figure 5, no propofol: 0.12 mcg/kg/min for 15 minutes reaches about
  # 24 nmol/L, read from the figure, including the endogenous baseline.  A
  # 70 kg adult of the simulation dataset's mean age, 42.
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "norepinephrine", Time = c(0, 15), Dose = c(0.12, 0),
               Units = "mcg/kg/min"),
    events, 70, 175, 42, "male", 40, FALSE, adjustToFFM = FALSE)$norepinephrine$wide
  clAt42 <- CL * exp(AGE_CL / 100 * 7)
  total <- (wide$Plasma[wide$Time == 15][1] + PROD / clAt42 / 1000) / MW * 1000
  expect_gt(total, 22)
  expect_lt(total, 27)
  # and falls fast once the infusion stops: by 25 minutes, under a third
  expect_lt(stats::approx(wide$Time, wide$Plasma, 25)$y,
            wide$Plasma[wide$Time == 15][1] / 3)
})

test_that("norepinephrine is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "norepinephrine", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
})
