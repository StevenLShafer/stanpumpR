# Milrinone in children (R/drugs_milrinonePediatric.R): Hornik 2019's one
# compartment with allometric weight, maturation and creatinine clearance.
# Drafted by Claude Code, 2026-10-19.

# Hornik 2019, Table 2
cl70 <- 15.9; v70 <- 32.2; tm50 <- 67.7; hill <- 1.12; crclExp <- 0.522

hornikCL <- function(wt, pma, crcl)
  cl70 * (wt / 70)^0.75 * pma^hill / (tm50^hill + pma^hill) * (crcl / 117)^crclExp

test_that("clearance and volume are the final model's, from the bedside Schwartz CrCl", {
  # A 2-year-old, 13 kg, 88 cm, creatinine 0.3 mg/dL
  crcl <- 0.413 * 88 / 0.3
  pma  <- 40 + 52 * 2
  set <- milrinonePediatric(13, 88, 2, "male", adjustToFFM = FALSE,
                            creatinine = 0.3)$PK$default
  expect_equal_rounded(hornikCL(13, pma, crcl) / 60, set$cl1)
  expect_equal_rounded(v70 * 13 / 70, set$v1)
  expect_equal(set$cl2, 0)
})

test_that("a 70 kg adult at the median CrCl is 97% mature, 15.5 of the 15.9 L/h", {
  # The maturation at 1600 weeks PMA (the paper's 30-year-old) is 0.972
  expect_equal(round(hornikCL(70, 1600, 117), 1), 15.5)
})

test_that("renal function is clamped to the studied 13-261 mL/min/1.73 m2", {
  lo <- milrinonePediatric(13, 88, 2, "male", adjustToFFM = FALSE,
                           creatinine = 10)$PK$default
  expect_equal_rounded(hornikCL(13, 144, 13.1) / 60, lo$cl1)
  hi <- milrinonePediatric(13, 88, 2, "male", adjustToFFM = FALSE,
                           creatinine = 0.05)$PK$default
  expect_equal_rounded(hornikCL(13, 144, 261.3) / 60, hi$cl1)
})

test_that("the renal estimate is bedside Schwartz at every age, with no step at 18", {
  # Hornik fitted the covariate on bedside Schwartz; switching equations at 18
  # made clearance jump 24% on the birthday (adversarial review, 2026-10-24)
  crcl <- 0.413 * 175 / 1.0
  set <- milrinonePediatric(70, 175, 40, "male", adjustToFFM = FALSE,
                            creatinine = 1.0)$PK$default
  expect_equal_rounded(hornikCL(70, 40 + 52 * 40, crcl) / 60, set$cl1)
  for (switch in c(FALSE, TRUE)) {
    before <- milrinonePediatric(70, 175, 17.999, "male", adjustToFFM = switch,
                                 creatinine = 1.0)$PK$default
    after  <- milrinonePediatric(70, 175, 18.001, "male", adjustToFFM = switch,
                                 creatinine = 1.0)$PK$default
    expect_lt(abs(after$cl1 / before$cl1 - 1), 0.005)
    expect_lt(abs(after$v1 / before$v1 - 1), 0.005)
  }
})

test_that("a blank creatinine is an assumed normal one, and getDrugPK passes it", {
  blank <- milrinonePediatric(13, 88, 2, "male", adjustToFFM = FALSE)$PK$default
  given <- milrinonePediatric(13, 88, 2, "male", adjustToFFM = FALSE,
                              creatinine = assumedCreatinine(2, "male"))$PK$default
  expect_equal(blank$cl1, given$cl1)
  PK <- getDrugPK("milrinonePediatric", 13, 88, 2, "male", adjustToFFM = FALSE,
                  creatinine = 0.6)$PK$default
  direct <- milrinonePediatric(13, 88, 2, "male", adjustToFFM = FALSE,
                               creatinine = 0.6)$PK$default
  expect_equal(PK$cl1, direct$cl1)
})

test_that("weight is the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(60, 160, 14, "female", TRUE)
  on <- milrinonePediatric(60, 160, 14, "female", adjustToFFM = TRUE,
                           creatinine = 0.6)$PK$default
  expect_equal_rounded(v70 * size$pkWeight / 70, on$v1)
})

test_that("it is plasma only, with the 100-300 ng/mL target range", {
  PK <- getDrugPK("milrinonePediatric", 13, 88, 2, "male")
  expect_equal(PK$tPeak, 0)
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "milrinonePediatric", ]
  expect_equal(c(row$Lower, row$Upper), c(100, 300))
})

test_that("a loading dose and an infusion simulate to the one-compartment solution", {
  # End to end through simulateDrugsWithCovariates() (adversarial review,
  # 2026-10-27).  Hornik's simulated regimen, 50 mcg/kg then 0.5 mcg/kg/min,
  # in a 2-year-old of 13 kg; ng/mL = mcg/L.
  set <- getDrugPK("milrinonePediatric", 13, 88, 2, "male", adjustToFFM = FALSE,
                   creatinine = 0.3)$PK$default
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "milrinonePediatric", Time = c(0, 0),
               Dose = c(50, 0.5), Units = c("mcg/kg", "mcg/kg/min")),
    data.frame(Time = double(), Event = character()),
    13, 88, 2, "male", 720, FALSE, creatinine = 0.3,
    adjustToFFM = FALSE)$milrinonePediatric$wide
  # Every point of the simulated time line after the dose
  times <- wide$Time[wide$Time > 0]
  got <- wide$Plasma[wide$Time > 0]
  expect_gt(length(times), 20)
  k <- set$cl1 / set$v1
  ref <- 50 * 13 / set$v1 * exp(-k * times) +
    0.5 * 13 / set$cl1 * (1 - exp(-k * times))
  expect_lt(max(abs(got / ref - 1)), 1e-6)
  expect_equal(ref, mammillaryReference(set, times, 50 * 13, 0.5 * 13))
})
