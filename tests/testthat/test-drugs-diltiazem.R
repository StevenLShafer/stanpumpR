# Diltiazem (R/drugs_diltiazem.R): Hoglund 1988's two-compartment means in ten
# healthy volunteers.  Drafted by Claude Code, 2026-10-18.
#
# The expectations re-derive the model from the five numbers in the abstract
# independently of the drug file, and use the one the model is not built
# from as the check.

# Hoglund P, Nilsson LG, Ther Drug Monit 1988;10:401-409
CL  <- 0.992     # L/min
V1  <- 119       # L
VSS <- 209       # L
TA  <- 0.40 * 60 # min
TB  <- 2.77 * 60 # min

events <- data.frame(Time = double(), Event = character())

test_that("the decomposition gives the published rates, clearance and central volume", {
  PK <- getDrugPK("diltiazem", 70, 175, 35, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CL, set$cl1)
  expect_equal_rounded(V1, set$v1)
  expect_equal_rounded(TA, log(2) / set$lambda_1)
  expect_equal_rounded(TB, log(2) / set$lambda_2)
  l1 <- log(2) / TA
  l2 <- log(2) / TB
  k10 <- CL / V1
  k21 <- l1 * l2 / k10
  k12 <- l1 + l2 - k10 - k21
  expect_equal_rounded(k12 * V1, set$cl2)
  expect_equal_rounded(k12 * V1 / k21, set$v2)
  expect_equal_rounded(84.6, round(set$v2, 1))
  expect_equal(set$cl3, 0)
  expect_identical(PK$Concentration.Units, "ng")
})

test_that("a 2-minute infusion peaks at the effect site 4.3 min after its start (Salerno 1989)", {
  PK <- getDrugPK("diltiazem", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 3.1)
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "diltiazem", Time = c(0, 2), Dose = c(0.25 * 70 / 2 * 60, 0),
               Units = "mg/hr"),
    data.frame(Time = double(), Event = character()),
    70, 170, 60, "male", 20, FALSE)$diltiazem$wide
  expect_lt(abs(wide$Time[which.max(wide$`Effect Site`)] - 4.3), 0.25)
})

test_that("the printed steady-state volume, which the model does not use, checks it", {
  p <- twoCompartmentFromCentralVolume(CL, V1, TA, TB)
  vss <- p$v1 + p$v2
  expect_equal_rounded(203.6, round(vss, 1))
  expect_lt(abs(vss / VSS - 1), 0.03)
})

test_that("fixed parameters stand with the switch off and scale to fat-free mass with it on", {
  off <- diltiazem(110, 175, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(CL, off$cl1)
  expect_equal_rounded(V1, off$v1)
  size <- pkSizeFactors(110, 175, 50, "male", TRUE)
  on <- diltiazem(110, 175, 50, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(CL * size$clearance, on$cl1)
  expect_equal_rounded(V1 * size$volume, on$v1)
})

test_that("the source's own infusion peaks below dose over the central volume", {
  # 20 mg over 20 minutes: the concentration at the end of the infusion must
  # lie below what the whole dose in V1 would give (168 ng/mL), because drug
  # leaves the central compartment while it runs
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "diltiazem", Time = c(0, 20), Dose = c(1, 0),
               Units = "mg/min"),
    events, 70, 175, 35, "male", 120, FALSE, adjustToFFM = FALSE)$diltiazem$wide
  peak <- max(wide$Plasma)
  expect_lt(peak, 20 / V1 * 1000)
  expect_equal(wide$Time[which.max(wide$Plasma)], 20)
})

test_that("diltiazem is a vasoactive and resuscitation drug with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "diltiazem", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
  expect_false(any(grepl("PO", unlist(row$Units))))
})

test_that("healthy volunteers' 24-hour infusions (Weir 1989, quoted by Dias 1992) agree", {
  # Dias VC et al., Circulation 1992;86:1421, Discussion: in normal volunteers
  # given 10 and 15 mg/h for 24 hours, the mean maximum concentrations were
  # 170 and 270 ng/mL.  The model never saw these.  (Dias's own atrial
  # fibrillation patients reached 242 and 470: lower clearance with age and
  # disease, and a clearance that falls at the higher rate.)
  peak <- function(rate) {
    wide <- simulateDrugsWithCovariates(
      data.frame(Drug = "diltiazem", Time = c(0, 1440), Dose = c(rate, 0),
                 Units = "mg/hr"),
      events, 70, 175, 35, "male", 1500, FALSE, adjustToFFM = FALSE)$diltiazem$wide
    max(wide$Plasma)
  }
  expect_lt(abs(peak(10) / 170 - 1), 0.05)
  expect_lt(abs(peak(15) / 270 - 1), 0.10)
})
