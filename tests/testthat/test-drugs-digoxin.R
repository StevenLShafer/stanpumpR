# Digoxin (R/drugs_digoxin.R): Yukawa 2001's apparent two-compartment model,
# offered as a constant oral daily rate.  Drafted by Claude Code, 2026-10-19.
#
# The expectations are built from Table IV and Table VI of the published
# article, independently of the drug file.

# Yukawa E et al., Clin Pharmacokinet 2001;40:773-781, Table IV
TH1 <- 0.036; TH5 <- 0.112          # L/h per kg, L/h per mL/min
V1 <- 1.83; Q <- 0.629; V2 <- 22.6  # L/kg, L/h/kg, L/kg

cg <- function(weight, age, sex, scr)
  (140 - age) * weight / (72 * scr) * if (sex == "female") 0.85 else 1

events <- data.frame(Time = double(), Event = character())

test_that("the parameters are Table IV's, at total body weight with the switch off", {
  scr <- 1.0
  set <- getDrugPK("digoxin", 50, 158, 60, "female", adjustToFFM = FALSE,
                   creatinine = scr)$PK$default
  crcl <- cg(50, 60, "female", scr)
  expect_equal_rounded((TH1 * 50 + TH5 * crcl) / 60, set$cl1)
  expect_equal_rounded(V1 * 50, set$v1)
  expect_equal_rounded(V2 * 50, set$v2)
  expect_equal_rounded(Q * 50 / 60, set$cl2)
  expect_equal(set$cl3, 0)
})

test_that("Table VI's clearances and maintenance doses are reproduced", {
  # 70 kg, no spironolactone or calcium antagonist: 13.72 L/h at CLcr 100
  # and 3.08 at CLcr 5; the dose for an average 1 mcg/L is CL x 24, 329 and
  # 74 mcg/day.  Choose the creatinine that gives each CLcr for a 70 kg man.
  for (row in list(c(100, 13.72, 329), c(5, 3.08, 74))) {
    scr <- (140 - 60) * 70 / (72 * row[1])
    set <- digoxin(70, 175, 60, "male", adjustToFFM = FALSE,
                   creatinine = scr)$PK$default
    expect_equal(round(set$cl1 * 60, 2), row[2])
    expect_equal(round(set$cl1 * 60 * 24), row[3])
  }
})

test_that("a constant daily rate reaches rate / clearance", {
  scr <- (140 - 60) * 70 / (72 * 100)
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "digoxin", Time = 0, Dose = 0.329, Units = "mg/day PO"),
    events, 70, 175, 60, "male", 60 * 24 * 60, FALSE, adjustToFFM = FALSE,
    creatinine = scr)$digoxin$wide
  # Table VI's dose for 1 mcg/L, after sixty days
  expect_equal(round(utils::tail(wide$Plasma, 1), 2), 1.00)
})

test_that("only the oral daily rate is offered, with the paper's therapeutic range", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "digoxin", ]
  expect_setequal(unlist(row$Units), "mg/day PO")
  expect_equal(row$Lower, 0.5)
  expect_equal(row$Upper, 2)
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  PK <- getDrugPK("digoxin", 70, 175, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_identical(PK$Concentration.Units, "ng")
})

test_that("with the switch on, weight and Cockcroft-Gault use the pharmacokinetic weight", {
  size <- pkSizeFactors(110, 175, 60, "male", TRUE)
  set <- digoxin(110, 175, 60, "male", adjustToFFM = TRUE,
                 creatinine = 1)$PK$default
  w <- size$pkWeight
  expect_equal_rounded((TH1 * w + TH5 * cg(w, 60, "male", 1)) / 60, set$cl1)
  expect_equal_rounded(V1 * w, set$v1)
})
