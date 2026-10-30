# Lisinopril (R/drugs_lisinopril.R): Thomson 1989's apparent oral
# one-compartment model.  Drafted by Claude Code, 2026-10-20.
#
# The expectations are written from the paper's Table 3 independently of the
# drug file, in the paper's own units (L, L/h, /h, umol/L), and converted
# here.  Thomson AH, Kelly JG, Whiting B, Br J Clin Pharmacol 1989;27:57-65.

CL_PER_KG <- 0.251    # L/h per kg      Table 3, theta1
V_F       <- 36.7     # L               Table 3, theta2
KA_H      <- 0.104    # /h              Table 3, theta3
CR_EXP    <- -0.887   #                 Table 3, theta4
AGE_EXP   <- -0.451   #                 Table 3, theta5
CHF       <- 0.645    # NOT applied     Table 3, theta6
CR_REF    <- 70       # umol/L
AGE_REF   <- 65       # years
UMOL      <- 88.4     # umol/L per mg/dL

# Thomson's equation, written out here rather than called from the package
thomsonCL <- function(wt, crUmol, age)
  CL_PER_KG * wt * (crUmol / CR_REF)^CR_EXP *
    (max(age, 18) / AGE_REF)^AGE_EXP

test_that("the published equation is reproduced exactly", {
  # At the paper's own reference: 70 kg, 65 y, creatinine 70 umol/L
  PK <- getDrugPK("lisinopril", 70, 175, 65, "male", adjustToFFM = FALSE,
                  creatinine = CR_REF / UMOL)
  set <- PK$PK$default
  expect_equal_rounded(thomsonCL(70, CR_REF, 65), set$cl1 * 60)
  expect_equal_rounded(CL_PER_KG * 70, set$cl1 * 60)   # both terms are 1 here
  expect_equal_rounded(V_F, set$v1)
  expect_equal_rounded(KA_H, set$ka_PO * 60)
  # One compartment: the second and third are the library's placeholders
  expect_equal(set$cl2, 0)
  expect_equal(set$cl3, 0)
  expect_equal(set$v2, 1)
  expect_equal(set$v3, 1)
})

test_that("the covariate terms move clearance the way the source says", {
  at <- function(wt, cr, age)
    getDrugPK("lisinopril", wt, 175, age, "male", adjustToFFM = FALSE,
              creatinine = cr / UMOL)$PK$default$cl1 * 60

  # Each term in isolation, against the equation written out above
  expect_equal_rounded(thomsonCL(70, 200, 65), at(70, 200, 65))
  expect_equal_rounded(thomsonCL(50, 70, 65),  at(50, 70, 65))
  expect_equal_rounded(thomsonCL(70, 70, 40),  at(70, 70, 40))

  # Direction and rough magnitude, which is what a reader checks
  expect_lt(at(70, 200, 65), at(70, 70, 65))      # worse renal -> lower CL
  expect_gt(at(90, 70, 65), at(70, 70, 65))       # heavier     -> higher CL
  expect_gt(at(70, 70, 40), at(70, 70, 80))       # younger     -> higher CL

  # The paper's figure 3a span: clearance falls several fold as creatinine
  # rises across its studied range (70 to 900 umol/L)
  expect_gt(at(70, 70, 65) / at(70, 900, 65), 8)
  expect_lt(at(70, 70, 65) / at(70, 900, 65), 12)
})

test_that("the age term is floored, so a child is not given infinite clearance", {
  # (age/65)^-0.451 is infinite at age 0, which is a legal entry.  Age also
  # reaches the creatinine term, through adultEquivalentCreatinine(), so two
  # ages never give the same clearance; what the floor fixes is the AGE TERM,
  # which is therefore what this checks, by building the whole equation here.
  expected <- function(wt, cr, age, sex) {
    scr <- adultEquivalentCreatinine(cr, age, sex) * UMOL
    CL_PER_KG * wt * (scr / CR_REF)^CR_EXP *
      (max(age, 18) / AGE_REF)^AGE_EXP
  }
  for (age in c(0.5, 2, 10, 18, 40)) {
    got <- getDrugPK("lisinopril", 20, 120, age, "male", adjustToFFM = FALSE,
                     creatinine = 0.5)$PK$default$cl1 * 60
    expect_true(is.finite(got), info = paste("age", age))
    expect_equal_rounded(expected(20, 0.5, age, "male"), got)
  }
})

test_that("absorption is slower than elimination, so the drug is flip-flop", {
  PK <- getDrugPK("lisinopril", 70, 175, 65, "male", adjustToFFM = FALSE,
                  creatinine = CR_REF / UMOL)
  set <- PK$PK$default
  k  <- set$cl1 / set$v1           # elimination rate constant, /min
  ka <- set$ka_PO                  # absorption rate constant, /min
  expect_lt(ka, k)                 # THE defining property of this model
  expect_gt(k / ka, 4)             # and by more than fourfold
  # So the terminal half-life of the curve is the ABSORPTION half-life
  expect_equal_rounded(log(2) / KA_H, log(2) / (ka * 60))
})

test_that("it is plasma only, with no effect site, band or blood pressure", {
  PK <- getDrugPK("lisinopril", 70, 175, 65, "male", adjustToFFM = FALSE)
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(PK$PK$default$ke0, 0)
  expect_equal(PK$MEAC, 0)
  dd <- getDrugDefaultsGlobal()
  expect_identical(dd$Category[dd$Drug == "lisinopril"], "Antihypertensives")
  expect_equal(dd$MEAC[dd$Drug == "lisinopril"], 0)
  expect_equal(dd$Lower[dd$Drug == "lisinopril"], 0)
  expect_equal(dd$Upper[dd$Drug == "lisinopril"], 0)
})

test_that("a blank creatinine means the assumed normal value", {
  blank <- getDrugPK("lisinopril", 70, 175, 65, "male", adjustToFFM = FALSE)
  named <- getDrugPK("lisinopril", 70, 175, 65, "male", adjustToFFM = FALSE,
                     creatinine = assumedCreatinine(65, "male"))
  expect_equal(blank$PK$default$cl1, named$PK$default$cl1)
})

test_that("the fat-free-mass switch leaves the published equation alone when off", {
  # Off: clearance linear in total weight, volume constant, exactly as printed
  off <- getDrugPK("lisinopril", 100, 175, 65, "male", adjustToFFM = FALSE,
                   creatinine = CR_REF / UMOL)$PK$default
  expect_equal_rounded(thomsonCL(100, CR_REF, 65), off$cl1 * 60)
  expect_equal_rounded(V_F, off$v1)
  # On: the package's own scaling moves both, so they must differ
  on <- getDrugPK("lisinopril", 100, 175, 65, "male", adjustToFFM = TRUE,
                  creatinine = CR_REF / UMOL)$PK$default
  expect_false(isTRUE(all.equal(off$cl1, on$cl1)))
  expect_false(isTRUE(all.equal(off$v1, on$v1)))
})

test_that("the cardiac-failure factor is recorded but not applied", {
  # There is no cardiac failure field, so nothing in the Patient Profile can
  # reach this coefficient; the test pins that it is absent from the model.
  PK <- getDrugPK("lisinopril", 70, 175, 65, "male", adjustToFFM = FALSE,
                  creatinine = CR_REF / UMOL)
  expect_equal_rounded(thomsonCL(70, CR_REF, 65), PK$PK$default$cl1 * 60)
  expect_false(isTRUE(all.equal(thomsonCL(70, CR_REF, 65) * CHF,
                                PK$PK$default$cl1 * 60)))
  # and that the reference string says so, where a reader will see it
  expect_match(PK$reference, "cardiac-failure factor of 0.645 is not applied",
               fixed = TRUE)
})
