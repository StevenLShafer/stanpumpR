# Olmesartan (R/drugs_olmesartan.R): Yoshihara 2005's apparent oral
# two-compartment model.  Drafted by Claude Code, 2026-10-20.
#
# The expectations are written from the paper's Table IV and Equation 5
# independently of the drug file, in the paper's own units (L/h, /h, h,
# umol/L), and converted here.  Yoshihara K, Gao Y, Shiga H, Wada DR,
# Hisaoka M, Clin Pharmacokinet 2005;44:1329-1342.
#
# The strongest checks in this file are the paper's OWN sensitivity
# statements, in its Model Diagnostics section: six percentage changes it
# quotes for the covariates, plus the two typical clearances it prints.  Those
# are the paper reporting its own model back to itself, so reproducing them
# exercises the whole covariate equation rather than one coefficient at a time.

CL_F    <- 7.320    # L/h   Table IV
KE      <- 0.193    # /h
K12     <- 0.061    # /h
K21     <- 0.079    # /h
KA      <- 1.460    # /h
ALAG    <- 0.427    # h
K_AGE   <- -0.117
K_WT    <-  0.310
K_SCR   <- -0.344
K_SEX   <-  0.145
K_PV    <- -0.239
K_RACE  <- -0.030   # NOT applied
AGE_REF <- 50       # years
WT_REF  <- 70       # kg
SCR_REF <- 80       # umol/L
UMOL    <- 88.4     # umol/L per mg/dL

# Equation 5, written out here rather than called from the package.  Patient
# status is 1 (hypertensive) and race 0 (Westerner), as the model applies them.
yoshiharaCL <- function(wt, crUmol, age, sex)
  CL_F *
    (max(age, 18) / AGE_REF)^K_AGE *
    (wt / WT_REF)^K_WT *
    (crUmol / SCR_REF)^K_SCR *
    exp(K_SEX * (sex == "male")) *
    exp(K_PV * 1)

at <- function(wt = 70, cr = SCR_REF, age = AGE_REF, sex = "male")
  getDrugPK("olmesartan", wt, 175, age, sex, adjustToFFM = FALSE,
            creatinine = cr / UMOL)

test_that("the paper's own typical clearances are reproduced", {
  # "For a typical 50-year-old, 60kg male (female) Westerner hypertensive
  # patient with a SCr of 80 umol/L, CL/F was 6.66 L/h (5.76 L/h)."  The
  # 60 kg in that sentence is a slip: the arithmetic is the 70 kg median the
  # covariate is centred on, as Equation 3 and the demographics require.
  # 7.320 x exp(0.145) x exp(-0.239) = 6.663, and without the sex term 5.764.
  expect_equal(round(at(sex = "male")$PK$default$cl1 * 60, 2), 6.66)
  expect_equal(round(at(sex = "female")$PK$default$cl1 * 60, 2), 5.76)
})

test_that("the six sensitivity statements in the source are reproduced", {
  pctLower <- function(a, b) (1 - a / b) * 100   # a relative to b
  cl <- function(...) at(...)$PK$default$cl1

  # "Population-predicted CL/F for 75-year-old individuals was 4.6% lower
  # compared with 50-year-old individuals."
  expect_equal(round(pctLower(cl(age = 75), cl(age = 50)), 1), 4.6)

  # "CL/F for individuals with a bodyweight of 50kg was 9.9% lower compared
  # with individuals with a bodyweight of 70kg."
  expect_equal(round(pctLower(cl(wt = 50), cl(wt = 70)), 1), 9.9)

  # "CL/F for individuals with an SCr of 265-450 umol/L ... was 33.8-44.8%
  # lower compared with individuals with an SCr of 80 umol/L."
  expect_equal(round(pctLower(cl(cr = 265), cl(cr = 80)), 1), 33.8)
  expect_equal(round(pctLower(cl(cr = 450), cl(cr = 80)), 1), 44.8)

  # "The mean CL/F of olmesartan was 21.3% lower in hypertensive patients
  # than healthy volunteers" -- the term this model applies as 1, so the
  # check is on the coefficient rather than on two patients.
  expect_equal(round((1 - exp(K_PV)) * 100, 1), 21.3)

  # "13.5% lower in women than men"
  expect_equal(round(pctLower(cl(sex = "female"), cl(sex = "male")), 1), 13.5)

  # "3.0% lower in Japanese than Westerners" -- the term that is NOT applied,
  # pinned so the size of what is left out is on the record.
  expect_equal(round((1 - exp(K_RACE)) * 100, 1), 3.0)
})

test_that("each covariate term matches Equation 5 written out independently", {
  for (args in list(list(wt = 70, cr = 80,  age = 50, sex = "male"),
                    list(wt = 50, cr = 200, age = 75, sex = "female"),
                    list(wt = 110, cr = 45, age = 25, sex = "male"),
                    list(wt = 85, cr = 350, age = 87, sex = "female"))) {
    got <- do.call(at, args)$PK$default$cl1 * 60
    want <- yoshiharaCL(args$wt, args$cr, args$age, args$sex)
    expect_equal_rounded(want, got)
  }
})

test_that("the disposition is the source's parameterisation, not a refit", {
  set <- at()$PK$default
  clF <- set$cl1 * 60
  # V1 is not a fitted parameter: it is CL/F over ke (see the file header)
  expect_equal_rounded(clF / KE, set$v1)
  # and the peripheral compartment follows from k12 and k21
  expect_equal_rounded(K12 * (clF / KE), set$cl2 * 60)
  expect_equal_rounded(K12 * (clF / KE) / K21, set$v2)
  expect_equal(set$v3, 1)       # two compartments
  expect_equal(set$cl3, 0)
  # Absorption, its lag, and no second application of bioavailability
  expect_equal_rounded(KA, set$ka_PO * 60)
  expect_equal_rounded(ALAG * 60, set$tlag_PO)
  expect_equal(set$bioavailability_PO, 1)
})

test_that("the covariates scale the curve without changing its shape", {
  # The consequence of V1 = CL/ke with every covariate on CL: the three
  # eigenvalues of the disposition and the absorption rate are identical for
  # every patient, so a covariate is a pure multiplier on concentration.
  rates <- function(set)
    c(set$cl1 / set$v1,            # ke
      set$cl2 / set$v1,            # k12
      set$cl2 / set$v2,            # k21
      set$ka_PO, set$tlag_PO)
  ref <- rates(at()$PK$default)
  for (args in list(list(wt = 45, cr = 300, age = 80, sex = "female"),
                    list(wt = 120, cr = 50, age = 20, sex = "male"))) {
    expect_equal(rates(do.call(at, args)$PK$default), ref)
  }
  # ke in particular is the published 0.193 /h for everybody
  expect_equal_rounded(KE, ref[1] * 60)
})

test_that("the age term is floored, so a child is not given infinite clearance", {
  # (age/50)^-0.117 is infinite at age 0, which is a legal entry.  Age also
  # reaches the creatinine term, through adultEquivalentCreatinine(), so two
  # ages never give the same clearance; what the floor fixes is the AGE TERM,
  # which is therefore what this checks, by building the whole equation here.
  expected <- function(wt, cr, age, sex) {
    scr <- adultEquivalentCreatinine(cr, age, sex) * UMOL
    yoshiharaCL(wt, scr, age, sex)
  }
  for (age in c(0.5, 2, 10, 18, 40)) {
    got <- getDrugPK("olmesartan", 20, 120, age, "male", adjustToFFM = FALSE,
                     creatinine = 0.5)$PK$default$cl1 * 60
    expect_true(is.finite(got), info = paste("age", age))
    expect_equal_rounded(expected(20, 0.5, age, "male"), got)
  }
})

test_that("a blank creatinine means the assumed normal value", {
  blank <- getDrugPK("olmesartan", 70, 175, 50, "male", adjustToFFM = FALSE)
  named <- getDrugPK("olmesartan", 70, 175, 50, "male", adjustToFFM = FALSE,
                     creatinine = assumedCreatinine(50, "male"))
  expect_equal(blank$PK$default$cl1, named$PK$default$cl1)
})

test_that("the fat-free-mass switch moves the one size term and nothing else", {
  # Off: the published equation at total bodyweight, exactly as printed
  off <- at(wt = 110)$PK$default
  expect_equal_rounded(yoshiharaCL(110, SCR_REF, AGE_REF, "male"),
                       off$cl1 * 60)
  # On: the same equation at the pharmacokinetic weight, which for a heavy
  # patient is lower than total weight, so clearance must fall
  on <- getDrugPK("olmesartan", 110, 175, 50, "male", adjustToFFM = TRUE,
                  creatinine = SCR_REF / UMOL)$PK$default
  expect_lt(on$cl1, off$cl1)
  size <- pkSizeFactors(110, 175, 50, "male", TRUE)
  expect_equal_rounded(yoshiharaCL(size$pkWeight, SCR_REF, AGE_REF, "male"),
                       on$cl1 * 60)
  # and the library factors are NOT applied on top of the model's own term,
  # which would scale size twice
  expect_false(isTRUE(all.equal(on$cl1 * 60,
                                yoshiharaCL(size$pkWeight, SCR_REF, AGE_REF,
                                            "male") * size$clearance)))
})

test_that("a dose is the medoxomil prodrug, entered as prescribed", {
  # The source normalised its data to a 20 mg olmesartan medoxomil dose, so
  # the apparent parameters carry the 20% molecular-weight difference and the
  # dose must NOT be converted.  There is therefore no molar block.
  PK <- at()
  expect_null(PK$molar)
  expect_match(PK$reference, "entered as olmesartan medoxomil", fixed = TRUE)
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "olmesartan"])
  expect_identical(units, c("mg PO", "mg PO qd"))  # no IV unit: apparent in F
})

test_that("it is plasma only, with no effect site, band or blood pressure", {
  PK <- at()
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(PK$PK$default$ke0, 0)
  expect_equal(PK$MEAC, 0)
  dd <- getDrugDefaultsGlobal()
  expect_identical(dd$Category[dd$Drug == "olmesartan"], "Antihypertensives")
  expect_equal(dd$MEAC[dd$Drug == "olmesartan"], 0)
  expect_equal(dd$Lower[dd$Drug == "olmesartan"], 0)
  expect_equal(dd$Upper[dd$Drug == "olmesartan"], 0)
})
