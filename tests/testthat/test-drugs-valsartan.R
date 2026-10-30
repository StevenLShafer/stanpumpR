# Valsartan (R/drugs_valsartan.R): Heo 2016's apparent oral two-compartment
# model with a zero-order release.  Drafted by Claude Code, 2026-10-20.
#
# The expectations are written from the paper's Supplementary Table S4 and its
# Equations 1 and 2 independently of the drug file, in the paper's own units
# (L/h, L, h at a 70 kg standard), and converted here.  Heo YA, Holford N,
# Kim Y, Son M, Park K, Br J Clin Pharmacol 2016;82:1557-1567.

CL_F  <- 6.18     # L/h per 70 kg   Supplementary Table S4
V1_F  <- 25.9     # L   per 70 kg
Q_F   <- 2.01     # L/h per 70 kg
V2_F  <- 17.4     # L   per 70 kg
D1_H  <- 4.39     # h, zero-order release duration
WTSTD <- 70       # kg, the paper's standard individual

noEvents <- data.frame(Time = numeric(0), Event = character(0))

at <- function(wt = WTSTD, ffm = FALSE, ht = 175, age = 30, sex = "male")
  getDrugPK("valsartan", wt, ht, age, sex, adjustToFFM = ffm)

test_that("Supplementary Table S4 is reproduced at the 70 kg standard", {
  set <- at()$PK$default
  expect_equal_rounded(CL_F, set$cl1 * 60)
  expect_equal_rounded(V1_F, set$v1)
  expect_equal_rounded(Q_F,  set$cl2 * 60)
  expect_equal_rounded(V2_F, set$v2)
  expect_equal(set$v3, 1)       # two compartments
  expect_equal(set$cl3, 0)
})

test_that("absorption is zero order over the fitted duration, with no ka", {
  PK <- at()
  # The release the engine delivers each oral dose through, in minutes
  expect_equal_rounded(D1_H * 60, PK$oralZeroOrder$duration)
  expect_equal(PK$oralZeroOrder$lag, 0)     # none was fitted
  # A zero-order release carries no absorption rate constant and no separate
  # bioavailability: both are inside the apparent scale (R/oral-zero-order.R)
  expect_equal(PK$PK$default$ka_PO, 0)
  expect_equal(PK$PK$default$bioavailability_PO, 0)
})

test_that("the peak is at the end of the release, which is what zero order means", {
  dt <- data.frame(Drug = "valsartan", Time = 0, Dose = 160, Units = "mg PO",
                   stringsAsFactors = FALSE)
  w <- simulateDrugsWithCovariates(dt, noEvents, WTSTD, 175, 30, "male", 2880,
                                   FALSE, adjustToFFM = FALSE)$valsartan$wide
  tmaxHours <- w$Time[which.max(w$Plasma)] / 60
  # Exactly D1: a constant-rate input into a linear system peaks when the
  # input stops.  This is the model's own prediction and the thing the
  # label's 2-4 h disagrees with, so it is pinned rather than approximated.
  expect_equal(tmaxHours, D1_H, tolerance = 1e-6)
  # The area under the curve is the dose over the clearance, which is the
  # part of the model the source's own visual predictive check supports
  auc <- sum(diff(w$Time) *
               (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  expect_equal(auc, 160000 / CL_F, tolerance = 0.02)   # ng*h/mL, 48 h vs inf
})

test_that("the source's allometry is the library's, so the switch off is exact", {
  # Equations 1 and 2: weight^0.75 on CL/F and Q/F, weight^1 on V1/F and V2/F
  for (wt in c(45, 68.7, 70, 110)) {
    set <- at(wt = wt)$PK$default
    expect_equal_rounded(CL_F * (wt / WTSTD)^0.75, set$cl1 * 60)
    expect_equal_rounded(Q_F  * (wt / WTSTD)^0.75, set$cl2 * 60)
    expect_equal_rounded(V1_F * (wt / WTSTD),      set$v1)
    expect_equal_rounded(V2_F * (wt / WTSTD),      set$v2)
  }
  # On: the same exponents applied to fat-free mass instead of total weight
  size <- pkSizeFactors(110, 175, 30, "male", TRUE)
  on <- at(wt = 110, ffm = TRUE)$PK$default
  expect_equal_rounded(CL_F * size$clearance, on$cl1 * 60)
  expect_equal_rounded(V1_F * size$volume,    on$v1)
  # A heavy patient's fat-free mass is below total weight, so both must fall
  off <- at(wt = 110)$PK$default
  expect_lt(on$cl1, off$cl1)
  expect_lt(on$v1,  off$v1)
})

test_that("the release duration does not scale with size", {
  # The paper's two size equations name clearance and volume only, and a
  # release duration is not a size-dependent quantity.
  for (wt in c(45, 70, 110))
    for (ffm in c(FALSE, TRUE))
      expect_equal(at(wt = wt, ffm = ffm)$oralZeroOrder$duration, D1_H * 60)
})

test_that("there is no renal term, and the source says why", {
  # "Apart from body weight, no other covariate factors were found to be
  # significant (P > 0.001)" -- age, serum creatinine, smoking and drinking
  # were all screened.  So the signature must not name creatinine, which is
  # how getDrugPK() decides whether to pass it.
  expect_false("creatinine" %in% names(formals(valsartan)))
  severe <- getDrugPK("valsartan", 70, 175, 30, "male", adjustToFFM = FALSE,
                      creatinine = 4)$PK$default
  expect_equal(severe$cl1, at()$PK$default$cl1)
  # Nor age, the other continuous covariate the paper rejected
  expect_equal(at(age = 80)$PK$default$cl1, at(age = 20)$PK$default$cl1)
})

test_that("it is plasma only, with no effect site, band or blood pressure", {
  # The source DID fit a blood-pressure model (an effect compartment with an
  # Imax of 0.164) and this library still does not plot a pressure.
  PK <- at()
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(PK$PK$default$ke0, 0)
  expect_equal(PK$MEAC, 0)
  dd <- getDrugDefaultsGlobal()
  expect_identical(dd$Category[dd$Drug == "valsartan"], "Antihypertensives")
  expect_equal(dd$MEAC[dd$Drug == "valsartan"], 0)
  expect_equal(dd$Lower[dd$Drug == "valsartan"], 0)
  expect_equal(dd$Upper[dd$Drug == "valsartan"], 0)
  # Apparent in F, so no intravenous unit
  expect_identical(unlist(dd$Units[dd$Drug == "valsartan"]),
                   c("mg PO", "mg PO qd"))
})
