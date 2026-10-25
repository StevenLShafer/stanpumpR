# Benazepril (R/drugs_benazepril.R): the plotted moiety is benazeprilat, from a
# vector DERIVED from Kaiser 1989's non-compartmental Table 2.
# Drafted by Claude Code, 2026-10-20.
#
# The derivation is the thing under test, so this file redoes it from the
# paper's four printed numbers, independently of the drug file, and then checks
# the drug against it.  Kaiser G, Ackermann R, Brechbuhler S, Dieterle W,
# Biopharm Drug Dispos 1989;10:365-376, Table 2.

MW_HCL    <- 460.96     # benazepril hydrochloride, g/mol
MW_PRILAT <- 396.44     # benazeprilat free acid, g/mol
T_ALPHA   <- 2.7        # h, Table 2
T_BETA    <- 22.3       # h, Table 2
AUC_INF   <- 3185       # (pmol/g).h, Table 2
AUC_24    <- 2649       # (pmol/g).h, Table 2
CMAX_OBS  <- 492        # pmol/g, Table 2
TMAX_OBS  <- 1.5        # h, Table 2 (median)
CLR_OBS   <- 22.7       # mL/min, Table 2
DOSE_MG   <- 10         # mg benazepril.HCl

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Kaiser's Table 2 decomposed here, from scratch
kaiser <- local({
  dosePmol <- DOSE_MG / 1000 / MW_HCL * 1e12
  al <- log(2) / T_ALPHA
  be <- log(2) / T_BETA
  # The tail beyond 24 h is a single exponential, so its area gives C(24)
  C24 <- be * (AUC_INF - AUC_24)
  B   <- C24 * exp(be * 24)
  A   <- (AUC_INF - B / be) * al
  k21 <- (A * be + B * al) / (A + B)
  k10 <- al * be / k21
  k12 <- al + be - k21 - k10
  V1  <- dosePmol / ((A + B) * 1000)        # L, apparent in fm
  r   <- MW_HCL / MW_PRILAT                 # salt and moiety correction
  list(alpha = al, beta = be, A = A, B = B, C24 = C24,
       k10 = k10, k12 = k12, k21 = k21,
       v1 = V1 * r, v2 = V1 * k12 / k21 * r,
       cl = V1 * k10 * r, q = V1 * k12 * r)
})

at <- function(ffm = FALSE, wt = 70)
  getDrugPK("benazepril", wt, 175, 30, "male", adjustToFFM = ffm)

test_that("the derivation from Table 2 is valid: every rate constant is positive", {
  # A decomposition that returns a negative rate constant is not a model.
  expect_gt(kaiser$k10, 0)
  expect_gt(kaiser$k12, 0)
  expect_gt(kaiser$k21, 0)
  # And the extrapolated intercept must EXCEED the measured peak, since
  # absorption and formation delay the real peak past t = 0
  expect_gt(kaiser$A + kaiser$B, CMAX_OBS)
})

test_that("the drug carries exactly the derived vector", {
  set <- at()$PK$default
  expect_equal_rounded(kaiser$v1, set$v1, tolerance = 1e-3)
  expect_equal_rounded(kaiser$v2, set$v2, tolerance = 1e-3)
  expect_equal_rounded(kaiser$cl, set$cl1 * 60, tolerance = 1e-3)
  expect_equal_rounded(kaiser$q,  set$cl2 * 60, tolerance = 1e-3)
  expect_equal(set$v3, 1)       # two compartments
  expect_equal(set$cl3, 0)
  expect_equal(set$bioavailability_PO, 1)   # apparent in fm already
  expect_equal(set$tlag_PO, 0)              # none was fitted
})

test_that("the disposition reproduces BOTH of the paper's half-lives", {
  # This is what the derivation exists to do, and it is the check that
  # distinguishes benazeprilat from losartan, whose source prints one AUC and
  # therefore cannot supply the fourth number (docs/antihypertensives.md 15).
  set <- at()$PK$default
  halves <- sort(log(2) / unlist(set[c("lambda_1", "lambda_2")]) / 60)  # hours
  expect_equal(round(halves[[1]], 2), T_ALPHA)
  expect_equal(round(halves[[2]], 2), T_BETA)
})

test_that("two internal checks of the derivation hold", {
  set <- at()$PK$default
  clF <- set$cl1 * 60
  # (1) the apparent clearance must equal dose over area, on the molar basis
  dosePmol <- DOSE_MG / 1000 / MW_HCL * 1e12
  expect_equal_rounded(dosePmol / (AUC_INF * 1000) * MW_HCL / MW_PRILAT, clF,
                       tolerance = 1e-3)
  # (2) the renal fraction it implies is the source's own "about 20 per cent
  # of the dose were excreted as benazeprilat" -- a statement the derivation
  # was NOT given
  clrMolar <- CLR_OBS * 60 / 1000                       # L/h
  clMolar  <- clF * MW_PRILAT / MW_HCL                  # back to the molar basis
  expect_equal(round(100 * clrMolar / clMolar, 0), 20)
})

test_that("the curve reproduces the area exactly and the peak 20% low", {
  dt <- data.frame(Drug = "benazepril", Time = 0, Dose = DOSE_MG,
                   Units = "mg PO", stringsAsFactors = FALSE)
  w <- simulateDrugsWithCovariates(dt, noEvents, 70, 175, 30, "male", 14400,
                                   FALSE, adjustToFFM = FALSE)$benazepril$wide
  auc <- sum(diff(w$Time) *
               (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  # The area is firm: it is dose over the apparent clearance, which the
  # tail-area split does not touch.  Kaiser's AUC(0-inf) in mass units:
  expect_equal(auc, AUC_INF * MW_PRILAT / 1000, tolerance = 0.01)
  # The peak is the soft part, and it is pinned at the value it actually has
  # rather than hidden: 0.80x the reported mean of individual peaks, which
  # that statistic's upward bias over times to peak of 1 to 6 h accounts for.
  cmaxObs <- CMAX_OBS * MW_PRILAT / 1000                # ng/mL
  expect_equal(round(max(w$Plasma) / cmaxObs, 2), 0.80)
})

test_that("the effective input rate lands near the parent's own elimination rate", {
  # It is set from the measured 1.5 h time to peak, NOT from benazepril's
  # half-life -- so landing within 25% of the 1.155 /h that should rate-limit
  # the formation is a corroboration rather than a construction.
  ka <- at()$PK$default$ka_PO * 60
  kParent <- log(2) / 0.6                               # Table 1's 0.6 h
  expect_lt(abs(ka / kParent - 1), 0.25)
})

test_that("no covariate reaches it, because the source fitted none", {
  # 62 healthy young men at one dose level.  In particular there is NO renal
  # term, although benazeprilat is substantially renally cleared: the source
  # cannot speak to it, so the signature must not name creatinine.
  expect_false("creatinine" %in% names(formals(benazepril)))
  expect_equal(getDrugPK("benazepril", 70, 175, 30, "male", adjustToFFM = FALSE,
                         creatinine = 4)$PK$default$cl1,
               at()$PK$default$cl1)
  expect_equal(getDrugPK("benazepril", 70, 175, 85, "female",
                         adjustToFFM = FALSE)$PK$default$cl1,
               at()$PK$default$cl1)
})

test_that("the fat-free-mass switch moves it only through the library factors", {
  off <- at(wt = 110)$PK$default
  expect_equal_rounded(kaiser$v1, off$v1, tolerance = 1e-3)   # unscaled when off
  on  <- at(ffm = TRUE, wt = 110)$PK$default
  size <- pkSizeFactors(110, 175, 30, "male", TRUE)
  expect_equal_rounded(kaiser$v1 * size$volume, on$v1, tolerance = 1e-3)
  expect_gt(on$v1, off$v1)
})

test_that("it is plasma only, with no effect site, band or blood pressure", {
  PK <- at()
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(PK$PK$default$ke0, 0)
  expect_equal(PK$MEAC, 0)
  dd <- getDrugDefaultsGlobal()
  expect_identical(dd$Category[dd$Drug == "benazepril"], "Antihypertensives")
  expect_equal(dd$MEAC[dd$Drug == "benazepril"], 0)
  expect_equal(dd$Lower[dd$Drug == "benazepril"], 0)
  expect_equal(dd$Upper[dd$Drug == "benazepril"], 0)
  # Apparent in fm, so no intravenous unit
  expect_identical(unlist(dd$Units[dd$Drug == "benazepril"]),
                   c("mg PO", "mg PO qd"))
  # The reference must say which moiety is plotted, since it is not the
  # molecule named in the dose table
  expect_match(PK$reference, "THE PLOTTED MOIETY IS BENAZEPRILAT", fixed = TRUE)
})
