# Tamsulosin (R/drugs_tamsulosin.R): Tsuda 2010's apparent oral one-
# compartment model with a lag.  Drafted by Claude Code, 2026-11-19.
#
# The expectations are written from Tsuda 2010's Table 3 (read on the page
# image) and the Flomax label's Table 2 independently of the drug file, in the
# sources' own units (L/h, L, h at a 70 kg standard), and converted here.
# Tsuda Y et al., Br J Clin Pharmacol 2010;70:88-101.  Flomax (tamsulosin
# hydrochloride) capsules, NDA 020579, label s033 (2019).

CL_F  <- 2.28     # L/h at 70 kg and AAG 20.0 uM     Table 3
V_F   <- 37.5     # L
KA    <- 0.368    # 1/h
ALAG  <- 0.957    # h
WTSTD <- 70       # kg

pk <- function(wt = WTSTD, ffm = FALSE, ht = 175, age = 30, sex = "male")
  tamsulosin(wt, ht, age, sex, adjustToFFM = ffm)$PK$default

test_that("Table 3 is reproduced at the 70 kg standard", {
  set <- pk()
  expect_equal_rounded(CL_F, set$cl1 * 60)
  expect_equal_rounded(V_F, set$v1)
  expect_equal_rounded(KA, set$ka_PO * 60)
  expect_equal_rounded(ALAG * 60, set$tlag_PO)
  expect_equal(set$bioavailability_PO, 1)   # apparent in F
  expect_equal(c(set$v2, set$v3, set$cl2, set$cl3), c(1, 1, 0, 0))  # one compartment
})

test_that("the source's allometry is the library's, so the switch off is exact", {
  # CL/F x (WT/70)^0.75, V/F x (WT/70); ka and the lag do not scale
  for (wt in c(12.1, 25, 70, 92.2)) {
    set <- pk(wt = wt)
    expect_equal_rounded(CL_F * (wt / WTSTD)^0.75, set$cl1 * 60)
    expect_equal_rounded(V_F * (wt / WTSTD), set$v1)
    expect_equal_rounded(KA, set$ka_PO * 60)
    expect_equal_rounded(ALAG * 60, set$tlag_PO)
  }
  # On: the same exponents applied to fat-free mass
  size <- pkSizeFactors(110, 175, 60, "male", TRUE)
  on <- pk(wt = 110, ffm = TRUE, age = 60)
  expect_equal_rounded(CL_F * size$clearance, on$cl1 * 60)
  expect_equal_rounded(V_F * size$volume, on$v1)
  off <- pk(wt = 110, age = 60)
  expect_lt(on$cl1, off$cl1)
  expect_lt(on$v1, off$v1)
})

test_that("the half-life lies in the label's 9 to 13 h for healthy volunteers", {
  halfLife <- log(2) * V_F / CL_F
  expect_equal_rounded(halfLife, 11.40045)
  set <- pk()
  expect_equal(log(2) * set$v1 / set$cl1 / 60, halfLife, tolerance = 1e-10)
  expect_gt(halfLife, 9)
  expect_lt(halfLife, 13)
})

test_that("the label's steady state at 0.4 mg once daily is reproduced (external check)", {
  # The analytical steady state of a lagged one-compartment oral model, from
  # the model's own returned parameters (in hours here).
  set <- pk()
  cl <- set$cl1 * 60; v <- set$v1; ka <- set$ka_PO * 60; lag <- set$tlag_PO / 60
  k <- cl / v; tau <- 24; dose <- 400                  # mcg, so mcg/L = ng/mL
  t <- seq(0, tau, by = 0.001)
  s <- (t - lag) %% tau
  conc <- dose * ka / (v * (ka - k)) *
    (exp(-k * s) / (1 - exp(-k * tau)) - exp(-ka * s) / (1 - exp(-ka * tau)))

  cmax <- max(conc); cmin <- min(conc); tmax <- t[which.max(conc)]
  auc  <- dose / cl
  expect_equal_rounded(cmax, 10.25345)
  expect_equal_rounded(cmin, 3.867238)
  expect_equal_rounded(auc, 175.4386)

  # Flomax label Table 2: 0.4 mg QD, light breakfast, 23 men aged 18-32:
  # Cmax 10.1, Cmin 4.0 ng/mL, median Tmax 6.0 h, AUCtau 151 ng.h/mL.
  # None of these was in Tsuda's fit.
  expect_equal(cmax, 10.1, tolerance = 0.05)
  expect_equal(cmin, 4.0, tolerance = 0.05)
  expect_equal(tmax, 6.0, tolerance = 0.02)
  expect_equal(auc / 151, 1.16, tolerance = 0.01)   # 16% high, stated in the header
  # ...and below the older men's 0.8 mg arm (men 55-75: 440 per 0.8 mg)
  expect_lt(auc, 440 / 2)
})

test_that("the single-dose peak is late, as for the modified-release capsule", {
  set <- pk()
  cl <- set$cl1 * 60; v <- set$v1; ka <- set$ka_PO * 60; lag <- set$tlag_PO / 60
  k <- cl / v
  t <- seq(0, 48, by = 0.001)
  conc <- ifelse(t > lag, 400 * ka / (v * (ka - k)) * (exp(-k * (t - lag)) - exp(-ka * (t - lag))), 0)
  # The label's fed single-dose Tmax is 6 to 7 h
  expect_equal_rounded(t[which.max(conc)], 6.818)
  expect_gt(t[which.max(conc)], 6)
  expect_lt(t[which.max(conc)], 7)
})

test_that("no effect site, no band, and the reference names the source", {
  x <- tamsulosin(70, 175, 30, "male", adjustToFFM = FALSE)
  expect_equal(c(x$tPeak, x$MEAC, x$typical, x$lowerTypical, x$upperTypical), rep(0, 5))
  expect_match(x$reference, "Tsuda Y", fixed = TRUE)
  expect_match(x$reference, "10.1111/j.1365-2125.2010.03662.x", fixed = TRUE)
  expect_match(x$reference, "20 uM", fixed = TRUE)   # AAG held at the reference
  expect_null(formals(tamsulosin)$cyp2d6)            # not read: see the header
  expect_null(formals(tamsulosin)$pregnant)
})
