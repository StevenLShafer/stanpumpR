# Fosinopril (R/drugs_fosinopril.R): the plotted moiety is fosinoprilat, from a
# three-compartment model DERIVED from Hui 1991's Table IV.
# Drafted by Claude Code, 2026-10-29.
#
# The derivation is the thing under test, so this file redoes it from the
# paper's six printed numbers independently of the drug file -- and the oral
# curve is checked against measurements the model was not given, by an
# independent closed-form convolution written here.
#
# Hui KK, Duchin KL, Kripalani KJ, Chan D, Kramer PK, Yanagawa N,
# Clin Pharmacol Ther 1991;49:457-467, Tables I, IV and V; absorption from
# Singhvi et al., Br J Clin Pharmacol 1988;25:9-15, Tables 1 and 2.

# --- Hui Table IV, normal renal function (PDF p7) ---
IV_DOSE  <- 7500      # mcg fosinoprilat; "equimolar to 10 mg fosinopril sodium"
V_INIT   <- 3.1       # L
CL_OBS   <- 26.5      # mL/min, model-dependent
VSS_OBS  <- 10.2      # L,      model-dependent
T_HALF   <- c(0.2, 2.1, 12.4)   # h, alpha, beta, gamma
AUC_OBS  <- 4971      # ng.h/mL, Table IV's own model area

# --- Hui's four groups: Table I creatinine clearance, Table IV clearance ---
CRCL_GROUPS <- c(98.8, 59.6, 31.8, 16.3)   # mL/min/1.73 m^2
CL_GROUPS   <- c(26.5, 14.6, 13.7, 18.2)   # mL/min, model-dependent
T3_GROUPS   <- c(12.4, 14.0, 14.1, 16.9)   # h, terminal half-life

# --- Singhvi 1988 (PMID 2967089), nine healthy men, three-way crossover ---
# Intravenous fosinoprilat 7.5 mg and oral fosinopril sodium 10 mg as a capsule
# and as a solution.  F is READ from here, not derived: bioavailability is
# clearance-independent in a within-subject crossover, so forcing the model to
# reproduce THIS study's exposure while it runs on HUI's clearance would
# double-count the 1.47-fold difference between the two populations.
ORAL_DOSE <- 10       # mg fosinopril sodium
TMAX_RANGE <- c(2.8, 3.1)          # h, fosinoprilat, solution and capsule (Table 1)
F_CAPSULE  <- 0.29    # fosinoprilat bioavailability, capsule (solution 0.25)
CL_SINGHVI <- 39      # mL/min, his abstract (Table 2: 39.1)
VSS_SINGHVI <- 10     # L, his abstract (Table 2: 9.8)
# NOT fosinoprilat: the AUC(0-24h) 1035-1132 mcg/L.h that Murdoch's review
# labels "plasma fosinoprilat" is TOTAL RADIOACTIVITY in Singhvi's Table 1, of
# which only about 75% is the diacid (and the review's Cmax 99-140 spans both
# analytes).  The first draft of this model derived F = 0.26 against them; both
# the analyte and the method were wrong.
TOTAL_RADIO_AUC24 <- c(1035, 1132)  # mcg/L.h, total radioactivity
FOSINOPRILAT_SHARE <- 0.75          # of plasma radioactivity after an oral dose
DIACID_AUCINF <- c(788, 920)        # ng.h/mL, fosinoprilat, Table 1

# Hui's Methods: 7.5 mg fosinoprilat was equimolar to 10 mg fosinopril sodium
PRILAT_PER_FOSINOPRIL <- 0.75

# ---------------------------------------------------------------------------
# Hui's Table IV decomposed here, from scratch, without the drug file
# ---------------------------------------------------------------------------
hui <- local({
  lambda <- log(2) / (T_HALF * 60)            # /min
  cl <- CL_OBS / 1000                         # L/min
  auc <- IV_DOSE / cl                         # mcg.min/L
  mrt <- VSS_OBS / cl                         # min, since Vss = CL x MRT
  # The Methods define V_init as Dose/(A+B+C) and the area as sum(A/lambda);
  # the second moment follows from MRT x AUC.  Three linear equations.
  abc <- solve(rbind(c(1, 1, 1), 1 / lambda, 1 / lambda^2),
               c(IV_DOSE / V_INIT, auc, mrt * auc))
  # The mammillary model behind that triexponential, by hand
  cu <- abc / IV_DOSE                         # per mcg, i.e. 1/L
  v1 <- 1 / sum(cu)
  b1 <- v1 * (cu[1] * (lambda[2] + lambda[3]) + cu[2] * (lambda[1] + lambda[3]) +
                cu[3] * (lambda[1] + lambda[2]))
  b0 <- v1 * (cu[1] * lambda[2] * lambda[3] + cu[2] * lambda[1] * lambda[3] +
                cu[3] * lambda[1] * lambda[2])
  k21 <- (b1 + sqrt(b1^2 - 4 * b0)) / 2
  k31 <- (b1 - sqrt(b1^2 - 4 * b0)) / 2
  k10 <- prod(lambda) / (k21 * k31)
  s1 <- sum(lambda) - k10 - k21 - k31
  s2 <- lambda[1] * lambda[2] + lambda[1] * lambda[3] + lambda[2] * lambda[3] -
    k10 * k21 - k10 * k31 - k21 * k31
  k12 <- (s2 - s1 * k21) / (k31 - k21)
  k13 <- s1 - k12
  list(lambda = lambda, abc = abc,
       v1 = v1, v2 = v1 * k12 / k21, v3 = v1 * k13 / k31,
       cl1 = v1 * k10, cl2 = v1 * k12, cl3 = v1 * k13,
       k10 = k10, k12 = k12, k21 = k21, k13 = k13, k31 = k31)
})

# The eigenvalues of a three-compartment model, from its parameters -- the
# inverse of the decomposition above, used to check what a changed clearance
# does to the half-lives.
eigenvalues3 <- function(v1, v2, v3, cl1, cl2, cl3) {
  k10 <- cl1 / v1; k12 <- cl2 / v1; k21 <- cl2 / v2
  k13 <- cl3 / v1; k31 <- cl3 / v3
  a2 <- k10 + k12 + k21 + k13 + k31
  a1 <- k10 * k21 + k10 * k31 + k21 * k31 + k12 * k31 + k13 * k21
  a0 <- k10 * k21 * k31
  sort(-Re(polyroot(c(a0, a1, a2, 1))), decreasing = TRUE)
}

at <- function(ffm = FALSE, wt = 70, ht = 170, age = 50, sex = "male",
               creatinine = NULL)
  getDrugPK("fosinopril", wt, ht, age, sex, adjustToFFM = ffm,
            creatinine = creatinine)

test_that("Hui's six printed numbers close a three-compartment model", {
  # Three positive intercepts, or there is no triexponential at all
  expect_true(all(hui$abc > 0))
  # And every micro rate constant positive, or it is not a mammillary model
  expect_gt(hui$k10, 0)
  expect_gt(hui$k12, 0)
  expect_gt(hui$k21, 0)
  expect_gt(hui$k13, 0)
  expect_gt(hui$k31, 0)
  # The intercept sum must exceed the 5-minute concentration of a 90-second
  # infusion, since back-extrapolation to t = 0 can only go up
  expect_gt(sum(hui$abc), sum(hui$abc * exp(-hui$lambda * 5)))
})

test_that("the decomposition returns Hui's own inputs", {
  expect_equal_rounded(V_INIT, hui$v1, tolerance = 1e-9)
  expect_equal_rounded(CL_OBS, hui$cl1 * 1000, tolerance = 1e-9)
  expect_equal_rounded(VSS_OBS, hui$v1 + hui$v2 + hui$v3, tolerance = 1e-9)
  # and the half-lives it was built from
  expect_equal_rounded(T_HALF,
                       log(2) / eigenvalues3(hui$v1, hui$v2, hui$v3,
                                             hui$cl1, hui$cl2, hui$cl3) / 60,
                       tolerance = 1e-6)
})

test_that("the solved intercepts and vector are what the header states", {
  # Pinned: A, B, C in ng/mL at the 7500 mcg dose Hui gave
  expect_equal_rounded(c(1299.8187, 1055.5970, 63.9391), hui$abc,
                       tolerance = 1e-6)
  expect_equal_rounded(c(3.100000, 2.426674, 4.673326),
                       c(hui$v1, hui$v2, hui$v3), tolerance = 1e-6)
  expect_equal_rounded(c(26.50000, 71.74615, 5.47402),
                       c(hui$cl1, hui$cl2, hui$cl3) * 1000, tolerance = 1e-6)
})

test_that("the third phase is the small enzyme-binding tail the header claims", {
  # C is a few per cent of the intercept sum, and the terminal phase carries
  # only a few per cent of the area: this is the ACE-binding tail, linearised.
  expect_lt(hui$abc[3] / sum(hui$abc), 0.03)
  # Thin but long: a 2.6% intercept decaying that slowly still carries about a
  # quarter of the area, as benazeprilat's 22.3 h phase carries 17%.
  area <- hui$abc / hui$lambda
  expect_equal_rounded(0.2425, area[3] / sum(area), tolerance = 1e-3)
})

test_that("the drug carries exactly the derived vector at normal renal function", {
  set <- at()$PK$default
  expect_equal_rounded(hui$v1, set$v1, tolerance = 1e-6)
  expect_equal_rounded(hui$v2, set$v2, tolerance = 1e-6)
  expect_equal_rounded(hui$v3, set$v3, tolerance = 1e-6)
  expect_equal_rounded(hui$cl1, set$cl1, tolerance = 1e-6)
  expect_equal_rounded(hui$cl2, set$cl2, tolerance = 1e-6)
  expect_equal_rounded(hui$cl3, set$cl3, tolerance = 1e-6)
})

test_that("Table IV's parameters do not form one exactly consistent model", {
  # Stated in the header rather than corrected: the derived area is about 5%
  # below Table IV's own model area, because its parameters are means of
  # individual fits.  Hold the size of that gap so it cannot drift unnoticed.
  derived <- IV_DOSE / (CL_OBS / 1000) / 60          # ng.h/mL
  expect_equal_rounded(4716.98, derived, tolerance = 1e-4)
  expect_lt(abs(derived / AUC_OBS - 1), 0.06)
  expect_gt(abs(derived / AUC_OBS - 1), 0.04)
})

test_that("every corner of Hui's printed SEM box gives an admissible model", {
  # The conditioning test of docs/antihypertensives.md section 3.3, which
  # esmolol fails.  All 3^6 = 729 corners, t1/2 alpha reaching 0.1 and 0.3 h.
  box <- expand.grid(
    vInit = 3.1 + c(-0.4, 0, 0.4),
    cl    = (26.5 + c(-3.0, 0, 3.0)) / 1000,
    vss   = 10.2 + c(-0.9, 0, 0.9),
    t1    = 0.2 + c(-0.1, 0, 0.1),
    t2    = 2.1 + c(-0.2, 0, 0.2),
    t3    = 12.4 + c(-1.7, 0, 1.7)
  )
  expect_equal(nrow(box), 729L)
  got <- lapply(seq_len(nrow(box)), function(i) {
    r <- box[i, ]
    tryCatch(fosinoprilatDisposition(r$cl, r$vInit, r$vss,
                                     c(r$t1, r$t2, r$t3)),
             error = function(e) NULL)
  })
  expect_true(all(!vapply(got, is.null, logical(1))))
  # Every V1 and CL is its own input; the derived parameters are the soft ones
  spans <- vapply(c("v1", "v2", "v3", "cl1", "cl2", "cl3"), function(nm) {
    x <- vapply(got, function(g) g[[nm]], numeric(1)); max(x) / min(x)
  }, numeric(1))
  expect_lt(spans[["v1"]], 1.35)
  expect_lt(spans[["cl1"]], 1.30)
  expect_lt(spans[["v2"]], 4.1)      # the header quotes 4.03x
  expect_lt(spans[["cl2"]], 6.7)     # and 6.66x, driven by t1/2 alpha's SEM
})

test_that("the dose conversion is the source's own 0.75, inside bioavailability", {
  set <- at()$PK$default
  # bioavailability_PO carries BOTH the molar conversion of the prodrug and the
  # bioavailability of the diacid, so nothing applies either again.
  expect_equal_rounded(PRILAT_PER_FOSINOPRIL * F_CAPSULE, set$bioavailability_PO,
                       tolerance = 1e-9)
  # The 0.75 read from Hui agrees with molar arithmetic to under 1%: the label
  # gives fosinopril sodium 585.65 g/mol, and the diacid is near 436.4.
  expect_lt(abs(PRILAT_PER_FOSINOPRIL / (436.4 / 585.65) - 1), 0.01)
})

test_that("the renal covariate plateaus and is not proportional to CrCl", {
  # Hui's own finding: a fall from normal to impaired, then flat.
  expect_equal_rounded(26.5, fosinoprilatClearance(98.8), tolerance = 1e-9)
  expect_equal_rounded(26.5, fosinoprilatClearance(80), tolerance = 1e-9)
  expect_equal_rounded(15.5, fosinoprilatClearance(59.6), tolerance = 1e-9)
  # flat across the whole impaired range he measured (11 to 72, Fig. 3)
  expect_equal_rounded(15.5, fosinoprilatClearance(11), tolerance = 1e-9)
  expect_equal_rounded(15.5, fosinoprilatClearance(16.3), tolerance = 1e-9)
  expect_equal_rounded(15.5, fosinoprilatClearance(31.8), tolerance = 1e-9)
  # and capped above, where neither source measured anything
  expect_equal_rounded(26.5, fosinoprilatClearance(150), tolerance = 1e-9)
  # NOT proportional: halving a normal CrCl must not halve the clearance
  expect_gt(fosinoprilatClearance(49.4) / fosinoprilatClearance(98.8), 0.5)
  # monotone and continuous across the ramp
  x <- seq(0, 150, by = 0.5)
  y <- fosinoprilatClearance(x)
  expect_true(all(diff(y) >= 0))
  expect_lt(max(abs(diff(y))), 0.3)
  # 15.5 is the unweighted mean of Hui's three impaired group means
  expect_equal_rounded(15.5, mean(CL_GROUPS[2:4]), tolerance = 1e-9)
})

test_that("the covariate reproduces Hui's four groups to the data's own scatter", {
  got <- fosinoprilatClearance(CRCL_GROUPS)
  expect_equal_rounded(CL_GROUPS[1], got[1], tolerance = 1e-9)   # exact
  # The three impaired groups all take the plateau, within their own spread:
  # their SEMs are 1.7, 1.4 and 3.8 mL/min.
  expect_lt(max(abs(got[2:4] - CL_GROUPS[2:4])), 2.8)
})

test_that("a clearance-only covariate predicts the impaired half-lives it was not told", {
  # The volumes and both intercompartmental clearances are held at the normal
  # group's, so the impaired half-lives are a prediction, not a fit.
  plateau <- fosinoprilatClearance(31.8) / 1000
  p <- fosinoprilatDisposition(FOSINOPRIL_CL_NORMAL / 1000)
  th <- log(2) / eigenvalues3(p$v1, p$v2, p$v3, plateau, p$cl2, p$cl3) / 60
  expect_equal_rounded(14.72, th[3], tolerance = 1e-3)
  # Against Hui's measured 14.0, 14.1 and 16.9 h in the three impaired groups.
  # Worth less than the next check: Table IV marks t1/2 gamma with NO
  # significance symbol and the text's list of what differed between normal and
  # impaired omits it, so this is a match to a trend, not to an established
  # effect.
  expect_lt(abs(th[3] / mean(T3_GROUPS[2:4]) - 1), 0.03)
  # The second phase, less tightly but worth more: his 3.1, 3.9, 3.1, and he
  # DID report the elimination half-life as significantly lower in normal renal
  # function.  This phase also carries 68% of the area.
  expect_lt(abs(th[2] / mean(c(3.1, 3.9, 3.1)) - 1), 0.15)
  area <- hui$abc / hui$lambda
  expect_gt(area[2] / sum(area), 0.6)
  # the volumes are untouched by the covariate, as Table IV's show no trend,
  # and the drug file itself must carry that structure at an impaired clearance
  set <- at(creatinine = 4.5)$PK$default
  expect_equal_rounded(hui$v1, set$v1, tolerance = 1e-9)
  expect_equal_rounded(hui$v2, set$v2, tolerance = 1e-9)
  expect_equal_rounded(hui$v3, set$v3, tolerance = 1e-9)
  expect_equal_rounded(hui$cl2, set$cl2, tolerance = 1e-9)
  expect_equal_rounded(hui$cl3, set$cl3, tolerance = 1e-9)
  expect_equal_rounded(VSS_OBS, set$v1 + set$v2 + set$v3, tolerance = 1e-9)
})

test_that("the serum creatinine field reaches the model, and a blank is assumed normal", {
  # getDrugPK passes the Patient Profile's creatinine only to models that name
  # it; fosinopril does.
  normal <- at()$PK$default$cl1
  severe <- at(creatinine = 4.5)$PK$default$cl1
  expect_lt(severe, normal)
  expect_equal_rounded(15.5 / 1000, severe, tolerance = 1e-9)
  # A blank field is the assumed normal creatinine for age and sex, which for a
  # 50-year-old man puts the indexed clearance above the ramp's top
  expect_equal_rounded(26.5 / 1000, normal, tolerance = 1e-9)
  # and entering that same assumed value explicitly must change nothing
  expect_equal_rounded(normal, at(creatinine = 0.9)$PK$default$cl1,
                       tolerance = 1e-9)
  # The age term in Cockcroft-Gault puts an older patient on the ramp even with
  # a blank field -- stated in the header, so pinned here.
  older <- at(age = 70)$PK$default$cl1 * 1000
  expect_lt(older, 26.5)
  expect_gt(older, 15.5)
  expect_equal_rounded(22.34092, older, tolerance = 1e-5)
})

test_that("the quoted oral area is total radioactivity, not fosinoprilat", {
  # The arithmetic that settled it before the tables were read, from three
  # numbers in Singhvi's abstract.  His own bioavailability and his own
  # clearance give the fosinoprilat area directly -- and his Table 1 prints it,
  # within 2% -- and it matches the quoted figure only after the 75% share.
  dEq <- ORAL_DOSE * 1000 * PRILAT_PER_FOSINOPRIL            # mcg fosinoprilat
  clS <- CL_SINGHVI / 1000 * 60                              # L/h
  implied <- c(0.25, F_CAPSULE) * dEq / clS                  # ng.h/mL, 0-inf
  expect_equal_rounded(c(801.3, 929.5), implied, tolerance = 1e-3)
  expect_true(all(abs(implied / DIACID_AUCINF - 1) < 0.02))
  # the quoted area scaled to the diacid brackets what his own numbers imply
  # over 0 to 24 h, truncated with this model's oral curve run at HIS
  # clearance, which leaves 95.1% of the area inside 24 h (at Hui's slower
  # clearance it leaves 92%, the share an earlier draft used)
  diacid <- FOSINOPRILAT_SHARE * TOTAL_RADIO_AUC24
  expect_equal_rounded(c(776.25, 849.0), diacid, tolerance = 1e-4)
  expect_lt(abs(mean(diacid) / mean(0.951 * implied) - 1), 0.03)
  # whereas taking the quoted figure AS fosinoprilat overstates it by a third
  expect_gt(mean(TOTAL_RADIO_AUC24) / mean(0.951 * implied), 1.25)
})

test_that("the oral curve is Singhvi's F on Hui's clearance, 1.47-fold his", {
  # An independent closed-form oral solution, written here: the unit-impulse
  # response convolved with a first-order input,
  #   Cp(t) = F D ka sum_i c_i (exp(-lambda_i t) - exp(-ka t)) / (ka - lambda_i)
  set <- at()$PK$default
  cu <- hui$abc / IV_DOSE
  lam <- hui$lambda
  ka <- set$ka_PO
  dose <- ORAL_DOSE * 1000 * set$bioavailability_PO      # mcg reaching plasma
  cp <- function(t) dose * ka * sum(cu * (exp(-lam * t) - exp(-ka * t)) / (ka - lam))
  auc <- function(tt) dose * ka *
    sum(cu * ((1 - exp(-lam * tt)) / lam - (1 - exp(-ka * tt)) / ka) / (ka - lam))

  # The RATE is still set from the reported time to peak, which is
  # fosinoprilat's own in Singhvi's Table 1, and the label independently says
  # "approximately 3 hours".
  tmax <- optimize(function(t) -cp(t), c(1, 2000))$minimum / 60
  expect_gt(tmax, TMAX_RANGE[1])
  expect_lt(tmax, TMAX_RANGE[2])
  expect_equal_rounded(2.95, tmax, tolerance = 2e-3)   # the midpoint it was set to

  # The EXTENT is read, so the exposure is a prediction rather than a fit, and
  # it must come out 1.47-fold Singhvi's because that is the clearance ratio.
  expect_equal_rounded(131.36, cp(tmax * 60), tolerance = 1e-3)
  expect_equal_rounded(1256.9, auc(24 * 60) / 60, tolerance = 1e-4)

  dEq <- ORAL_DOSE * 1000 * PRILAT_PER_FOSINOPRIL
  aucInf <- F_CAPSULE * dEq / (CL_OBS / 1000 * 60)          # on Hui's clearance
  expect_equal_rounded(1367.9, aucInf, tolerance = 1e-3)
  singhvi <- F_CAPSULE * dEq / (CL_SINGHVI / 1000 * 60)     # on his own
  expect_equal_rounded(929.5, singhvi, tolerance = 1e-3)
  expect_equal_rounded(CL_SINGHVI / CL_OBS, aucInf / singhvi, tolerance = 1e-6)
  expect_equal_rounded(1.4717, aucInf / singhvi, tolerance = 1e-4)

  # A CLEARANCE RATIO IS NOT A CURVE RATIO.  The clearance scales the AREA
  # exactly and the PEAK much less, because the peak arrives before much drug
  # has been eliminated -- and it moves the peak time too.  Both documents said
  # "expect the whole curve 1.5-fold high" until 2026-10-29; this pins what is
  # actually true so the claim cannot drift back.
  sing <- function(f) {                       # the same model at Singhvi's CL
    k10 <- CL_SINGHVI/1000/hui$v1; k12 <- hui$cl2/hui$v1; k21 <- hui$cl2/hui$v2
    k13 <- hui$cl3/hui$v1; k31 <- hui$cl3/hui$v3
    a2 <- k10+k12+k21+k13+k31
    a1 <- k10*k21+k10*k31+k21*k31+k12*k31+k13*k21
    l <- sort(-Re(polyroot(c(k10*k21*k31, a1, a2, 1))), decreasing = TRUE)
    cu2 <- vapply(seq_along(l), function(i) {
      o <- l[-i]; (k21-l[i])*(k31-l[i])/(hui$v1*prod(o-l[i]))
    }, numeric(1))
    cp2 <- function(t) dose*ka*sum(cu2*(exp(-l*t)-exp(-ka*t))/(ka-l))
    tm2 <- optimize(function(t) -cp2(t), c(1, 4000))$minimum
    f(tm2, cp2)
  }
  tmaxS <- sing(function(tm, f) tm/60)
  cmaxS <- sing(function(tm, f) f(tm))
  expect_equal_rounded(2.43, tmaxS, tolerance = 5e-3)        # against Hui's 2.95
  expect_equal_rounded(1.21, cp(tmax*60)/cmaxS, tolerance = 5e-3)   # peak ratio
  expect_lt(cp(tmax*60)/cmaxS, aucInf/singhvi)               # peak feels it less
})

test_that("the bioavailability used is the capsule's, read, and Hui's is lower", {
  used <- at()$PK$default$bioavailability_PO / PRILAT_PER_FOSINOPRIL
  expect_equal_rounded(F_CAPSULE, used, tolerance = 1e-9)
  # Hui's own crossover: 22.5, 22.0 and 27.9% in the mild, moderate and severe
  # groups (Table V), against the same patients' own intravenous doses.  They
  # sit BELOW the healthy capsule value, which the help page says rather than
  # glossing: the mild group by 2.4 of its own standard errors.
  huiF <- mean(c(22.5, 22.0, 27.9)) / 100
  expect_equal_rounded(0.24133, huiF, tolerance = 1e-4)
  expect_lt(huiF, used)
  expect_lt(abs(used / huiF - 1), 0.21)
  expect_gt((F_CAPSULE * 100 - 22.5) / 2.7, 2.3)   # SEM of the mild group
  # Hui reports no relationship between bioavailability and renal function,
  # which is why one absorption model serves every patient.
})

test_that("the two sources agree on the volume and differ on the clearance", {
  # The reverse of the pattern AGENTS.md predicts: the CONSTRUCT agrees and the
  # physiological parameter does not.
  vss <- sum(unlist(at()$PK$default[c("v1", "v2", "v3")]))
  expect_lt(abs(vss / VSS_SINGHVI - 1), 0.03)
  expect_equal_rounded(1.4717, CL_SINGHVI / CL_OBS, tolerance = 1e-4)
})

test_that("both positions of the fat-free-mass switch behave as documented", {
  # Hui fitted no size covariate, so with the switch OFF the published
  # parameters stand for every patient (legacyVolume = 1).
  for (p in list(list(50, 160, 25, "female"), list(110, 185, 45, "male"))) {
    off <- do.call(function(w, h, a, s) at(FALSE, w, h, a, s), p)$PK$default
    expect_equal_rounded(hui$v1, off$v1, tolerance = 1e-6)
    expect_equal_rounded(hui$v2, off$v2, tolerance = 1e-6)
    expect_equal_rounded(hui$v3, off$v3, tolerance = 1e-6)
    expect_equal_rounded(hui$cl2, off$cl2, tolerance = 1e-6)
    # Both of these patients have an assumed-normal creatinine that puts them
    # on the covariate's upper plateau, so the published clearance stands too.
    expect_equal_rounded(hui$cl1, off$cl1, tolerance = 1e-6)
  }
  # With it ON, the volumes scale with fat-free mass and the clearances with
  # its 3/4 power.
  on <- at(TRUE, 110, 185, 45, "male")$PK$default
  ffm <- ffmAlSallami(110, 185, 45, "male") / FFM_REFERENCE
  expect_equal_rounded(hui$v1 * ffm, on$v1, tolerance = 1e-6)
  expect_equal_rounded(hui$v2 * ffm, on$v2, tolerance = 1e-6)
  expect_equal_rounded(hui$v3 * ffm, on$v3, tolerance = 1e-6)
  expect_equal_rounded(hui$cl2 * ffm^0.75, on$cl2, tolerance = 1e-6)
  # The renal covariate is NOT a size factor: the clearance it sets is scaled
  # by the same 3/4 power, and the switch also moves the weight Cockcroft-Gault
  # sees, so cl1 is checked against the covariate rather than against hui$cl1.
  expect_equal_rounded(
    fosinoprilatClearance(
      creatinineClearanceCG(pkSizeFactors(110, 185, 45, "male", TRUE)$pkWeight,
                            45, "male", adultCreatinine("male")) * 1.73 /
        bsaDuBois(pkSizeFactors(110, 185, 45, "male", TRUE)$pkWeight, 185)
    ) / 1000 * ffm^0.75,
    on$cl1, tolerance = 1e-6)
  # Pinned values, both switch positions, so neither can drift silently
  expect_equal_rounded(c(4.2597586, 0.03363282), c(on$v1, on$cl1),
                       tolerance = 1e-6)
})

test_that("it is plasma only: no effect site, no MEAC and no band", {
  # No antihypertensive in this library plots a blood pressure, and there is no
  # established therapeutic range for fosinoprilat.
  d <- fosinopril(70, 170, 50, "male")
  expect_equal(d$tPeak, 0)
  expect_equal(d$MEAC, 0)
  expect_equal(d$typical, 0)
  expect_equal(d$lowerTypical, 0)
  expect_equal(d$upperTypical, 0)
  expect_equal(at()$PK$default$tlag_PO, 0)
})

test_that("the reference string names the source, the moiety and the dose basis", {
  r <- fosinopril(70, 170, 50, "male")$reference
  expect_match(r, "Hui KK")
  expect_match(r, "Clin Pharmacol Ther 1991;49:457-67")
  expect_match(r, "FOSINOPRILAT")
  expect_match(r, "0\\.75 mg fosinoprilat per mg fosinopril sodium")
  expect_match(r, "Singhvi SM")
  expect_match(r, "2967089")
  expect_match(r, "TOTAL RADIOACTIVITY")
  expect_match(r, "BLANK SERUM")
  expect_match(r, "10.1038/clpt.1991.54", fixed = TRUE)
})
