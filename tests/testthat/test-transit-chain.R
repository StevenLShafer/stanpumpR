# The exact matrix-exponential engine (R/advanceTransitChain.R).
# Drafted by Claude Code, 2026-10-20.
#
# The engine claims to be EXACT, not approximate, so it is tested against
# closed-form answers wherever one exists and against an independent numerical
# integration where none does.  The order is deliberate: the matrix
# exponential first, on the Erlang density it has to reproduce; then the
# degenerate chain against the analytical solutions of a one-compartment
# system, which is the case where "exact" is checkable to rounding; then the
# full eight-deep chain with its metabolite against Runge-Kutta, where the
# reference is the approximate one.

noEventsTC <- data.frame(Time = numeric(0), Event = character(0))

# A classified dose table, in the shape simCpCe() hands the engines, with the
# doses already in the engine's amount unit.
classifyDoses <- function(Time, Dose, Units)
{
  d <- data.frame(Time = Time, Dose = Dose, Units = Units,
                  stringsAsFactors = FALSE)
  rate <- isRateUnit(Units)
  route <- doseRoute(Units)
  d$PO <- route == ROUTE_PO & !rate
  d$IM <- FALSE; d$IN <- FALSE; d$SL <- FALSE
  d$RA <- FALSE; d$RArate <- FALSE; d$RAslow <- FALSE; d$PO2 <- FALSE
  d$Bolus <- route == ROUTE_IV & !rate
  d
}

# Enalapril is the drug the engine was built for, and the only one that uses
# it, so it supplies the PK.  `plainPK()` collapses the chain to a single
# compartment so that the closed form applies.
tcPK <- function() getDrugPK("enalapril", 70, 175, 40, "male", adjustToFFM = FALSE)
plainPK <- function()
{
  PK <- tcPK()
  PK$transitChain <- list(n = 1L, ktr = 0, kMetabolic = 0)
  PK$PK[[PK_EVENT_DEFAULT]]$metabolite <- NULL
  PK
}

relErr <- function(got, ref, floor = 1e-9)
{
  keep <- ref > max(ref) * floor
  max(abs(got[keep] / ref[keep] - 1))
}


test_that("the matrix exponential reproduces the Erlang density exactly", {
  # The whole reason this engine exists: a chain of identical compartments has
  # one eigenvalue repeated, and the amount in the jth compartment after an
  # impulse into the first is the Erlang density, (kt)^(j-1) exp(-kt)/(j-1)!.
  # A partial-fraction expansion cannot write that down at all; the series
  # produces it with no special handling.
  k <- 0.3
  A <- matrix(0, 3, 3)
  A[1, 1] <- -k
  A[2, 1] <-  k; A[2, 2] <- -k
  A[3, 2] <-  k; A[3, 3] <- -k
  for (t in c(0.01, 1, 4, 40))
  {
    got <- expmScaleSquare(A * t)[, 1]
    exact <- c(1, k * t, (k * t)^2 / 2) * exp(-k * t)
    expect_lt(max(abs(got - exact)), 1e-14)
  }
})

test_that("the matrix exponential is accurate over a wide range of norms", {
  # Scaling and squaring has to hold up where the entries are large, which is
  # what a fast transit chain over a long step gives.
  k <- 16.13 / 60                      # enalapril's ktr, per minute
  A <- matrix(0, 2, 2)
  A[1, 1] <- -k
  A[2, 1] <-  k; A[2, 2] <- -k
  for (dt in c(1e-4, 0.01, 1, 60, 1440, 20160))
  {
    got <- expmScaleSquare(A * dt)[, 1]
    exact <- c(1, k * dt) * exp(-k * dt)
    expect_lt(max(abs(got - exact)), 1e-13 * max(1, max(abs(exact))))
  }
  # The identity, and a zero matrix, are the edge cases of the scaling
  expect_equal(expmScaleSquare(matrix(0, 3, 3)), diag(3))
})

test_that("a one-compartment chain reproduces the closed form for an IV bolus", {
  PK <- plainPK()
  s <- PK$PK[[PK_EVENT_DEFAULT]]
  got <- advanceTransitChain(classifyDoses(0, 1000, "mcg"), PK, 1440)
  ref <- 1000 / s$v1 * exp(-s$k10 * got$Time)
  expect_lt(relErr(got$Cp, ref), 1e-11)
  # No metabolite declared, so no metabolite columns at all
  expect_false("CpMetabolite" %in% names(got))
  # Nothing is in a depot after an intravenous dose
  expect_equal(max(got$Depot), 0)
})

test_that("a one-compartment chain reproduces the closed form for an infusion", {
  # The propagator's rate response is the part that is scaled rather than
  # recomputed, so it is checked against the analytical constant-rate
  # solution both during the infusion and after it stops.
  PK <- plainPK()
  s <- PK$PK[[PK_EVENT_DEFAULT]]
  R <- 50
  got <- advanceTransitChain(classifyDoses(c(0, 120), c(R, 0), "mcg/min"),
                             PK, 1440)
  plateau <- R / (s$v1 * s$k10)
  ref <- ifelse(got$Time <= 120,
                plateau * (1 - exp(-s$k10 * got$Time)),
                plateau * (1 - exp(-s$k10 * 120)) *
                  exp(-s$k10 * (got$Time - 120)))
  expect_lt(relErr(got$Cp, ref), 1e-11)
  # The peak is at the end of the infusion, as a constant-rate input gives
  expect_equal(got$Time[which.max(got$Cp)], 120)
})

test_that("a one-compartment chain reproduces the closed-form oral engine", {
  # With n = 1 the chain IS an ordinary first-order depot, so the two engines
  # are solving the same system and must agree to rounding.  This is the
  # strongest available check that the inputs, the lag, the bioavailability
  # and the time line are read the same way as everywhere else in the library.
  PK <- plainPK()
  s <- PK$PK[[PK_EVENT_DEFAULT]]
  d <- classifyDoses(c(0, 360), c(10000, 5000), "mcg PO")
  got <- advanceTransitChain(d, PK, 1440)
  ref <- advanceClosedFormPO_IM_IN(d, s, 1440, FALSE, 0)
  # The engines put the same knots on the line, so the points coincide
  expect_identical(got$Time, ref$Time)
  expect_lt(relErr(got$Cp, ref$Cp), 1e-11)
  # And so does what is still waiting in the depot
  expect_lt(relErr(got$Depot, ref$Depot), 1e-11)
})

test_that("a one-compartment parent reports no peripheral concentrations", {
  # The peripheral states are allocated only when the drug has them, so a
  # one-compartment parent must leave both columns flat at zero rather than
  # report whatever an unallocated state held.
  PK <- tcPK()
  got <- advanceTransitChain(classifyDoses(0, 10000, "mcg PO"), PK, 1440)
  expect_equal(max(got$C2), 0)
  expect_equal(max(got$C3), 0)
  # The metabolite HAS one, so its column must not be flat
  expect_gt(max(got$C2Metabolite), 0)
  expect_equal(max(got$C3Metabolite), 0)
})

test_that("the full eight-deep chain and its metabolite match Runge-Kutta", {
  # No closed form exists for this system, which is the point of the engine,
  # so the reference is an independent fourth-order Runge-Kutta integration of
  # the same thirteen states at a 0.001 minute step, built here from the
  # block rather than from the engine's own matrix.
  PK <- tcPK()
  s  <- PK$PK[[PK_EVENT_DEFAULT]]
  tc <- PK$transitChain
  mt <- tc$metabolite
  n <- tc$n; m <- mt$n
  nS <- n + 1L + m + 2L
  iC <- n + 1L; iMC <- iC + m + 1L; iMP <- iMC + 1L
  A <- matrix(0, nS, nS)
  chain <- c(rep(tc$ktr, n - 1L), s$ka_PO)
  for (j in seq_len(n)) { A[j, j] <- -chain[j]; A[j + 1L, j] <- chain[j] }
  A[iC, iC] <- -s$k10
  A[iC + 1L, iC] <- tc$kMetabolic
  for (j in seq_len(m))
  {
    A[iC + j, iC + j] <- -mt$ktr
    A[iC + j + 1L, iC + j] <- mt$ktr
  }
  A[iMC, iMC] <- -(mt$kel + mt$k12); A[iMC, iMP] <- mt$k21
  A[iMP, iMC] <- mt$k12;             A[iMP, iMP] <- -mt$k21

  dose <- 10000                                   # mcg, i.e. 10 mg
  second <- 720
  h <- 1e-3
  grid <- seq(0, 1440, by = h)
  x <- numeric(nS)
  x[1] <- dose * s$bioavailability_PO
  Cp <- Cm <- numeric(length(grid))
  Cp[1] <- x[iC] / s$v1; Cm[1] <- x[iMC] / mt$v
  atSecond <- which(abs(grid - second) < h / 2)[1]
  for (i in 2:length(grid))
  {
    k1 <- A %*% x; k2 <- A %*% (x + h / 2 * k1)
    k3 <- A %*% (x + h / 2 * k2); k4 <- A %*% (x + h * k3)
    x <- as.vector(x + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4))
    if (i == atSecond) x[1] <- x[1] + dose * s$bioavailability_PO
    Cp[i] <- x[iC] / s$v1; Cm[i] <- x[iMC] / mt$v
  }

  got <- advanceTransitChain(
    classifyDoses(c(0, second), c(dose, dose), "mcg PO"), PK, 1440)
  # Interpolated onto the engine's own time line, so the residual below is
  # the reference's error and the interpolation's, not the engine's.
  expect_lt(relErr(got$Cp, approx(grid, Cp, got$Time)$y, 1e-6), 1e-5)
  expect_lt(relErr(got$CpMetabolite, approx(grid, Cm, got$Time)$y, 1e-6), 1e-5)
})

test_that("the formed fraction is the elimination split, and mass balances", {
  # Everything absorbed leaves by one of two parent paths, and the share that
  # becomes metabolite is kMetabolic / k10 exactly.  This is the check that
  # the split of the parent's elimination is wired the right way round: a
  # swapped kOut and kMetabolic would keep the parent curve identical and get
  # the metabolite wrong by the ratio.
  PK <- tcPK()
  s  <- PK$PK[[PK_EVENT_DEFAULT]]
  tc <- PK$transitChain
  mt <- tc$metabolite
  dose <- 10000
  got <- advanceTransitChain(classifyDoses(0, dose, "mcg PO"), PK, 40320)
  auc <- function(y) sum(diff(got$Time) * (head(y, -1) + tail(y, -1)) / 2)
  absorbed <- dose * s$bioavailability_PO
  cleared  <- auc(got$Cp) * s$v1 * s$k10
  formed   <- auc(got$Cp) * s$v1 * tc$kMetabolic
  expect_equal(cleared / absorbed, 1, tolerance = 0.01)
  expect_equal(formed / absorbed, tc$kMetabolic / s$k10, tolerance = 0.01)
  # Faisal's KM / (KREN + KM): about 69% of what is absorbed is converted
  expect_equal(round(100 * tc$kMetabolic / s$k10), 69)
  # And what was formed is what the metabolite eliminates
  expect_equal(auc(got$CpMetabolite) * mt$v * mt$kel / formed, 1,
               tolerance = 0.01)
})

test_that("nothing is left in any compartment once the drug is gone", {
  PK <- tcPK()
  got <- advanceTransitChain(classifyDoses(0, 10000, "mcg PO"), PK, 40320)
  last <- nrow(got)
  expect_lt(got$Cp[last] / max(got$Cp), 1e-9)
  expect_lt(got$CpMetabolite[last] / max(got$CpMetabolite), 1e-6)
  expect_lt(got$Depot[last], 1e-9)
  # The chain fills and empties: the depot column peaks at the dose and falls
  expect_equal(max(got$Depot), 10000 * PK$PK[[PK_EVENT_DEFAULT]]$bioavailability_PO,
               tolerance = 1e-9)
})

test_that("doses superpose, because the system is linear", {
  # The engine integrates the whole table at once, as the Michaelis-Menten
  # engine must, but unlike that one it has no business being non-linear: two
  # doses must give exactly the sum of each alone.
  PK <- tcPK()
  # Each single run carries the other dose as ZERO rather than dropping the
  # row, so that all three tables put the same knots on the time line and the
  # curves can be added point by point.  Interpolating between lines instead
  # would measure the interpolation, which on this curvature is a thousand
  # times larger than what is being tested.
  one  <- advanceTransitChain(classifyDoses(c(0, 720), c(10000, 0), "mcg PO"),
                              PK, 2880)
  two  <- advanceTransitChain(classifyDoses(c(0, 720), c(0, 10000), "mcg PO"),
                              PK, 2880)
  both <- advanceTransitChain(classifyDoses(c(0, 720), c(10000, 10000), "mcg PO"),
                              PK, 2880)
  expect_identical(one$Time, both$Time)
  expect_identical(two$Time, both$Time)
  expect_lt(relErr(both$Cp, one$Cp + two$Cp), 1e-11)
  expect_lt(relErr(both$CpMetabolite, one$CpMetabolite + two$CpMetabolite), 1e-11)
})

test_that("the engine reports no effect site and no time until threshold", {
  # Structural, not pending: a defective system leaves no exponential
  # amplitudes for recoveryStates() to solve a threshold from.
  PK <- tcPK()
  got <- advanceTransitChain(classifyDoses(0, 10000, "mcg PO"), PK, 1440,
                             plotRecovery = TRUE, emerge = 1)
  expect_true(all(is.na(got$Ce)))
  expect_true(all(is.na(got$Recovery)))
  expect_true(all(is.na(got$CeMetabolite)))
  expect_null(attr(got, "recoveryStates"))
  expect_null(attr(got, "metaboliteRecoveryStates"))
})

test_that("a route the engine does not carry is refused, not ignored", {
  PK <- tcPK()
  d <- classifyDoses(0, 1000, "mcg IM")
  expect_error(advanceTransitChain(d, PK, 1440), "cannot simulate units")
  # A zero dose of such a unit is harmless, so it must not stop the run
  d0 <- classifyDoses(0, 0, "mcg IM")
  expect_equal(max(advanceTransitChain(d0, PK, 1440)$Cp), 0)
})

test_that("validateTransitChain refuses every malformed block", {
  PK <- tcPK()
  s <- PK$PK[[PK_EVENT_DEFAULT]]
  ok <- PK$transitChain
  bad <- function(change) {
    tc <- modifyList(ok, change)
    expect_error(validateTransitChain(tc, s, "enalapril"), "transitChain")
  }
  bad(list(n = 0L))
  bad(list(n = 2.5))
  bad(list(ktr = 0))
  bad(list(ktr = -1))
  # kMetabolic above the total elimination the clearance pays for
  bad(list(kMetabolic = s$k10 * 2))
  bad(list(metabolite = modifyList(ok$metabolite, list(v = 0))))
  bad(list(metabolite = modifyList(ok$metabolite, list(kel = -1))))
  bad(list(metabolite = modifyList(ok$metabolite, list(name = ""))))
  bad(list(metabolite = modifyList(ok$metabolite, list(ktr = 0))))
  # A peripheral compartment needs both rate constants or neither
  bad(list(metabolite = modifyList(ok$metabolite, list(k21 = 0))))
  # A metabolite with nothing forming it
  expect_error(
    validateTransitChain(modifyList(ok, list(kMetabolic = 0)), s, "enalapril"),
    "kMetabolic is zero")
  # The chain's last compartment empties at ka_PO, so there must be one
  expect_error(
    validateTransitChain(ok, modifyList(s, list(ka_PO = 0)), "enalapril"),
    "ka_PO")
  # And the real block passes
  expect_identical(validateTransitChain(ok, s, "enalapril")$n, ok$n)
  expect_null(validateTransitChain(NULL, s, "enalapril"))
})

test_that("getDrugPK refuses a transit chain beside an incompatible mode", {
  # The engine integrates the whole table at once and returns both moieties,
  # so nothing upstream may split the table, rewrite the oral rows or attach a
  # second metabolite by the closed-form route.
  withMode <- function(mode) {
    X <- list(
      PK = list(default = list(v1 = 50, v2 = 1, v3 = 1, cl1 = 0.8, cl2 = 0,
                               cl3 = 0, ka_PO = 0.1, bioavailability_PO = 0.6,
                               tlag_PO = 0)),
      tPeak = 0, MEAC = 0, typical = 0, upperTypical = 0, lowerTypical = 0,
      transitChain = list(n = 2L, ktr = 0.3, kMetabolic = 0)
    )
    c(X, mode)
  }
  # Modes this guard is the first to reject.
  for (mode in list(list(michaelisMenten = list(vmax = 1, km = 1)),
                    list(oralSaturation = list(Imax = 0.906, ID50 = 571)),
                    list(oralPulses = list(default = list(
                      fraction = c(0.5, 0.5), delay = c(0, 60)))),
                    list(metabolite = list(name = "enalaprilat",
                                           kFormation = 0.01))))
    expect_error(getDrugPK("enalapril", 70, 175, 40, "male",
                           model = withMode(mode)),
                 "Invalid transitChain")
  # A zero-order oral release is rejected by its own earlier guard, which
  # names itself; either message is a refusal, and the point is that the
  # combination cannot be built.
  expect_error(getDrugPK("enalapril", 70, 175, 40, "male",
                         model = withMode(list(
                           oralZeroOrder = list(duration = 60, lag = 0)))),
               "Invalid oralZeroOrder|Invalid transitChain")
})
