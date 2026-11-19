# The front-end (recirculatory) engine, R/frontEndKinetics.R, and the models
# of the two papers it was built on, R/frontEndModels.R.
# Drafted by Claude Code, 2026-11-19.
#
# The order: the engine's mathematics first (the Erlang density a delay
# element must produce, mass balance, the steady states the papers' equations
# give, the mammillary model against an independent eigen-decomposition, the
# BET regimen holding its own model at the target), then the papers' results
# (Henthorn 2018's Figure 6 and Table 3, Avram and Krejcie 2003's Table 1),
# then the teaching claim that an effect site filters the front end away.
# docs/front-end-kinetics.md gives the reasoning behind every tolerance.

fixture <- function(name) test_path("fixtures", "front-end", name)

test_that("a central delay element delivers an Erlang first pass", {
  # One chain of n compartments; the periphery is one enormous tissue, so
  # nothing recirculates in the first minute at any visible concentration.
  n <- 6; V <- 1.2; CO <- 4; dose <- 100
  spec <- list(type = "recirculatory", cardiac_output = CO,
               central = list(volume = V, n = n),
               pathways = list(list(name = "body", type = "tissue", flow = CO, volume = 1e6)))
  t <- seq(0.02, 0.8, by = 0.02)
  s <- frontEndSimulate(frontEndSystem(spec), frontEndDoses(bolus = data.frame(time = 0, amount = dose)), t)
  expected <- dose / CO * stats::dgamma(t, shape = n, rate = n * CO / V)
  expect_lt(max(abs(s$arterial - expected)) / max(expected), 1e-4)
})

test_that("mass is conserved with boluses, infusions and decaying infusions", {
  doses <- frontEndDoses(bolus = data.frame(time = c(0, 7.3), amount = c(50, 20)),
                         infusion = data.frame(start = c(2, 5), end = c(40, 25), rate = c(3, 2), decay = c(0, 0.1)))
  t <- c(0.5, 3, 10, 26, 41, 120)
  for (spec in list(frontEndAvram2003Thiopental("recirculatory"), frontEndHenthorn2018Ketamine("S"),
                    frontEndAvram2003Thiopental("model1"))) {
    s <- frontEndSimulate(frontEndSystem(spec), doses, t)
    expect_lt(max(abs(s$body_plus_eliminated - s$dosed)), 1e-9 * max(s$dosed))
  }
})

test_that("the steady states are the papers' equations", {
  R0 <- 1.7
  long <- frontEndDoses(infusion = data.frame(start = 0, end = 1e6, rate = R0))
  # Recirculatory: all drug eventually leaves by ClE from arterial blood.
  th <- frontEndAvram2003Thiopental("recirculatory")
  s <- frontEndSimulate(frontEndSystem(th), long, 5e4)
  expect_equal(s$arterial, R0 / th$elimination_clearance, tolerance = 1e-9)
  # Mixing (Henthorn 2018 eq. 2): Ca = R0 / Cl + Cc, with Cc = R0 / ClE.
  k <- frontEndHenthorn2018Ketamine("S")
  s <- frontEndSimulate(frontEndSystem(k), long, 5e4)
  expect_equal(s$central, R0 / k$cl1, tolerance = 1e-9)
  expect_equal(s$arterial, R0 / k$flow + R0 / k$cl1, tolerance = 1e-9)
  expect_equal(s$unmixed, R0 / k$flow, tolerance = 1e-9)
  # Henthorn's venous equation (eq. 4) at equilibrium of the arm.
  expect_equal(s$venous, s$central * (1 - k$arm$fraction + k$arm$fraction * k$arm$partition), tolerance = 1e-9)
})

test_that("the mammillary model agrees with an independent eigen-decomposition", {
  pk <- frontEndAvram2003Thiopental("model1")
  t <- c(0.1, 1, 5, 30, 300)
  s <- frontEndSimulate(frontEndSystem(pk), frontEndDoses(bolus = data.frame(time = 0, amount = 100)), t)
  k10 <- pk$cl1 / pk$v1; k12 <- pk$cl2 / pk$v1; k21 <- pk$cl2 / pk$v2; k13 <- pk$cl3 / pk$v1; k31 <- pk$cl3 / pk$v3
  K <- rbind(c(-(k10 + k12 + k13), k21, k31), c(k12, -k21, 0), c(k13, 0, -k31))
  ev <- eigen(K)
  coef <- solve(ev$vectors, c(100, 0, 0))
  ref <- sapply(t, function(tt) Re(ev$vectors %*% (coef * exp(ev$values * tt)))[1] / pk$v1)
  expect_equal(s$central, ref, tolerance = 1e-10)
})

test_that("the BET regimen holds its own model at the target exactly", {
  for (m in c("model1", "model4")) {
    pk <- frontEndAvram2003Thiopental(m)
    s <- frontEndSimulate(frontEndSystem(pk), frontEndBET(20, pk, 60), c(0, 0.01, 1, 17.5, 59.99))
    expect_equal(s$central, rep(20, 5), tolerance = 1e-9)
  }
})

test_that("a front-end model is refused when its flows do not balance", {
  bad <- frontEndAvram2003Thiopental("recirculatory")
  bad$pathways[[2]]$flow <- 2
  expect_error(frontEndSystem(bad), "blood would be created or lost")
  bad <- frontEndAvram2003Thiopental("recirculatory")
  bad$central[[1]]$n <- 2.5
  expect_error(frontEndSystem(bad), "whole number")
  expect_error(frontEndSystem(list(type = "plug flow")), "Unknown front-end model type")
})

# --- Henthorn 2018 -------------------------------------------------------------

henthornFigure6 <- function(spec)
{
  fig <- utils::read.csv(fixture("henthorn2018-figure6.csv"))
  doses <- frontEndDoses(infusion = data.frame(start = 0, end = 30,
                                               rate = 7 * FRONT_END_KETAMINE_BASE_FRACTION / 30))
  s <- frontEndSimulate(frontEndSystem(spec), doses, sort(unique(fig$time)))
  vapply(split(fig, fig$series), function(d) {
    pred <- 1000 * s[[d$series[1]]][match(d$time, s$time)]
    sqrt(mean((log(d$conc) - log(pred))^2))
  }, numeric(1))
}

test_that("Henthorn 2018 Figure 6 is reproduced from Table 2", {
  # Points digitised from the figure (fixtures/front-end/henthorn2018-figure6.csv),
  # a simulation of 7 mg S(+)-ketamine hydrochloride over 30 min.  The RMS
  # log error of every curve is below 10%; the unmixed line, drawn about 7%
  # above infusion rate / total flow in the figure, is the loosest.
  err <- henthornFigure6(frontEndHenthorn2018Ketamine("S"))
  expect_true(all(err < 0.10), info = paste(names(err), round(err, 3), collapse = " "))
  # The arm link driven by arterial blood misses the arm curve badly.
  arterial <- frontEndHenthorn2018Ketamine("S"); arterial$arm$driver <- "arterial"
  expect_gt(henthornFigure6(arterial)[["arm_tissue"]], 2 * err[["arm_tissue"]])
})

test_that("Henthorn 2018 Table 3: arterial sampling shrinks the conventional VC", {
  skip_on_cran()
  # A conventional three-compartment model fitted to the final model's own
  # arterial samples has a much smaller central volume than one fitted to its
  # venous samples (Table 3: 15.4 against 54.2 L, from the study's data).
  spec <- frontEndHenthorn2018Ketamine("S")
  doses <- frontEndDoses(infusion = data.frame(start = 0, end = 30, rate = 7 * FRONT_END_KETAMINE_BASE_FRACTION / 30))
  t <- c(5, 10, 15, 20, 25, 30, 40, 50, 60, 120, 180, 300)
  s <- frontEndSimulate(frontEndSystem(spec), doses, t)
  start <- list(v1 = 30, v2 = 80, v3 = 150, cl1 = 1.3, cl2 = 2, cl3 = 0.8)
  a <- frontEndFitMammillary(t, s$arterial, doses, start)
  v <- frontEndFitMammillary(t, s$venous, doses, start)
  expect_lt(a$v1, 0.4 * v$v1)
  expect_equal(v$v1, spec$v1, tolerance = 0.25)
})

# --- Avram and Krejcie 2003 ----------------------------------------------------

test_that("Avram 2003 Table 1: TCIs from bolus-derived models overshoot", {
  # BET infusions to 20 mcg/mL for 60 min, designed with each three-compartment
  # model and run through the recirculatory model.  The paper's infusion
  # delta AUC (mean of five dogs): 162.0, 124.6, 17.3 and -3.79 mcg.min/mL;
  # computed here from the mean parameters.
  truth <- frontEndSystem(frontEndAvram2003Thiopental("recirculatory"))
  t <- seq(0, 60, by = 0.01)
  dAUC <- vapply(paste0("model", 1:4), function(m) {
    pk <- frontEndAvram2003Thiopental(m)
    s <- frontEndSimulate(truth, frontEndBET(20, pk, 60), t)
    frontEndAreaFromTarget(s$time, s$arterial, 20)
  }, numeric(1))
  expect_equal(dAUC[["model1"]], 162.0, tolerance = 0.10)
  expect_equal(dAUC[["model2"]], 124.6, tolerance = 0.10)
  expect_equal(dAUC[["model3"]], 17.3, tolerance = 0.25)
  expect_lt(abs(dAUC[["model4"]]), 10)
  expect_true(all(diff(dAUC) < 0))
})

test_that("Avram 2003: the recirculatory model has a first pass and a recirculation peak", {
  sys <- frontEndSystem(frontEndAvram2003Thiopental("recirculatory"))
  t <- seq(0, 2, by = 0.01)
  ca <- frontEndSimulate(sys, frontEndDoses(bolus = data.frame(time = 0, amount = 100)), t)$arterial
  expect_equal(ca[1], 0)
  turn <- which(diff(sign(diff(ca))) != 0) + 1
  # a peak, a trough, then the recirculation peak, all within the first 1.5 min
  expect_gte(length(turn), 3)
  expect_lt(t[turn[3]], 1.5)
  expect_gt(ca[turn[1]], 5 * ca[turn[2]])
})

test_that("the dog of Avram 2003 Figure 3 is reproduced with Krejcie 1996a's central chains", {
  # The central chains' shape is Krejcie 1996a's (antipyrine); this dog's
  # flows and volumes and the nondistributive chain's length were fitted to
  # it (FRONT_END_THIOPENTAL_FIGURE3_DOG, FRONT_END_ND_CHAIN): 8.4%.
  fig <- utils::read.csv(fixture("avram2003-figure3.csv"))
  s <- frontEndSimulate(frontEndSystem(frontEndThiopentalFigure3Dog()),
                        frontEndDoses(bolus = data.frame(time = 0, amount = 100)), fig$time)
  # The first point (8.8 at 0.18 min) is the steepest part of the upstroke,
  # where a reading error of a hundredth of a minute is a factor of two.
  keep <- fig$time > 0.2
  expect_lt(sqrt(mean((log(fig$conc[keep]) - log(s$arterial[keep]))^2)), 0.10)
})

# --- Teaching ---------------------------------------------------------------

test_that("an effect site filters the oscillations, if it is slow enough, but not the central volume", {
  # Thiopental, 100 mg (ke0 values are assumptions for the demonstration).
  # The recirculatory model's arterial curve and model 3's (VC fixed to VC +
  # VND) differ by more than their own peak in the first minute.  With ke0
  # 0.25 /min their effect-site peaks differ by 3%; model 1, whose VC came
  # from samples at 1 min and later, misses it by a third: what matters at a
  # slow effect site is the central volume, not the oscillation.  With ke0 1
  # /min the first pass itself reaches the effect site and the recirculatory
  # peak is 30% above model 3's (docs/front-end-kinetics.md).
  dose <- frontEndDoses(bolus = data.frame(time = 0, amount = 100))
  t <- seq(0, 10, by = 0.01)
  effect <- function(m, driver, ke0) frontEndSimulate(
    frontEndAddEffectSite(frontEndSystem(frontEndAvram2003Thiopental(m)), ke0, driver = driver), dose, t)
  rec <- effect("recirculatory", "arterial", 0.25)
  m3 <- effect("model3", "central", 0.25)
  m1 <- effect("model1", "central", 0.25)
  expect_gt(max(abs(rec$arterial - m3$central)) / max(m3$central), 1)
  expect_equal(max(m3$effect), max(rec$effect), tolerance = 0.05)
  expect_lt(max(m1$effect) / max(rec$effect), 0.75)
  expect_equal(t[which.max(m3$effect)], t[which.max(rec$effect)], tolerance = 0.1)
  fast <- max(effect("recirculatory", "arterial", 1)$effect) / max(effect("model3", "central", 1)$effect)
  expect_gt(fast, 1.2)
})

# --- Henthorn 2018: the supplement's arm -------------------------------------

test_that("Henthorn 2018's arm follows its supplement's code for each enantiomer", {
  # PCarm = V2 / VC of the enantiomer, and the arm rate constant scales with
  # ClE as VC does (Supplemental Digital Content, PML code).
  s <- frontEndHenthorn2018Ketamine("S"); r <- frontEndHenthorn2018Ketamine("R")
  expect_equal(s$arm$partition, 136.58 / 44.93)
  expect_equal(r$arm$partition, 136.58 / (44.93 * 1.06 / 1.23))
  expect_equal(r$arm$k / s$arm$k, r$v1 / s$v1)
})

# --- Model moments ---------------------------------------------------------------

frontEndArterialMoments <- function(spec, dose = 1)
{
  sys <- frontEndSystem(spec)
  keep <- sys$states != "eliminated"
  A0 <- sys$A[keep, keep]; x0 <- dose * sys$entry[keep]; o <- sys$O["arterial", keep]
  inv <- solve(A0)
  auc <- -sum(o * (inv %*% x0)); aumc <- sum(o * (inv %*% inv %*% x0))
  list(auc = auc, vss = dose * aumc / auc^2)
}

test_that("the Northwestern models' steady-state volumes are their tables'", {
  # Vss from the arterial moments (dose AUMC / AUC^2) equals the sum of the
  # volumes, which each table prints, and AUC equals dose / ClE when ClE is
  # taken from arterial blood.
  vss <- c(icg = 2.83, antipyrine = 33.00, thiopental = 53.12)
  cle <- c(icg = 0.33, antipyrine = 0.13, thiopental = 0.17)
  for (m in names(vss)) {
    mo <- frontEndArterialMoments(frontEndAvram2002(m))
    expect_equal(mo$vss, vss[[m]], tolerance = 0.005, info = m)
    expect_equal(mo$auc, 1 / cle[[m]], tolerance = 1e-6, info = m)
  }
  expect_equal(frontEndArterialMoments(frontEndKuipers2001Rocuronium())$vss, 17.29, tolerance = 0.005)
  expect_equal(frontEndArterialMoments(frontEndKuipers2001Rocuronium())$auc, 1 / 0.45, tolerance = 1e-6)
})

test_that("Henthorn 1999's ketamine models conserve mass and differ only in ClE", {
  dose <- frontEndDoses(bolus = data.frame(time = 0, amount = 10))
  t <- c(0.5, 1, 5, 30, 120)
  for (e in c("S", "R")) {
    s <- frontEndSimulate(frontEndSystem(frontEndHenthorn1999Ketamine(e)), dose, t)
    expect_equal(s$body_plus_eliminated, s$dosed, tolerance = 1e-9)
  }
  sS <- frontEndSimulate(frontEndSystem(frontEndHenthorn1999Ketamine("S")), dose, t)
  sR <- frontEndSimulate(frontEndSystem(frontEndHenthorn1999Ketamine("R")), dose, t)
  expect_equal(sS$arterial[1], sR$arterial[1], tolerance = 0.01)   # the first pass is shared
  expect_lt(sS$arterial[5], sR$arterial[5])                        # S(+) is cleared faster
})

# --- Henthorn 1992 -----------------------------------------------------------------

test_that("Henthorn 1992 Figure 3 (dog 2) is reproduced from Table 1", {
  # Points digitised from Figure 3 (fixtures/front-end/henthorn1992-figure3-
  # dog2.csv), 5 mg ICG.  The paper's own curve rises 0.20 min after
  # injection, its Table 1 lag 0.47 L / 1.89 L/min = 0.25 min: the figure's
  # time axis runs 0.04 min ahead.  With that offset the row reproduces the
  # first pass, trough, recirculation and mixing within 16% RMS (log); the
  # remainder is within the table's rounding to 0.01 L (12% when every value
  # is moved by at most 0.005).
  fig <- utils::read.csv(fixture("henthorn1992-figure3-dog2.csv"))
  s <- frontEndSimulate(frontEndSystem(frontEndHenthorn1992ICG(2)),
                        frontEndDoses(bolus = data.frame(time = 0, amount = 5)), seq(0, 2.2, by = 0.0025))
  pred <- stats::approx(s$time - 0.04, s$arterial, fig$time)$y
  expect_lt(sqrt(mean((log(fig$conc) - log(pred))^2)), 0.16)
  unshifted <- stats::approx(s$time, s$arterial, fig$time)$y
  expect_gt(sqrt(mean((log(fig$conc) - log(unshifted))^2)), 0.25)
  # Elimination from the slow circuit, taken from its flow: mass is conserved
  # and ICG is eliminated at ClE times the slow circuit's outflow.
  expect_equal(s$body_plus_eliminated, s$dosed, tolerance = 1e-9)
})

# --- Upton 2004 --------------------------------------------------------------------

test_that("Upton 2004's two-compartment recirculatory model is reproduced exactly", {
  sys <- frontEndSystem(frontEndUpton2004())
  lambda <- sort(-eigen(sys$A[1:2, 1:2])$values)
  expect_equal(lambda, c(0.12051, 2.2128), tolerance = 1e-4)          # eq. 21
  s <- frontEndSimulate(sys, frontEndDoses(infusion = data.frame(start = 0, end = 5, rate = 20)), seq(0, 40, by = 0.01))
  expect_equal(max(s$arterial), 6.30, tolerance = 0.002)               # Figure 3
  expect_equal(s$body_plus_eliminated, s$dosed, tolerance = 1e-9)     # ClE extracted, not added
  ss <- frontEndSimulate(sys, frontEndDoses(infusion = data.frame(start = 0, end = 2000, rate = 20)), 1999)
  expect_equal(ss$arterial, 20 / 2, tolerance = 1e-6)                  # R0 / Q_Cl
  expect_equal(ss$mixed_venous, 20 / 2 * (1 - 2 / 5), tolerance = 1e-6) # (R0 / Q_Cl)(1 - E_B)
  # Figure 4: 100 mg over 30 s, cardiac output 3, 6 and 9 L/min
  peaks <- vapply(c(3, 6, 9), function(co) max(frontEndSimulate(frontEndSystem(frontEndUpton2004(cardiac_output = co)),
    frontEndDoses(infusion = data.frame(start = 0, end = 0.5, rate = 200)), seq(0, 10, by = 0.001))$arterial), 0)
  expect_equal(peaks, c(30.2, 23.9, 19.7), tolerance = 0.005)
})

# --- Olofsen 2010 -----------------------------------------------------------------

test_that("Olofsen 2010's venous chain has the paper's half-lives and crossing", {
  sys <- frontEndSystem(frontEndOlofsen2010M6G())
  v <- match(c("venous", "venous2"), sys$states)
  halfLives <- sort(log(2) / -eigen(sys$A[v, v])$values)
  expect_equal(halfLives, c(1.4, 33), tolerance = 0.06)               # Discussion
  # 0.3 mg/kg over 90 s (70 kg): arterial above venous early, below later
  s <- frontEndSimulate(sys, frontEndDoses(infusion = data.frame(start = 0, end = 1.5, rate = 21 / 1.5)),
                        c(5, 20, 120, 240))
  expect_gt(s$arterial[1], s$venous[1])
  expect_lt(s$arterial[3], s$venous[3])
  expect_equal(s$body_plus_eliminated, s$dosed, tolerance = 1e-9)
})

test_that("state limits apply before allocation, including effect sites", {
  sys <- frontEndNewSystem()
  sys$names <- rep("state", FRONT_END_MAX_STATES)
  expect_error(frontEndAddState(sys, "extra"), "at most 400")
  sys <- list(A = matrix(0, 400, 400), O = matrix(0, 1, 400,
    dimnames = list("arterial", NULL)))
  expect_error(frontEndAddEffectSite(sys, 1), "at most 400")
})

test_that("pathway names cannot overwrite chain keys or generated states", {
  spec <- list(type = "recirculatory", cardiac_output = 2,
    central = list(volume = 1, n = 2),
    pathways = list(list(name = "body", type = "delay", volume = 2, n = 2, flow = 1),
                    list(name = "other", type = "delay", volume = 2, n = 2, flow = 1)))
  expect_silent(frontEndSystem(spec))
  for (name in c("central1", "central1[1]", "eliminated", "body[1]")) {
    spec$pathways[[2]]$name <- name
    expect_error(frontEndSystem(spec), "collides")
  }
})

test_that("rounding flows preserve positive concentrations and mass balance", {
  for (inFlow in c(TRUE, FALSE)) for (offset in c(-0.005, 0, 0.005)) {
    spec <- list(type = "recirculatory", cardiac_output = 2,
      central = list(volume = 1), elimination_clearance = 0.2,
      elimination_in_flow = inFlow,
      pathways = list(list(name = "body", type = "tissue", volume = 3,
        flow = 2 + offset - if (inFlow) 0.2 else 0)))
    sys <- frontEndSystem(spec)
    expect_lt(max(abs(colSums(sys$A))), 1e-12)
    expect_true(all(sys$O >= 0))
    # The published clearance is kept: only the pathways' flows absorb the excess.
    expect_equal(sys$A[match("eliminated", sys$states), match("central1", sys$states)], 0.2, tolerance = 1e-12)
    sim <- frontEndSimulate(sys, frontEndDoses(bolus = data.frame(time = 0, amount = 10)), c(0, 1, 10))
    expect_equal(sim$body_plus_eliminated, sim$dosed, tolerance = 1e-10)
    expect_true(all(sim$mixed_venous >= 0))
  }
})

test_that("dose fractions must be finite and nonnegative but observation weights may be signed", {
  spec <- list(type = "compartmental", compartments = list(list(name = "a", volume = 1),
    list(name = "b", volume = 1)), observe = list(difference = c(a = 1, b = -1)))
  expect_silent(frontEndSystem(spec))
  for (w in list(c(a = 2, b = -1), c(a = Inf), c(a = NA_real_), c(a = NaN))) {
    spec$dose_into <- w
    expect_error(frontEndSystem(spec), "finite and nonnegative")
  }
})

test_that("a single central element may be a series of segments", {
  spec <- list(type = "recirculatory", cardiac_output = 2,
    central = list(segments = list(list(volume = 1, n = 1), list(volume = 1, n = 3))),
    pathways = list(list(name = "body", type = "tissue", volume = 3, flow = 1.8)),
    elimination_clearance = 0.2)
  sys <- frontEndSystem(spec)
  expect_equal(sum(grepl("^central1", sys$states)), 4)
  sim <- frontEndSimulate(sys, frontEndDoses(bolus = data.frame(time = 0, amount = 10)), c(0, 1, 10))
  expect_equal(sim$body_plus_eliminated, sim$dosed, tolerance = 1e-10)
})

test_that("the exponential cache is bounded without changing the answer", {
  sys <- frontEndSystem(frontEndUpton2004())
  doses <- frontEndDoses(bolus = data.frame(time = 0, amount = 100))
  times <- sort(c(0, cumsum(seq(0.01, 0.01 * 3 * FRONT_END_EXPM_CACHE, by = 0.01))))
  all <- frontEndSimulate(sys, doses, times)
  expect_equal(all$body_plus_eliminated, all$dosed, tolerance = 1e-9)
  for (t in times[c(10, 2 * FRONT_END_EXPM_CACHE, length(times))])
    expect_equal(all$arterial[match(t, all$time)], frontEndSimulate(sys, doses, t)$arterial, tolerance = 1e-10)
})
