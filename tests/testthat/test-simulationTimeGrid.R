# The time line the closed-form engines simulate on, and the dose lines laid
# on it (R/simulationTimeGrid.R).  Drafted by Claude Code, 2026-10-07, at the
# request of Steven L. Shafer.
#
# Up to a day the line must be the one the engines always built, point for
# point; beyond a day it is scaled to the plot.  The references below are the
# code the engines ran before the refactoring, copied verbatim, so that
# "unchanged" is checked against what it was rather than against itself.

local_mocked_bindings(outputComments = function(...) {})

noEvents <- data.frame(Time = double(), Event = character())
W52 <- 52 * MINS_PER_WEEK

# advanceClosedForm0()'s time line before 2026-10-07.
legacyGrid <- function(knots, maximum, start) {
  timeLine <- sort(unique(c(0, knots, maximum)))
  timeLine <- timeLine[timeLine >=0]
  gapStart <- timeLine[1:length(timeLine)-1]
  gapEnd   <- timeLine[2:length(timeLine)]
  newTimes <- c(exp(log(start)+0:40 * log(MINS_PER_DAY/start)/41))
  for (i in 1:length(gapEnd))
  {
    distance <- gapEnd[i] - gapStart[i]
    timeLine <- c(timeLine, gapStart[i] + newTimes[newTimes <= distance])
  }
  sort(unique(timeLine))
}

# The engines' per-point dose loop before 2026-10-07, in the general form
# advanceClosedForm1() had it: a bolus line, a line per extravascular route,
# and an infusion for anything else.
legacyDoseLines <- function(dose, timeLine, routes) {
  L <- length(timeLine)
  bolusLine <- infusionLine <- dt <- rate <- rep(0, L)
  depot <- matrix(0, L, length(routes), dimnames = list(NULL, routes))
  extravascular <- rep(FALSE, nrow(dose))
  for (r in routes) extravascular <- extravascular | dose[[r]]
  for (i in 1:L)
  {
    bolusLine[i] <- sum(dose$Dose[dose$Time == timeLine[i] & dose$Bolus])
    for (r in routes)
      depot[i, r] <- sum(dose$Dose[dose$Time == timeLine[i] & dose[[r]]])
    USE <- dose$Time == timeLine[i] & !dose$Bolus & !extravascular
    if (i == 1)
    {
      infusionLine[i] <- sum(dose$Dose[USE])
      rate[1] <- 0
      dt[1] <- 0
    } else {
      if (sum(USE) == 0)
      {
        infusionLine[i] <- infusionLine[i-1]
      } else {
        infusionLine[i] <- sum(dose$Dose[USE])
      }
      dt[i] <- timeLine[i] - timeLine[i-1]
      rate[i] <- infusionLine[i-1]
    }
  }
  out <- list(bolus = bolusLine, infusion = infusionLine, rate = rate, dt = dt)
  for (r in routes) out[[r]] <- depot[, r]
  out
}

# advanceState() before 2026-10-07.
legacyAdvanceState <- function(l, bolus, infusion, start, L) {
  Z <- lapply(1:L, function(i) list(l = l[i], bolus = bolus[i], infusion = infusion[i]))
  Reduce(function(state, Z) {state * Z$l +  Z$bolus + Z$infusion},
         Z, init = start, accumulate = TRUE)[2:(L+1)]
}

# The steps the adaptive fill takes, worked out from the definition.
fillRatio <- function(start) (MINS_PER_DAY / start)^(1 / GRID_LOG_POINTS)
maxStep   <- function(maximum) maximum / GRID_UNIFORM_POINTS

# An upper bound on the points of an adaptive line, from its definition: each
# gap of length D gets at most as many geometric offsets as fit below D, and
# never more than the 2 + log(FINE / (UNIFORM (r - 1))) / log(r) that fit
# below the hand-over to uniform steps; the uniform steps over the whole plot
# cannot outnumber GRID_UNIFORM_POINTS.
gridBound <- function(knots, maximum, start = 1) {
  knots <- sort(unique(c(0, knots, maximum)))
  knots <- knots[knots >= 0 & knots <= maximum]
  r <- fillRatio(start)
  tau0 <- max(start, maximum / GRID_FINE_POINTS)
  D <- diff(knots)
  geo <- pmin(pmax(0, ceiling(log(D / tau0) / log(r))),
              2 + log(GRID_FINE_POINTS / (GRID_UNIFORM_POINTS * (r - 1))) / log(r))
  length(knots) + sum(geo) + GRID_UNIFORM_POINTS
}

sim <- function(dose, maximum, plotRecovery = FALSE) {
  simulateDrugsWithCovariates(dose, noEvents, 70, 171, 50, "male", maximum, plotRecovery)
}


test_that("a plot of a day or less gets exactly the line it always had", {
  set.seed(20261007)
  for (trial in 1:60) {
    maximum <- sample(c(60, 120, 240, 360, 720, 1440), 1)
    knots <- c(sample(c(-5, 0, 0.01, 10, 10, 30), 3, replace = TRUE),
               runif(sample(0:12, 1), -10, maximum * 1.2))
    knots <- c(knots, knots - PRE_DOSE_OFFSET)
    start <- sample(c(1, 0.3927911, runif(1, 0.05, 1)), 1)
    expect_identical(simulationTimeGrid(knots, maximum, start),
                     legacyGrid(knots, maximum, start))
  }
  expect_identical(simulationTimeGrid(numeric(0), 1440, 1), legacyGrid(numeric(0), 1440, 1))
})


test_that("the pinned remifentanil grid is reproduced", {
  # The grid test-advanceClosedForm0.R and test-simCpCe.R pin for a 60-minute
  # remifentanil run with no doses: start, then start x (1440 / start)^(k/41)
  # while it fits, then 60.
  pinned <- c(0.0000000, 0.3927911, 0.4798366, 0.5861721, 0.7160723, 0.8747594,
              1.0686127, 1.3054254, 1.5947176, 1.9481191, 2.3798371, 2.9072271,
              3.5514907, 4.3385281, 5.2999789, 6.4744945, 7.9092917, 9.6620508,
              11.8032348, 14.4189213, 17.6142639, 21.5177187, 26.2862088,
              32.1114325, 39.2275701, 47.9206978, 58.5402888, 60.0000000)
  start <- gridStart(0.693 / 4 / 0.3927911)
  g <- simulationTimeGrid(numeric(0), 60, start)
  expect_equal(g, pinned, tolerance = 1e-7)
  # and from the power form, written independently of the code's exp/log form
  k <- 0:40
  powers <- start * (MINS_PER_DAY / start)^(k / 41)
  expect_equal(g, c(0, powers[powers <= 60], 60), tolerance = 1e-12)
})


test_that("gridStart() is a quarter of the ke0 half-time, at most a minute", {
  expect_equal(gridStart(0.5), 0.693 / 0.5 / 4)
  expect_equal(gridStart(0.1), 1)
  expect_equal(gridStart(0), 1)
  expect_equal(gridStart(NULL), 1)
  # A prodrug's grid follows its metabolite's effect site; its own comes first
  expect_equal(gridStart(0, 0.5), 0.693 / 0.5 / 4)
  expect_equal(gridStart(0.5, 2), 0.693 / 0.5 / 4)
  expect_equal(gridStart(0, NULL), 1)
  expect_equal(gridStart(0, 0), 1)
})


test_that("every knot survives, sorted and unique, at every length", {
  set.seed(1)
  for (maximum in c(60, 1440, 2880, MINS_PER_WEEK, 13 * MINS_PER_WEEK, W52)) {
    for (trial in 1:10) {
      doses <- c(0, runif(sample(1:30, 1), 0, maximum))
      knots <- c(doses, doses - PRE_DOSE_OFFSET)
      start <- runif(1, 0.1, 1)
      g <- simulationTimeGrid(knots, maximum, start)
      expect_true(all(knots[knots >= 0] %in% g))
      expect_true(all(c(0, maximum) %in% g))
      expect_false(is.unsorted(g, strictly = TRUE))
      expect_true(all(g >= 0))
    }
  }
})


test_that("beyond a day no step is longer than maximum / GRID_UNIFORM_POINTS", {
  set.seed(2)
  for (maximum in c(1441, 2880, MINS_PER_WEEK, 8 * MINS_PER_WEEK, W52, MINS_PER_YEAR)) {
    for (n in c(0, 1, 7, 50)) {
      doses <- runif(n, 0, maximum)
      g <- simulationTimeGrid(c(doses, doses - PRE_DOSE_OFFSET), maximum, runif(1, 0.1, 1))
      expect_lte(max(diff(g)), maxStep(maximum) * (1 + 1e-9))
    }
  }
  # The line before 2026-10-07 drew one chord across 363 of the 364 days
  # after a single dose.
  expect_gt(max(diff(legacyGrid(0, W52, 1))), 360 * MINS_PER_DAY)
})


test_that("the fill after a knot starts at maximum / GRID_FINE_POINTS", {
  g <- simulationTimeGrid(0, W52, 1)
  tau0 <- W52 / GRID_FINE_POINTS
  expect_equal(g[2], tau0)
  expect_equal(g[3] / g[2], fillRatio(1))
  # ... or at the drug's own start, on a plot short enough that it is later
  g <- simulationTimeGrid(0, 2 * MINS_PER_DAY, 0.5)
  expect_equal(g[2], 0.5)
  expect_equal(g[3] / g[2], fillRatio(0.5))
})


test_that("the line does not grow with the plot beyond its doses", {
  # One dose: about 500 points at any length, against 43 that drew a chord
  for (maximum in c(2 * MINS_PER_DAY, 4 * MINS_PER_WEEK, W52, MINS_PER_YEAR)) {
    g <- simulationTimeGrid(c(0, 60, 60 - PRE_DOSE_OFFSET), maximum, 1)
    expect_lte(length(g), gridBound(c(0, 60, 60 - PRE_DOSE_OFFSET), maximum, 1))
    expect_lte(length(g), GRID_UNIFORM_POINTS + 2 * 33 + 4)
  }
  # A knot far past the end of the plot costs only its geometric fill
  g <- simulationTimeGrid(c(0, 100 * W52), W52, 1)
  expect_lt(sum(g > W52), 40)
})


test_that("bounded points: seven rate changes over 52 weeks", {
  days <- c(0, 2, 7, 14, 21, 28, 90)
  dose <- data.frame(Drug = "morphine", Time = days * MINS_PER_DAY,
                     Dose = c(4, 3, 2.5, 2, 1.5, 1, 0.86), Units = "mg/hr")
  w <- sim(dose, W52)$morphine$wide
  expect_lte(nrow(w), gridBound(dose$Time, W52))
  expect_lte(max(diff(w$Time)), maxStep(W52) * (1 + 1e-9))
  expect_true(all(dose$Time %in% w$Time))
})


test_that("bounded points: oral doses once and four times a day for 52 weeks", {
  for (f in c("qd", "qid")) {
    dose <- data.frame(Drug = "oxycodone", Time = 0, Dose = 10,
                       Units = paste("mg PO", f))
    o <- sim(dose, W52)
    w <- o$oxycodone$wide
    given <- seq(0, W52 - 1, by = SCHEDULE_INTERVALS[[f]])
    knots <- c(given, given - PRE_DOSE_OFFSET)
    expect_true(all(knots[knots >= 0] %in% w$Time))
    expect_lte(nrow(w), gridBound(knots, W52))
    # Fewer points than the line before, 41 per gap, which also drew the
    # peaks, but at far greater cost (15,652 and 52,416 points).
    old <- length(legacyGrid(knots, W52, 1))
    expect_lt(nrow(w), old)
    # The metabolite row is folded onto the same line
    expect_equal(nrow(o$oxymorphone$wide), nrow(w))
  }
})


test_that("oral peaks are drawn under daily dosing for 52 weeks", {
  # The fill starts maximum / GRID_FINE_POINTS after each dose, 26 minutes
  # here, before oxycodone's plasma peak at 30 minutes.  The peak after each
  # dose is then drawn to within 1% of its true height, which comes from the
  # closed form: the plasma before the last dose has reached a steady state,
  # and each dose adds the single-dose curve (independent of the grid).
  PK <- getDrugPK("oxycodone", 70, 171, 50, "male", getDrugDefaults("oxycodone"))$PK$default
  lam <- c(PK$lambda_1, PK$lambda_2, PK$lambda_3, PK$ka_PO)
  cf  <- c(PK$p_coef_PO_l1, PK$p_coef_PO_l2, PK$p_coef_PO_l3, PK$p_coef_PO_ka) * 10 / 0.001
  use <- lam > 0 & cf != 0
  # Steady state under dosing every tau: each exponential's sum over all
  # earlier doses is a geometric series.
  steady <- function(t, tau) sum(cf[use] * exp(-lam[use] * t) / (1 - exp(-lam[use] * tau)))
  truePeak <- stats::optimize(steady, c(0, 240), tau = MINS_PER_DAY, maximum = TRUE)$objective

  w <- sim(data.frame(Drug = "oxycodone", Time = 0, Dose = 10, Units = "mg PO qd"), W52)$oxycodone$wide
  lastDay <- w$Time >= W52 - MINS_PER_DAY & w$Time < W52
  expect_equal(max(w$Plasma[lastDay]), truePeak, tolerance = 0.01)
})


test_that("a single oral dose on a 52-week plot has a curved washout", {
  # Hydrocodone, whose washout lasts days.  The old line drew it as a straight
  # chord from 20 hours to the end of the plot, which put the concentration
  # on day 2 at about a fifth of the peak; it is under 2%.  The drawn line is
  # compared hour by hour with the closed form for one oral dose, which needs
  # no grid at all.
  w <- sim(data.frame(Drug = "hydrocodone", Time = 0, Dose = 20, Units = "mg PO"),
           W52)$hydrocodone$wide
  PK <- getDrugPK("hydrocodone", 70, 171, 50, "male", getDrugDefaults("hydrocodone"))$PK$default
  lam <- c(PK$lambda_1, PK$lambda_2, PK$lambda_3, PK$ka_PO)
  cf  <- c(PK$p_coef_PO_l1, PK$p_coef_PO_l2, PK$p_coef_PO_l3, PK$p_coef_PO_ka) * 20 / 0.001
  exact <- function(t) as.vector(exp(-outer(t, lam)) %*% cf)

  hours <- seq(MINS_PER_DAY, 10 * MINS_PER_DAY, by = 60)
  drawn <- stats::approx(w$Time, w$Plasma, hours)$y
  peak  <- max(w$Plasma)
  expect_lt(max(abs(drawn - exact(hours))), 0.01 * peak)

  # The washout is a curve of many points, not one chord
  expect_gt(sum(w$Time > MINS_PER_DAY & w$Time < 10 * MINS_PER_DAY), 10)

  # And the same comparison on the old line fails it by an order of magnitude
  old <- legacyGrid(c(0, -PRE_DOSE_OFFSET), W52, 1)
  oldDrawn <- stats::approx(old, exact(old), hours)$y
  expect_gt(max(abs(oldDrawn - exact(hours))), 0.1 * peak)
})


test_that("values at shared points equal a dense reference", {
  # The engines are exact at any point, so adding points must not move the
  # ones already there.  The reference runs every engine on the same line
  # with nine more points inside every step.
  realGrid <- simulationTimeGrid
  dense <- function(knots, maximum, start = 1) {
    g <- realGrid(knots, maximum, start)
    f <- (1:9) / 10
    sort(unique(c(g, as.vector(outer(g[-length(g)], rep(1, 9)) + outer(diff(g), f)))))
  }
  cases <- list(
    # advanceClosedForm0
    list(data.frame(Drug = "morphine", Time = c(0, 2, 30) * MINS_PER_DAY,
                    Dose = c(5, 2, 0), Units = c("mg", "mg/hr", "mg/hr")), "morphine"),
    # advanceClosedFormPO_IM_IN
    list(data.frame(Drug = "oxymorphone", Time = c(0, 3 * MINS_PER_DAY), Dose = 40,
                    Units = "mg PO"), "oxymorphone"),
    # advanceClosedFormMetabolite, and the fold into the formed drug's row
    # (whose formed concentration stays below its threshold: no time to compare)
    list(data.frame(Drug = "oxycodone", Time = 0, Dose = 20, Units = "mg PO bid"),
         c("oxycodone", "oxymorphone"))
  )
  for (case in cases) {
    for (maximum in c(MINS_PER_DAY, 2 * MINS_PER_WEEK)) {
      A <- sim(case[[1]], maximum, plotRecovery = TRUE)
      B <- with_mocked_bindings(sim(case[[1]], maximum, plotRecovery = TRUE),
                                simulationTimeGrid = dense)
      for (row in case[[2]]) {
        a <- A[[row]]$wide
        b <- B[[row]]$wide
        expect_gt(nrow(b), 5 * nrow(a))
        at <- match(a$Time, b$Time)
        expect_false(anyNA(at))
        expect_equal(a$Plasma, b$Plasma[at], tolerance = 1e-9)
        expect_equal(a$`Effect Site`, b$`Effect Site`[at], tolerance = 1e-9)
        # Time until threshold is solved to 0.01 minutes, so two solutions of
        # the same problem agree to 0.02
        expect_lt(max(abs(a$Recovery - b$Recovery[at])), 0.02)
      }
      expect_gt(max(A[[case[[2]][1]]]$wide$Recovery), 0)
    }
  }
})


test_that("doseLines() is the loop it replaced, on random dose tables", {
  set.seed(20261007)
  routeSets <- list(character(0), "PO", c("PO", "IM", "IN"))
  for (trial in 1:200) {
    n <- sample(1:25, 1)
    maximum <- sample(c(240, 1440, 3 * MINS_PER_DAY), 1)
    # Few distinct times, so that several rows share one; some at zero, some
    # before it and some past the end, which land on no point of a clipped line
    times <- sample(c(0, 0, 5, 10, 10, 37.5, 60, 61.25, 239.99, 240,
                      1440, -5, 4 * MINS_PER_DAY), n, replace = TRUE)
    routes <- routeSets[[sample(length(routeSets), 1)]]
    kind <- sample(c("bolus", "infusion", routes), n, replace = TRUE)
    dose <- data.frame(
      Time  = times,
      # Zero-dose rows, which stop an infusion, and unrounded amounts, so that
      # sums of three or more would show a change in the order of addition
      Dose  = ifelse(runif(n) < 0.25, 0, runif(n, 0, 1000) / 3),
      Bolus = kind == "bolus"
    )
    for (r in routes) dose[[r]] <- kind == r
    knots <- c(dose$Time, dose$Time - PRE_DOSE_OFFSET)
    timeLine <- simulationTimeGrid(knots, maximum, runif(1, 0.2, 1))
    if (runif(1) < 0.5) timeLine <- timeLine[timeLine <= maximum]  # advanceClosedForm1's clip

    expect_identical(doseLines(dose, timeLine, routes),
                     legacyDoseLines(dose, timeLine, routes)[c("bolus", routes, "infusion", "rate", "dt")])
  }
})


test_that("doseLines() keeps the infusion rules", {
  timeLine <- c(0, 1, 2, 3, 4, 5)
  dose <- data.frame(Time = c(1, 1, 3, 4, 4), Dose = c(2, 3, 0, 7, 1),
                     Bolus = c(FALSE, FALSE, FALSE, TRUE, FALSE))
  x <- doseLines(dose, timeLine)
  # Two rows at one time are summed; a zero-dose row stops the infusion; the
  # rate over a step is the one set at its start
  expect_equal(x$infusion, c(0, 5, 5, 0, 1, 1))
  expect_equal(x$rate,     c(0, 0, 5, 5, 0, 1))
  expect_equal(x$bolus,    c(0, 0, 0, 0, 7, 0))
  expect_equal(x$dt,       c(0, 1, 1, 1, 1, 1))
  # A missing route column is a route nobody took
  y <- doseLines(dose, timeLine, "PO")
  expect_equal(y$PO, rep(0, 6))
  expect_equal(y$infusion, x$infusion)
})


test_that("advanceState() is the Reduce() it replaced, bit for bit", {
  set.seed(3)
  for (L in c(1, 2, 17, 500)) {
    l <- exp(-runif(L, 0, 3)); b <- runif(L) * (runif(L) < 0.2); u <- runif(L) / 7
    s0 <- runif(1)
    expect_identical(advanceState(l, b, u, s0, L), legacyAdvanceState(l, b, u, s0, L))
    po <- runif(L) * (runif(L) < 0.3); im <- runif(L) / 3; inl <- runif(L) / 11
    # advanceStatePO adds PO, IM and IN after the infusion, from zero
    old <- Reduce(function(s, i) s * l[i] + b[i] + u[i] + po[i] + im[i] + inl[i],
                  seq_len(L), init = 0, accumulate = TRUE)[-1]
    expect_identical(advanceStatePO(l, b, u, po, im, inl, L), old)
  }
})


test_that("the plasma horizon is a week, or the plot if that is longer", {
  expect_equal(recoveryHorizonPlasma(60), MINS_PER_WEEK)
  expect_equal(recoveryHorizonPlasma(MINS_PER_WEEK), MINS_PER_WEEK)
  expect_equal(recoveryHorizonPlasma(W52), W52)

  # One compartment, half-life 10 days, timed on the plasma (no effect site).
  # From C0 the plasma falls to C0 / 32 in five half-lives, 50 days, which a
  # week's horizon reported as "a week".
  pk <- getDrugPK("morphine", 70, 171, 50, "male", getDrugDefaults("morphine"))$PK$default
  for (n in grep("_coef_", names(pk), value = TRUE)) pk[[n]] <- 0
  k <- log(2) / (10 * MINS_PER_DAY)
  V <- 100
  pk$lambda_1 <- k
  pk$p_coef_bolus_l1    <- 1 / V
  pk$p_coef_infusion_l1 <- 1 / V / k
  pk$ke0 <- 0
  dose <- data.frame(Time = 0, Dose = 3200, Bolus = TRUE)
  threshold <- 3200 / V / 32

  long <- advanceClosedForm0(dose, pk, W52, TRUE, threshold)
  expect_equal(long$Recovery[1], 50 * MINS_PER_DAY, tolerance = 1e-6)
  expect_equal(attr(long, "recoveryStates")$horizon, W52)
  # A day later it is a day less, all the way down
  at <- which(long$Time >= 20 * MINS_PER_DAY)[1]
  expect_equal(long$Recovery[at], 50 * MINS_PER_DAY - long$Time[at], tolerance = 1e-6)

  # A plot of a week or less keeps the week's horizon, so its answer is
  # unchanged: still above the threshold after a week
  short <- advanceClosedForm0(dose, pk, MINS_PER_WEEK, TRUE, threshold)
  expect_equal(short$Recovery[1], MINS_PER_WEEK)
  expect_equal(attr(short, "recoveryStates")$horizon, MINS_PER_WEEK)
})


test_that("every engine that times on the plasma scales its horizon", {
  # advanceClosedFormPO_IM_IN: cefalexin has no effect site
  for (maximum in c(MINS_PER_DAY, 4 * MINS_PER_WEEK)) {
    PK <- getDrugPK("cefalexin", 70, 171, 50, "male", getDrugDefaults("cefalexin"))$PK$default
    dose <- data.frame(Time = 0, Dose = 500, Bolus = FALSE, PO = TRUE, IM = FALSE, IN = FALSE)
    r <- advanceClosedFormPO_IM_IN(dose, PK, maximum, TRUE, 1)
    expect_equal(attr(r, "recoveryStates")$horizon, recoveryHorizonPlasma(maximum))

    # advanceClosedFormMetabolite: codeine's own row, and the formed morphine
    # when it has no effect site, are timed on the plasma
    PK <- getDrugPK("codeine", 70, 171, 50, "male", getDrugDefaults("codeine"))$PK$default
    PK$metabolite$ke0 <- 0
    dose <- data.frame(Time = 0, Dose = 60000, Bolus = FALSE, PO = TRUE, IM = FALSE, IN = FALSE)
    r <- advanceClosedFormMetabolite(dose, PK, maximum, TRUE, 1)
    expect_equal(attr(r, "recoveryStates")$horizon, recoveryHorizonPlasma(maximum))
    expect_equal(attr(r, "metaboliteRecoveryStates")$horizon, recoveryHorizonPlasma(maximum))
  }
})


test_that("a year of daily oral doses simulates within a time budget", {
  skip_on_cran()
  # 0.5 s on the development machine with the time until threshold, against
  # 2.7 s on the line before this change; the budget is loose for slow CI.
  dose <- data.frame(Drug = "oxycodone", Time = 0, Dose = 10, Units = "mg PO qd")
  elapsed <- system.time(o <- sim(dose, W52, plotRecovery = TRUE))[["elapsed"]]
  expect_lt(elapsed, 4)
  expect_true(all(is.finite(o$oxycodone$wide$Recovery)))
})
