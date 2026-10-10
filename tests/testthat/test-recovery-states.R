# The machinery in R/recoveryStates.R, which carries the effect site as one
# amplitude per eigenvalue rather than as a curve, so that a drug arriving partly
# as another drug's active metabolite can have its "time until threshold" solved
# from the combined state.
#
# (Claude Code, Claude Opus 5, 2026-10-05; run on R 4.6.1.)


# A state obeying  s(t) = I + (s0 - I) exp(-lambda (t - t0)):  the exact
# solution for a constant input I, which is what holds between any two
# neighbouring points of an engine's time line.
oneSegment <- function(t, t0, s0, lambda, I) I + (s0 - I) * exp(-lambda * (t - t0))


test_that("a free decay is carried onto another time line exactly", {
  lambda <- c(0.3, 0.02, 0.001)
  coarse <- c(0, 5, 30, 120, 600)
  decay  <- function(t) outer(t, lambda, function(a, b) 3 * exp(-b * a))

  set <- recoveryStateSet(coarse, decay(coarse), lambda)
  fine <- sort(unique(c(coarse, seq(0, 600, by = 7), 1.3, 417.9)))

  got <- advanceStatesOnto(set, fine)
  expect_equal(got$time, fine)
  expect_equal(got$state, decay(fine), tolerance = 1e-12)
  expect_equal(got$lambda, matrix(lambda, length(fine), 3, byrow = TRUE))
})


test_that("a constant input is carried exactly, not interpolated", {
  # Linear interpolation of this curve would be badly wrong over the long
  # intervals a geometric time line has late on, so this is the test that the
  # input is solved for rather than assumed away.
  lambda <- 0.05
  I      <- 4
  coarse <- c(0, 2, 40, 300)
  s      <- oneSegment(coarse, 0, 0, lambda, I)

  set  <- recoveryStateSet(coarse, matrix(s, ncol = 1), lambda)
  fine <- seq(0, 300, by = 3)
  got  <- advanceStatesOnto(set, fine)

  expect_equal(as.vector(got$state), oneSegment(fine, 0, 0, lambda, I),
               tolerance = 1e-12)
  # And it really is a case linear interpolation gets wrong
  linear <- stats::approx(coarse, s, fine)$y
  expect_gt(max(abs(linear - oneSegment(fine, 0, 0, lambda, I))), 0.1)
})


test_that("an input that changes at a time-line point is carried exactly", {
  # An infusion rate change: zero, then 5, then zero again, each change landing
  # on a point of the coarse line, which is how the engines build it.
  lambda <- 0.08
  coarse <- c(0, 10, 25, 60, 100, 200)
  rates  <- c(0, 0, 5, 5, 0, 0)          # input in force arriving at each point

  s <- numeric(length(coarse))
  s[1] <- 1
  for (i in 2:length(coarse))
    s[i] <- oneSegment(coarse[i], coarse[i - 1], s[i - 1], lambda, rates[i])

  set <- recoveryStateSet(coarse, matrix(s, ncol = 1), lambda)
  fine <- sort(unique(c(coarse, seq(0, 200, by = 2.5))))
  got  <- advanceStatesOnto(set, fine)

  want <- vapply(fine, function(t) {
    i <- findInterval(t, coarse)
    if (i == length(coarse)) return(oneSegment(t, coarse[i], s[i], lambda, 0))
    oneSegment(t, coarse[i], s[i], lambda, rates[i + 1])
  }, numeric(1))
  expect_equal(as.vector(got$state), want, tolerance = 1e-12)
})


test_that("a zero eigenvalue accumulates linearly", {
  # A compartment the model does not use is carried as a zero lambda, and so is
  # an absorption constant for a route the drug is not given by.
  coarse <- c(0, 10, 50)
  state  <- matrix(c(0, 2, 10), ncol = 1)
  set    <- recoveryStateSet(coarse, state, 0)

  got <- advanceStatesOnto(set, c(0, 5, 10, 30, 50))
  expect_equal(as.vector(got$state), c(0, 1, 2, 6, 10), tolerance = 1e-12)
})


test_that("an exact hit, the same time line, and the far end are all handled", {
  lambda <- c(0.1, 0.01)
  coarse <- c(0, 7, 50)
  S <- cbind(c(1, 0.5, 0.1), c(2, 1.9, 1.2))
  set <- recoveryStateSet(coarse, S, lambda)

  # The engine's own line comes straight back
  expect_identical(advanceStatesOnto(set, coarse)$state, S)
  # And so does any subset of it
  expect_equal(advanceStatesOnto(set, c(0, 50))$state, S[c(1, 3), ],
               tolerance = 1e-12)
  # Past the end there is no further input, so the states decay freely
  beyond <- advanceStatesOnto(set, 80)$state
  expect_equal(as.vector(beyond), S[3, ] * exp(-lambda * 30), tolerance = 1e-12)
})


test_that("effectSiteCoefficients reproduces getDrugPK's own three-compartment lines", {
  # getDrugPK.R writes e_coef_bolus_l1 <- p_coef_bolus_l1 / (ke0 - lambda_1) *
  # ke0 and so on.  This is the same operation generalised to any eigenvalue
  # set, which is what a metabolite needs, so it has to agree where the two
  # overlap.
  pk <- getDrugPK("fentanyl", 70, 170, 50, "male",
                  getDrugDefaults("fentanyl"))$PK$default

  got <- effectSiteCoefficients(
    list(lambda = c(pk$lambda_1, pk$lambda_2, pk$lambda_3),
         bolus  = c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)),
    pk$ke0)

  expect_equal(got$lambda, c(pk$lambda_1, pk$lambda_2, pk$lambda_3, pk$ke0))
  expect_equal(got$bolus, c(pk$e_coef_bolus_l1, pk$e_coef_bolus_l2,
                            pk$e_coef_bolus_l3, pk$e_coef_bolus_ke0),
               tolerance = 1e-12)
  expect_equal(got$infusion, c(pk$e_coef_infusion_l1, pk$e_coef_infusion_l2,
                               pk$e_coef_infusion_l3, pk$e_coef_infusion_ke0),
               tolerance = 1e-12)
  # The effect site starts at zero when the plasma jumps, which is what the
  # ke0 coefficient being minus the sum of the others means.
  expect_equal(sum(got$bolus), 0, tolerance = 1e-15)
})


test_that("effectSiteCoefficients agrees with getDrugPK by the oral route too", {
  # getDrugPK retards the effect-site BOLUS coefficients by absorption;
  # effectSiteCoefficients links the already-retarded PLASMA coefficients.  Two
  # routes to the same function of time, so -- exponentials with distinct rates
  # being linearly independent -- the same coefficients.
  pk <- getDrugPK("oxycodone", 70, 170, 50, "male",
                  getDrugDefaults("oxycodone"))$PK$default
  skip_if(is.null(pk$ka_PO) || pk$ka_PO <= 0, "oxycodone has no oral route")

  got <- effectSiteCoefficients(
    list(lambda = c(pk$lambda_1, pk$lambda_2, pk$lambda_3, pk$ka_PO),
         PO     = c(pk$p_coef_PO_l1, pk$p_coef_PO_l2, pk$p_coef_PO_l3,
                    pk$p_coef_PO_ka)),
    pk$ke0)

  expect_equal(got$PO, c(pk$e_coef_PO_l1, pk$e_coef_PO_l2, pk$e_coef_PO_l3,
                         pk$e_coef_PO_ka, pk$e_coef_PO_ke0),
               tolerance = 1e-9)
})


test_that("recoveryFromStates is recoveryCalc, one point at a time", {
  lambda <- c(0.2, 0.01, 0.05)
  S <- rbind(c(1, 2, -3), c(0.5, 1.8, -0.4), c(0, 0, 0))
  set <- recoveryStateSet(c(0, 10, 20), S, lambda)

  expect_equal(recoveryFromStates(set, 0.5),
               vapply(1:3, function(i) recoveryCalc(S[i, ], lambda, 0.5),
                      numeric(1)))
  # No threshold to fall to means no time to report
  expect_equal(recoveryFromStates(set, NULL), rep(0, 3))
  expect_equal(recoveryFromStates(set, NA_real_), rep(0, 3))
  # A threshold of zero is no threshold, not a day at every point
  expect_equal(recoveryFromStates(set, 0), rep(0, 3))
})


test_that("combinedRecovery concatenates the states rather than the times", {
  # Two contributions, each a sum of exponentials.  The combined effect site is
  # a sum over the union of the eigenvalue sets, which is one call to
  # recoveryCalc -- not a function of the two separate answers.
  a <- recoveryStateSet(c(0, 10, 40), cbind(c(3, 2, 1), c(-1, -0.4, -0.1)),
                        c(0.02, 0.2))
  b <- recoveryStateSet(c(0, 40), cbind(c(0, 1.5)), 0.01)

  times <- c(0, 10, 40)
  got <- combinedRecovery(times, list(a, b), 1.2)

  bOnto <- advanceStatesOnto(b, times)$state
  want <- vapply(seq_along(times), function(i)
    recoveryCalc(c(a$state[i, ], bOnto[i, ]), c(0.02, 0.2, 0.01), 1.2),
    numeric(1))
  expect_equal(got, want)

  # And it is NOT either part's own answer, nor their sum
  expect_false(isTRUE(all.equal(got, recoveryFromStates(a, 1.2))))
})


test_that("splitting one drug's effect site into two sets changes nothing", {
  # The property the whole fold rests on: the states superpose, so it makes no
  # difference whether an effect site arrives as one contribution or as two
  # halves of one.  Checked on a real drug's own states.
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("fentanyl", 70, 170, 50, "male", dd[dd$Drug == "fentanyl", ])
  PK$endCe <- dd$endCe[dd$Drug == "fentanyl"]
  noEvents <- data.frame(Time = numeric(0), Event = character(0))

  whole <- simCpCe(data.frame(Drug = "fentanyl", Time = c(0, 30),
                              Dose = c(200, 100), Units = "mcg"),
                   noEvents, PK, 240, TRUE)
  half  <- simCpCe(data.frame(Drug = "fentanyl", Time = c(0, 30),
                              Dose = c(100, 50), Units = "mcg"),
                   noEvents, PK, 240, TRUE)

  times <- whole$recoveryStates$time
  expect_equal(
    combinedRecovery(times, list(half$recoveryStates, half$recoveryStates),
                     PK$endCe),
    recoveryFromStates(whole$recoveryStates, PK$endCe),
    tolerance = 1e-6
  )
})


test_that("the intravenous engines all carry their states out", {
  dd <- getDrugDefaultsGlobal()
  noEvents <- data.frame(Time = numeric(0), Event = character(0))
  pkFor <- function(drug, ...) {
    PK <- getDrugPK(drug = drug, drugDefaults = dd[dd$Drug == drug, ], ...)
    PK$endCe <- dd$endCe[dd$Drug == drug]
    PK
  }

  # advanceClosedForm0
  PK <- pkFor("propofol", weight = 70, height = 170, age = 50, sex = "male")
  X <- simCpCe(data.frame(Drug = "propofol", Time = 0, Dose = 140, Units = "mg"),
               noEvents, PK, 120, TRUE)
  expect_equal(ncol(X$recoveryStates$state), 4)
  expect_equal(recoveryFromStates(X$recoveryStates, PK$endCe), X$wide$Recovery)

  # advanceClosedFormPO_IM_IN.  Hydromorphone, not oxycodone: oxycodone now
  # forms oxymorphone and so takes the metabolite engine instead.
  PK <- pkFor("hydromorphone", weight = 70, height = 170, age = 50, sex = "male")
  X <- simCpCe(data.frame(Drug = "hydromorphone", Time = 0, Dose = 2,
                          Units = "mg PO"),
               noEvents, PK, 480, TRUE)
  expect_equal(ncol(X$recoveryStates$state), 7)
  expect_equal(recoveryFromStates(X$recoveryStates, PK$endCe), X$wide$Recovery)

  # A drug with a regional anesthesia depot carries one more state, ka_RA.
  PK <- pkFor("lidocaine", weight = 70, height = 170, age = 50, sex = "male")
  X <- simCpCe(data.frame(Drug = "lidocaine", Time = 0, Dose = 300,
                          Units = "mg RA"),
               noEvents, PK, 480, TRUE)
  expect_equal(ncol(X$recoveryStates$state), 8)
  expect_equal(recoveryFromStates(X$recoveryStates, PK$endCe), X$wide$Recovery)

  # So does a drug with a sublingual depot, ka_SL (buprenorphine).
  PK <- pkFor("buprenorphine", weight = 70, height = 170, age = 50, sex = "male")
  X <- simCpCe(data.frame(Drug = "buprenorphine", Time = 0, Dose = 8,
                          Units = "mg SL"),
               noEvents, PK, 480, TRUE)
  expect_equal(ncol(X$recoveryStates$state), 8)
  expect_equal(recoveryFromStates(X$recoveryStates, PK$endCe), X$wide$Recovery)

  # advanceClosedFormMetabolite, for a parent that has an effect site of its
  # own: two sets come out, the parent's and the metabolite's.
  PK <- pkFor("oxycodone", weight = 70, height = 170, age = 50, sex = "male")
  X <- simCpCe(data.frame(Drug = "oxycodone", Time = 0, Dose = 20, Units = "mg PO"),
               noEvents, PK, 720, TRUE)
  expect_equal(ncol(X$recoveryStates$state), 5)      # 3 lambdas, ke0, ka_PO
  expect_equal(recoveryFromStates(X$recoveryStates, PK$endCe), X$wide$Recovery)
  expect_equal(rowSums(X$metaboliteRecoveryStates$state), X$metaboliteSeries$Ce,
               tolerance = 1e-15)

  # And for a pure prodrug, which has no effect site of its own at all.  Its
  # own time until threshold, should it be given one, is timed on its plasma
  # (see "Which concentration is timed" in R/recoveryStates.R), so its states
  # are plasma states and sum to its plasma concentration.
  PK <- pkFor("codeine", weight = 70, height = 171, age = 50, sex = "male")
  X <- simCpCe(data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg PO"),
               noEvents, PK, 720, TRUE)
  expect_equal(rowSums(X$recoveryStates$state), X$wide$Plasma, tolerance = 1e-12)
  expect_false(is.null(X$metaboliteRecoveryStates))
  # The metabolite's states sum to the metabolite's effect site exactly
  expect_equal(rowSums(X$metaboliteRecoveryStates$state), X$metaboliteSeries$Ce,
               tolerance = 1e-15)

  # advanceClosedForm1: time-varying PK, so a lambda per time point
  PK <- pkFor("dexmedetomidine", weight = 7, height = 65, age = 0.5, sex = "male")
  skip_if(length(PK$pkEvents) < 2, "dexmedetomidine has no PK events for this patient")
  X <- simCpCe(data.frame(Drug = "dexmedetomidine", Time = c(0, 0),
                          Dose = c(1, 0.7), Units = c("mcg/kg", "mcg/kg/hr")),
               data.frame(Time = 30, Event = "CPB Start"), PK, 120, TRUE)
  expect_true(is.matrix(X$recoveryStates$lambda))
  expect_equal(dim(X$recoveryStates$lambda), dim(X$recoveryStates$state))
  expect_equal(recoveryFromStates(X$recoveryStates, PK$endCe), X$wide$Recovery)
  # The states are the effect site, which is what the rewrite of this engine's
  # recovery was about
  expect_equal(rowSums(X$recoveryStates$state), X$wide$"Effect Site",
               tolerance = 1e-12)
})


test_that("a metabolite with no effect site carries plasma states", {
  # This branch is reached by hand: a metabolite drug whose potency has not
  # been supplied yet, or which is itself a prodrug.  Its effect-site
  # contribution is NA rather than a copy of its plasma concentration.  Since
  # 2026-10-07 a drug with no effect site is timed on its plasma, so the state
  # set it carries for the fold is the formed PLASMA contribution (until then
  # it carried none).
  PK <- getDrugPK("codeine", 70, 171, 50, "male", getDrugDefaults("codeine"))
  pkSet <- PK$PK$default
  pkSet$metabolite$ke0 <- 0

  dose <- data.frame(Drug = "codeine", Time = 0, Dose = 60, Units = "mg PO",
                     Bolus = FALSE, PO = TRUE, IM = FALSE, IN = FALSE)
  out <- advanceClosedFormMetabolite(dose, pkSet, 720, TRUE, 0.008)

  expect_true(all(is.na(out$CeMetabolite)))
  expect_true(all(out$CpMetabolite >= 0))
  expect_gt(max(out$CpMetabolite), 0)
  formed <- attr(out, "metaboliteRecoveryStates")
  expect_false(is.null(formed))
  expect_equal(pmax(rowSums(formed$state), 0), out$CpMetabolite, tolerance = 1e-9)
})


test_that("a lagged parent dose masks the formed states as well as its own", {
  # A parent dose given but not yet absorbing leaves out the metabolite it is
  # certain to form, so the formed contribution has to carry the mask too, in
  # both of its branches: effect-site states when the metabolite has an effect
  # site (morphine as shipped), plasma states when it has none.  Nothing tested
  # the plasma branch.  Codeine has no lag of its own, so it is given
  # a 20-minute one by hand, and a second dose given while the first is well
  # under way, so that an unmasked answer would be a plausible time rather
  # than zero.  The thresholds are a quarter of each curve's peak.
  # (Claude Code, 2026-10-07, mutation review.)
  PK <- getDrugPK("codeine", 70, 171, 50, "male", getDrugDefaults("codeine"))
  pkSet <- PK$PK$default
  pkSet$tlag_PO <- 20
  dose <- data.frame(Drug = "codeine", Time = c(0, 120), Dose = 60000, Units = "mg PO",
                     Bolus = FALSE, PO = TRUE, IM = FALSE, IN = FALSE)
  for (metKe0 in c(pkSet$metabolite$ke0, 0)) {
    pkSet$metabolite$ke0 <- metKe0
    label <- if (metKe0 > 0) "formed effect site" else "formed plasma"
    base <- advanceClosedFormMetabolite(dose, pkSet, 480, FALSE, 0)
    out  <- advanceClosedFormMetabolite(dose, pkSet, 480, TRUE, max(base$Cp) / 4)
    pending <- out$Time < 20 | (out$Time >= 120 & out$Time < 140)
    expect_true(all(c(0, 20, 120, 140) %in% out$Time))

    # The parent's own time, on its plasma
    expect_true(all(is.na(out$Recovery[pending])), label = paste(label, ": parent masked"))
    expect_false(anyNA(out$Recovery[!pending]))

    # The formed contribution's
    formed <- attr(out, "metaboliteRecoveryStates")
    expect_identical(formed$pending, pending, label = paste(label, "mask"))
    curve <- if (metKe0 > 0) out$CeMetabolite else out$CpMetabolite
    rec <- recoveryFromStates(formed, max(curve) / 4)
    expect_true(all(is.na(rec[pending])), label = paste(label, ": masked"))
    expect_false(anyNA(rec[!pending]))
    expect_gt(rec[max(which(out$Time < 120))], 60)
  }
})
